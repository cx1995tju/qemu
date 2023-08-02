/*
 * QEMU Object Model
 *
 * Copyright IBM, Corp. 2011
 *
 * Authors:
 *  Anthony Liguori   <aliguori@us.ibm.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#include "qemu/osdep.h"
#include "qapi/error.h"
#include "qom/object.h"
#include "qom/object_interfaces.h"
#include "qemu/cutils.h"
#include "qapi/visitor.h"
#include "qapi-visit.h"
#include "qapi/string-input-visitor.h"
#include "qapi/string-output-visitor.h"
#include "qapi/qmp/qerror.h"
#include "trace.h"

/* TODO: replace QObject with a simpler visitor to avoid a dependency
 * of the QOM core on QObject?  */
#include "qom/qom-qobject.h"
#include "qapi/qmp/qobject.h"
#include "qapi/qmp/qbool.h"
#include "qapi/qmp/qint.h"
#include "qapi/qmp/qstring.h"

#define MAX_INTERFACES 32

typedef struct InterfaceImpl InterfaceImpl;
typedef struct TypeImpl TypeImpl;

struct InterfaceImpl
{
    const char *typename;
};

//类型的实现，是全局per 类型唯一的, refer to %type_table_add
struct TypeImpl
{
    const char *name; //类名字, 唯一 id

    size_t class_size; //类的大小

    size_t instance_size; //该类定义的对象的大小

    //类初始化相关的函数，牢记：因为QOM的类相关概念都是运行时构建的，所以这些在c++中编译器做的事情我们必须自己做
    void (*class_init)(ObjectClass *klass, void *data);
    void (*class_base_init)(ObjectClass *klass, void *data);
    void (*class_finalize)(ObjectClass *klass, void *data);

    void *class_data;

    void (*instance_init)(Object *obj);
    void (*instance_post_init)(Object *obj);
    void (*instance_finalize)(Object *obj);

    bool abstract;	//是否是抽象类

    const char *parent;	//父类名字
    TypeImpl *parent_type; //父类是哪一个

    ObjectClass *class; //指向类的一些基本信息, 在类初始化的时候，会去赋值的，如果没有赋值说明类没有初始化
	//这些基本信息，也是按照父类的继承关系组织的，参考linux 的sock结构的层次关系。
	//譬如： PCIDeviceClass
	//可以认为是保存了一些类的metadata，理解为C++中类的静态成员
	//每个类型的实现 都会在这是使用first-memeber inherit的机制，包含所有父类的信息的

    int num_interfaces;
    InterfaceImpl interfaces[MAX_INTERFACES]; //接口类，就是c++中仅仅包含纯虚函数的抽象类, 或者就是java中的接口
};

static Type type_interface;

static GHashTable *type_table_get(void)
{
	//注意是static的
    static GHashTable *type_table;

    if (type_table == NULL) {
        type_table = g_hash_table_new(g_str_hash, g_str_equal);
    }

    return type_table;
}

static bool enumerating_types;

static void type_table_add(TypeImpl *ti)
{
    assert(!enumerating_types);
    g_hash_table_insert(type_table_get(), (void *)ti->name, ti);
}

static TypeImpl *type_table_lookup(const char *name)
{
    return g_hash_table_lookup(type_table_get(), name);
}

//使用TypeInfo构造一个具体的类型, 注意不是构造对象
static TypeImpl *type_new(const TypeInfo *info)
{
    TypeImpl *ti = g_malloc0(sizeof(*ti));
    int i;

    g_assert(info->name != NULL);

    if (type_table_lookup(info->name) != NULL) {
        fprintf(stderr, "Registering `%s' which already exists\n", info->name);
        abort();
    }

    ti->name = g_strdup(info->name);
    ti->parent = g_strdup(info->parent);

    ti->class_size = info->class_size;
    ti->instance_size = info->instance_size;

    ti->class_init = info->class_init;
    ti->class_base_init = info->class_base_init;
    ti->class_finalize = info->class_finalize;
    ti->class_data = info->class_data;

    ti->instance_init = info->instance_init;
    ti->instance_post_init = info->instance_post_init;
    ti->instance_finalize = info->instance_finalize;

    ti->abstract = info->abstract;

    for (i = 0; info->interfaces && info->interfaces[i].type; i++) {
        ti->interfaces[i].typename = g_strdup(info->interfaces[i].type);
    }
    ti->num_interfaces = i;

    return ti;
}

static TypeImpl *type_register_internal(const TypeInfo *info)
{
    TypeImpl *ti;
    ti = type_new(info);

    type_table_add(ti);
    return ti;
}

TypeImpl *type_register(const TypeInfo *info)
{
    assert(info->parent);
    return type_register_internal(info);
}

TypeImpl *type_register_static(const TypeInfo *info)
{
    return type_register(info);
}

static TypeImpl *type_get_by_name(const char *name)
{
    if (name == NULL) {
        return NULL;
    }

    return type_table_lookup(name);
}

static TypeImpl *type_get_parent(TypeImpl *type)
{
    if (!type->parent_type && type->parent) {
        type->parent_type = type_get_by_name(type->parent);
        g_assert(type->parent_type != NULL);
    }

    return type->parent_type;
}

static bool type_has_parent(TypeImpl *type)
{
    return (type->parent != NULL);
}

static size_t type_class_get_size(TypeImpl *ti)
{
    if (ti->class_size) {
        return ti->class_size; // 一个类型有的话就直接用
    }

    if (type_has_parent(ti)) { // 没有的话，就使用其父亲的 class_size, 说明这个类型没有一些 class specific 的信息需要保存
        return type_class_get_size(type_get_parent(ti));
    }

    return sizeof(ObjectClass);
}

static size_t type_object_get_size(TypeImpl *ti)
{
    if (ti->instance_size) {
        return ti->instance_size;
    }

    if (type_has_parent(ti)) {
        return type_object_get_size(type_get_parent(ti));
    }

    return 0;
}

size_t object_type_get_instance_size(const char *typename)
{
    TypeImpl *type = type_get_by_name(typename);

    g_assert(type != NULL);
    return type_object_get_size(type);
}

static bool type_is_ancestor(TypeImpl *type, TypeImpl *target_type)
{
    assert(target_type);

    /* Check if target_type is a direct ancestor of type */
    while (type) {
        if (type == target_type) {
            return true;
        }

        type = type_get_parent(type);
    }

    return false;
}

static void type_initialize(TypeImpl *ti);

static void type_initialize_interface(TypeImpl *ti, TypeImpl *interface_type,
                                      TypeImpl *parent_type)
{
    InterfaceClass *new_iface;
    TypeInfo info = { };
    TypeImpl *iface_impl;

    info.parent = parent_type->name;
    info.name = g_strdup_printf("%s::%s", ti->name, interface_type->name);
    info.abstract = true;

    iface_impl = type_new(&info);
    iface_impl->parent_type = parent_type;
    type_initialize(iface_impl);
    g_free((char *)info.name);

    new_iface = (InterfaceClass *)iface_impl->class;
    new_iface->concrete_class = ti->class;
    new_iface->interface_type = interface_type;

    ti->class->interfaces = g_slist_append(ti->class->interfaces,
                                           iface_impl->class);
}

static void object_property_free(gpointer data)
{
    ObjectProperty *prop = data;

    g_free(prop->name);
    g_free(prop->type);
    g_free(prop->description);
    g_free(prop);
}

static void type_initialize(TypeImpl *ti)
{
    TypeImpl *parent;

    if (ti->class) {
        return;
    }

    ti->class_size = type_class_get_size(ti);
    ti->instance_size = type_object_get_size(ti); //如果没有设置instance_size, 那么就等于父亲的instance size

    ti->class = g_malloc0(ti->class_size);

    parent = type_get_parent(ti);
    if (parent) {
        type_initialize(parent); //递归方式，初始化所有父类类型
        GSList *e;
        int i;

        g_assert_cmpint(parent->class_size, <=, ti->class_size); //父亲类型，不应该大于子类型的
        memcpy(ti->class, parent->class, parent->class_size); //层次化来初始化所有Type的class成员。 妙妙妙。 一层一层的将祖先的class信息copy下来
        ti->class->interfaces = NULL;
        ti->class->properties = g_hash_table_new_full( // 创建了保存类属性的 hash 表
            g_str_hash, g_str_equal, g_free, object_property_free);

        for (e = parent->class->interfaces; e; e = e->next) {
            InterfaceClass *iface = e->data;
            ObjectClass *klass = OBJECT_CLASS(iface);

            type_initialize_interface(ti, iface->interface_type, klass->type);
        }

        for (i = 0; i < ti->num_interfaces; i++) {
            TypeImpl *t = type_get_by_name(ti->interfaces[i].typename);
            for (e = ti->class->interfaces; e; e = e->next) {
                TypeImpl *target_type = OBJECT_CLASS(e->data)->type;

                if (type_is_ancestor(target_type, t)) {
                    break;
                }
            }

            if (e) {
                continue;
            }

            type_initialize_interface(ti, t, t);
        }
    } else {
        ti->class->properties = g_hash_table_new_full(
            g_str_hash, g_str_equal, g_free, object_property_free);
    }

    ti->class->type = ti;

    //注意：前面是递归的，所以所有类的class_init class_base_init函数按照先父类后子类的顺序都会被调用的
    //另外，注意这个函数刚进来的初始化条件，如果一个类被的init函数被调用过了就不会被调用的。
    while (parent) {
        if (parent->class_base_init) {
            parent->class_base_init(ti->class, ti->class_data);
        }
        parent = type_get_parent(parent);
    }

    if (ti->class_init) {
        ti->class_init(ti->class, ti->class_data); //类型初始化，本质也就是1.分配class结构（从祖先到后代的顺序）2. 执行一个class_init函数(从祖先到父亲的顺序)了并记录下是否初始化的信息
    }
}

static void object_init_with_type(Object *obj, TypeImpl *ti)
{
	//构造的过程是先构造祖先类，再构造子类的。这里就是递归实现的
	//最终上溯到终极的type object
    if (type_has_parent(ti)) {
        object_init_with_type(obj, type_get_parent(ti));
    }

//这里递归调用都是一个指针参数obj，但是却能一直调用到父类，显然这个obj是类似与linux sock的那种层次继承关系，我称作first-member-inherit
    if (ti->instance_init) {
        ti->instance_init(obj);
    }
}

static void object_post_init_with_type(Object *obj, TypeImpl *ti)
{
    if (ti->instance_post_init) {
        ti->instance_post_init(obj);
    }

    if (type_has_parent(ti)) {
        object_post_init_with_type(obj, type_get_parent(ti));
    }
}

void object_initialize_with_type(void *data, size_t size, TypeImpl *type)
{
    Object *obj = data;

    g_assert(type != NULL);
    type_initialize(type);

    g_assert_cmpint(type->instance_size, >=, sizeof(Object));
    g_assert(type->abstract == false);
    g_assert_cmpint(size, >=, type->instance_size);

    memset(obj, 0, type->instance_size);
    obj->class = type->class;
    object_ref(obj);
    obj->properties = g_hash_table_new_full(g_str_hash, g_str_equal, //搞一个hash table来保存属性咯
                                            NULL, object_property_free);
    object_init_with_type(obj, type);
    object_post_init_with_type(obj, type);
}

//构造函数
void object_initialize(void *data, size_t size, const char *typename)
{
    TypeImpl *type = type_get_by_name(typename);

    object_initialize_with_type(data, size, type);
}

static inline bool object_property_is_child(ObjectProperty *prop)
{
    return strstart(prop->type, "child<", NULL);
}

static void object_property_del_all(Object *obj)
{
    ObjectProperty *prop;
    GHashTableIter iter;
    gpointer key, value;
    bool released;

    do {
        released = false;
        g_hash_table_iter_init(&iter, obj->properties);
        while (g_hash_table_iter_next(&iter, &key, &value)) {
            prop = value;
            if (prop->release) {
                prop->release(obj, prop->name, prop->opaque);
                prop->release = NULL;
                released = true;
                break;
            }
            g_hash_table_iter_remove(&iter);
        }
    } while (released);

    g_hash_table_unref(obj->properties);
}

static void object_property_del_child(Object *obj, Object *child, Error **errp)
{
    ObjectProperty *prop;
    GHashTableIter iter;
    gpointer key, value;

    g_hash_table_iter_init(&iter, obj->properties);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        prop = value;
        if (object_property_is_child(prop) && prop->opaque == child) {
            if (prop->release) {
                prop->release(obj, prop->name, prop->opaque);
                prop->release = NULL;
            }
            break;
        }
    }
    g_hash_table_iter_init(&iter, obj->properties);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        prop = value;
        if (object_property_is_child(prop) && prop->opaque == child) {
            g_hash_table_iter_remove(&iter);
            break;
        }
    }
}

void object_unparent(Object *obj)
{
    if (obj->parent) {
        object_property_del_child(obj->parent, obj, NULL);
    }
}

static void object_deinit(Object *obj, TypeImpl *type)
{
    if (type->instance_finalize) {
        type->instance_finalize(obj);
    }

    if (type_has_parent(type)) {
        object_deinit(obj, type_get_parent(type));
    }
}

static void object_finalize(void *data)
{
    Object *obj = data;
    TypeImpl *ti = obj->class->type;

    object_property_del_all(obj);
    object_deinit(obj, ti);

    g_assert_cmpint(obj->ref, ==, 0);
    if (obj->free) {
        obj->free(obj);
    }
}

Object *object_new_with_type(Type type)
{
    Object *obj;

    g_assert(type != NULL);
    type_initialize(type);

    obj = g_malloc(type->instance_size);
    object_initialize_with_type(obj, type->instance_size, type);
    obj->free = g_free;

    return obj;
}

Object *object_new(const char *typename)
{
    TypeImpl *ti = type_get_by_name(typename);

    return object_new_with_type(ti);
}


Object *object_new_with_props(const char *typename,
                              Object *parent,
                              const char *id,
                              Error **errp,
                              ...)
{
    va_list vargs;
    Object *obj;

    va_start(vargs, errp);
    obj = object_new_with_propv(typename, parent, id, errp, vargs);
    va_end(vargs);

    return obj;
}


Object *object_new_with_propv(const char *typename,
                              Object *parent,
                              const char *id,
                              Error **errp,
                              va_list vargs)
{
    Object *obj;
    ObjectClass *klass;
    Error *local_err = NULL;

    klass = object_class_by_name(typename);
    if (!klass) {
        error_setg(errp, "invalid object type: %s", typename);
        return NULL;
    }

    if (object_class_is_abstract(klass)) {
        error_setg(errp, "object type '%s' is abstract", typename);
        return NULL;
    }
    obj = object_new(typename);

    if (object_set_propv(obj, &local_err, vargs) < 0) {
        goto error;
    }

    object_property_add_child(parent, id, obj, &local_err);
    if (local_err) {
        goto error;
    }

    if (object_dynamic_cast(obj, TYPE_USER_CREATABLE)) {
        user_creatable_complete(obj, &local_err);
        if (local_err) {
            object_unparent(obj);
            goto error;
        }
    }

    object_unref(OBJECT(obj));
    return obj;

 error:
    error_propagate(errp, local_err);
    object_unref(obj);
    return NULL;
}


int object_set_props(Object *obj,
                     Error **errp,
                     ...)
{
    va_list vargs;
    int ret;

    va_start(vargs, errp);
    ret = object_set_propv(obj, errp, vargs);
    va_end(vargs);

    return ret;
}


int object_set_propv(Object *obj,
                     Error **errp,
                     va_list vargs)
{
    const char *propname;
    Error *local_err = NULL;

    propname = va_arg(vargs, char *);
    while (propname != NULL) {
        const char *value = va_arg(vargs, char *);

        g_assert(value != NULL);
        object_property_parse(obj, value, propname, &local_err);
        if (local_err) {
            error_propagate(errp, local_err);
            return -1;
        }
        propname = va_arg(vargs, char *);
    }

    return 0;
}


Object *object_dynamic_cast(Object *obj, const char *typename)
{
    if (obj && object_class_dynamic_cast(object_get_class(obj), typename)) {
        return obj;
    }

    return NULL;
}

//动态cast
Object *object_dynamic_cast_assert(Object *obj, const char *typename,
                                   const char *file, int line, const char *func)
{
    trace_object_dynamic_cast_assert(obj ? obj->class->type->name : "(null)",
                                     typename, file, line, func);

#ifdef CONFIG_QOM_CAST_DEBUG
    int i;
    Object *inst;

    for (i = 0; obj && i < OBJECT_CLASS_CAST_CACHE; i++) {
        if (atomic_read(&obj->class->object_cast_cache[i]) == typename) {
            goto out;
        }
    }

    inst = object_dynamic_cast(obj, typename);

    if (!inst && obj) {
        fprintf(stderr, "%s:%d:%s: Object %p is not an instance of type %s\n",
                file, line, func, obj, typename);
        abort();
    }

    assert(obj == inst);

    if (obj && obj == inst) {
        for (i = 1; i < OBJECT_CLASS_CAST_CACHE; i++) {
            atomic_set(&obj->class->object_cast_cache[i - 1],
                       atomic_read(&obj->class->object_cast_cache[i]));
        }
        atomic_set(&obj->class->object_cast_cache[i - 1], typename);
    }

out:
#endif
    return obj;
}

//只有是后代能够强行转换为祖先，这里有多态的意思了
//不同于c++是编译期去检查，这里在运行期间去检查明显影响效率呀
ObjectClass *object_class_dynamic_cast(ObjectClass *class,
                                       const char *typename)
{
    ObjectClass *ret = NULL;
    TypeImpl *target_type;
    TypeImpl *type;

    if (!class) {
        return NULL;
    }

    /* A simple fast path that can trigger a lot for leaf classes.  */
    type = class->type;
    if (type->name == typename) {
        return class;
    }

    target_type = type_get_by_name(typename);
    if (!target_type) {
        /* target class type unknown, so fail the cast */
        return NULL;
    }

    if (type->class->interfaces &&
            type_is_ancestor(target_type, type_interface)) {
        int found = 0;
        GSList *i;

        for (i = class->interfaces; i; i = i->next) {
            ObjectClass *target_class = i->data;

            if (type_is_ancestor(target_class->type, target_type)) {
                ret = target_class;
                found++;
            }
         }

        /* The match was ambiguous, don't allow a cast */
        if (found > 1) {
            ret = NULL;
        }
    } else if (type_is_ancestor(type, target_type)) {
        ret = class;
    }

    return ret;
}

ObjectClass *object_class_dynamic_cast_assert(ObjectClass *class,
                                              const char *typename,
                                              const char *file, int line,
                                              const char *func)
{
    ObjectClass *ret;

    trace_object_class_dynamic_cast_assert(class ? class->type->name : "(null)",
                                           typename, file, line, func);

#ifdef CONFIG_QOM_CAST_DEBUG
    int i;

    for (i = 0; class && i < OBJECT_CLASS_CAST_CACHE; i++) {
        if (atomic_read(&class->class_cast_cache[i]) == typename) {
            ret = class;
            goto out;
        }
    }
#else
    if (!class || !class->interfaces) {
        return class;
    }
#endif

    ret = object_class_dynamic_cast(class, typename);
    if (!ret && class) {
        fprintf(stderr, "%s:%d:%s: Object %p is not an instance of type %s\n",
                file, line, func, class, typename);
        abort();
    }

#ifdef CONFIG_QOM_CAST_DEBUG
    if (class && ret == class) {
        for (i = 1; i < OBJECT_CLASS_CAST_CACHE; i++) {
            atomic_set(&class->class_cast_cache[i - 1],
                       atomic_read(&class->class_cast_cache[i]));
        }
        atomic_set(&class->class_cast_cache[i - 1], typename);
    }
out:
#endif
    return ret;
}

const char *object_get_typename(Object *obj)
{
    return obj->class->type->name;
}

ObjectClass *object_get_class(Object *obj)
{
    return obj->class;
}

bool object_class_is_abstract(ObjectClass *klass)
{
    return klass->type->abstract;
}

const char *object_class_get_name(ObjectClass *klass)
{
    return klass->type->name;
}

ObjectClass *object_class_by_name(const char *typename)
{
    TypeImpl *type = type_get_by_name(typename);

    if (!type) {
        return NULL;
    }

    type_initialize(type);

    return type->class;
}

ObjectClass *object_class_get_parent(ObjectClass *class)
{
    TypeImpl *type = type_get_parent(class->type);

    if (!type) {
        return NULL;
    }

    type_initialize(type);

    return type->class;
}

typedef struct OCFData
{
    void (*fn)(ObjectClass *klass, void *opaque);
    const char *implements_type;
    bool include_abstract;
    void *opaque;
} OCFData;

static void object_class_foreach_tramp(gpointer key, gpointer value,
                                       gpointer opaque)
{
    OCFData *data = opaque;
    TypeImpl *type = value;
    ObjectClass *k;

    type_initialize(type);
    k = type->class;

    if (!data->include_abstract && type->abstract) {
        return;
    }

    if (data->implements_type && 
        !object_class_dynamic_cast(k, data->implements_type)) {
        return;
    }

    data->fn(k, data->opaque);
}

void object_class_foreach(void (*fn)(ObjectClass *klass, void *opaque),
                          const char *implements_type, bool include_abstract,
                          void *opaque)
{
    OCFData data = { fn, implements_type, include_abstract, opaque };

    enumerating_types = true;
    g_hash_table_foreach(type_table_get(), object_class_foreach_tramp, &data);
    enumerating_types = false;
}

static int do_object_child_foreach(Object *obj,
                                   int (*fn)(Object *child, void *opaque),
                                   void *opaque, bool recurse)
{
    GHashTableIter iter;
    ObjectProperty *prop;
    int ret = 0;

    g_hash_table_iter_init(&iter, obj->properties);
    while (g_hash_table_iter_next(&iter, NULL, (gpointer *)&prop)) {
        if (object_property_is_child(prop)) {
            Object *child = prop->opaque;

            ret = fn(child, opaque);
            if (ret != 0) {
                break;
            }
            if (recurse) {
                do_object_child_foreach(child, fn, opaque, true);
            }
        }
    }
    return ret;
}

int object_child_foreach(Object *obj, int (*fn)(Object *child, void *opaque),
                         void *opaque)
{
    return do_object_child_foreach(obj, fn, opaque, false);
}

int object_child_foreach_recursive(Object *obj,
                                   int (*fn)(Object *child, void *opaque),
                                   void *opaque)
{
    return do_object_child_foreach(obj, fn, opaque, true);
}

static void object_class_get_list_tramp(ObjectClass *klass, void *opaque)
{
    GSList **list = opaque;

    *list = g_slist_prepend(*list, klass);
}

GSList *object_class_get_list(const char *implements_type,
                              bool include_abstract)
{
    GSList *list = NULL;

    object_class_foreach(object_class_get_list_tramp,
                         implements_type, include_abstract, &list);
    return list;
}

void object_ref(Object *obj)
{
    if (!obj) {
        return;
    }
    atomic_inc(&obj->ref);
}

void object_unref(Object *obj)
{
    if (!obj) {
        return;
    }
    g_assert_cmpint(obj->ref, >, 0);

    /* parent always holds a reference to its children */
    if (atomic_fetch_dec(&obj->ref) == 1) {
        object_finalize(obj);
    }
}

//为对象obj添加一个名为name，类型是type， 对应的set get release函数的属性
//本质就是在对象的properties hash 表上插入了一个属性结构ObjectProperty
//这个opaque后续会作为参数传递给get set函数的
// 给 obj 对象，增加一个 名字为 name，类型为 type，操作函数为 get/set/release，操作对象是 opaque 的属性
ObjectProperty *
object_property_add(Object *obj, const char *name, const char *type,
                    ObjectPropertyAccessor *get,
                    ObjectPropertyAccessor *set,
                    ObjectPropertyRelease *release,
                    void *opaque, Error **errp)
{
    ObjectProperty *prop;
    size_t name_len = strlen(name);

    //name len太长要处理下
    if (name_len >= 3 && !memcmp(name + name_len - 3, "[*]", 4)) {
        int i;
        ObjectProperty *ret;
        char *name_no_array = g_strdup(name);

        name_no_array[name_len - 3] = '\0';
        for (i = 0; ; ++i) {
            char *full_name = g_strdup_printf("%s[%d]", name_no_array, i);

            ret = object_property_add(obj, full_name, type, get, set,
                                      release, opaque, NULL);
            g_free(full_name);
            if (ret) {
                break;
            }
        }
        g_free(name_no_array);
        return ret;
    }

    if (object_property_find(obj, name, NULL) != NULL) {
        error_setg(errp, "attempt to add duplicate property '%s'"
                   " to object (type '%s')", name,
                   object_get_typename(obj));
        return NULL;
    }

    prop = g_malloc0(sizeof(*prop));

    prop->name = g_strdup(name);
    prop->type = g_strdup(type);

    prop->get = get;
    prop->set = set;
    prop->release = release;
    prop->opaque = opaque; //关键哟

    g_hash_table_insert(obj->properties, prop->name, prop); //关键哟, 将property插入到了object上
    return prop;
}

ObjectProperty *
object_class_property_add(ObjectClass *klass,
                          const char *name,
                          const char *type,
                          ObjectPropertyAccessor *get,
                          ObjectPropertyAccessor *set,
                          ObjectPropertyRelease *release,
                          void *opaque,
                          Error **errp)
{
    ObjectProperty *prop;

    if (object_class_property_find(klass, name, NULL) != NULL) {
        error_setg(errp, "attempt to add duplicate property '%s'"
                   " to object (type '%s')", name,
                   object_class_get_name(klass));
        return NULL;
    }

    prop = g_malloc0(sizeof(*prop));

    prop->name = g_strdup(name);
    prop->type = g_strdup(type);

    prop->get = get;
    prop->set = set;
    prop->release = release;
    prop->opaque = opaque;

    g_hash_table_insert(klass->properties, g_strdup(name), prop);

    return prop;
}

ObjectProperty *object_property_find(Object *obj, const char *name,
                                     Error **errp)
{
    ObjectProperty *prop;
    ObjectClass *klass = object_get_class(obj);

    prop = object_class_property_find(klass, name, NULL);
    if (prop) {
        return prop;
    }

    prop = g_hash_table_lookup(obj->properties, name);
    if (prop) {
        return prop;
    }

    error_setg(errp, "Property '.%s' not found", name);
    return NULL;
}

void object_property_iter_init(ObjectPropertyIterator *iter,
                               Object *obj)
{
    g_hash_table_iter_init(&iter->iter, obj->properties);
    iter->nextclass = object_get_class(obj);
}

ObjectProperty *object_property_iter_next(ObjectPropertyIterator *iter)
{
    gpointer key, val;
    while (!g_hash_table_iter_next(&iter->iter, &key, &val)) {
        if (!iter->nextclass) {
            return NULL;
        }
        g_hash_table_iter_init(&iter->iter, iter->nextclass->properties);
        iter->nextclass = object_class_get_parent(iter->nextclass);
    }
    return val;
}

ObjectProperty *object_class_property_find(ObjectClass *klass, const char *name,
                                           Error **errp)
{
    ObjectProperty *prop;
    ObjectClass *parent_klass;

    parent_klass = object_class_get_parent(klass);
    if (parent_klass) {
        prop = object_class_property_find(parent_klass, name, NULL);
        if (prop) {
            return prop;
        }
    }

    prop = g_hash_table_lookup(klass->properties, name);
    if (!prop) {
        error_setg(errp, "Property '.%s' not found", name);
    }
    return prop;
}

void object_property_del(Object *obj, const char *name, Error **errp)
{
    ObjectProperty *prop = g_hash_table_lookup(obj->properties, name);

    if (!prop) {
        error_setg(errp, "Property '.%s' not found", name);
        return;
    }

    if (prop->release) {
        prop->release(obj, name, prop->opaque);
    }
    g_hash_table_remove(obj->properties, name);
}

void object_property_get(Object *obj, Visitor *v, const char *name,
                         Error **errp)
{
    ObjectProperty *prop = object_property_find(obj, name, errp);
    if (prop == NULL) {
        return;
    }

    if (!prop->get) {
        error_setg(errp, QERR_PERMISSION_DENIED);
    } else {
        prop->get(obj, v, name, prop->opaque, errp);
    }
}

void object_property_set(Object *obj, Visitor *v, const char *name,
                         Error **errp)
{
    ObjectProperty *prop = object_property_find(obj, name, errp);
    if (prop == NULL) {
        return;
    }

    if (!prop->set) {
        error_setg(errp, QERR_PERMISSION_DENIED);
    } else {
        prop->set(obj, v, name, prop->opaque, errp);
    }
}

void object_property_set_str(Object *obj, const char *value,
                             const char *name, Error **errp)
{
    QString *qstr = qstring_from_str(value);
    object_property_set_qobject(obj, QOBJECT(qstr), name, errp);

    QDECREF(qstr);
}

char *object_property_get_str(Object *obj, const char *name,
                              Error **errp)
{
    QObject *ret = object_property_get_qobject(obj, name, errp);
    QString *qstring;
    char *retval;

    if (!ret) {
        return NULL;
    }
    qstring = qobject_to_qstring(ret);
    if (!qstring) {
        error_setg(errp, QERR_INVALID_PARAMETER_TYPE, name, "string");
        retval = NULL;
    } else {
        retval = g_strdup(qstring_get_str(qstring));
    }

    QDECREF(qstring);
    return retval;
}

void object_property_set_link(Object *obj, Object *value,
                              const char *name, Error **errp)
{
    if (value) {
        gchar *path = object_get_canonical_path(value);
        object_property_set_str(obj, path, name, errp);
        g_free(path);
    } else {
        object_property_set_str(obj, "", name, errp);
    }
}

Object *object_property_get_link(Object *obj, const char *name,
                                 Error **errp)
{
    char *str = object_property_get_str(obj, name, errp);
    Object *target = NULL;

    if (str && *str) {
        target = object_resolve_path(str, NULL);
        if (!target) {
            error_set(errp, ERROR_CLASS_DEVICE_NOT_FOUND,
                      "Device '%s' not found", str);
        }
    }

    g_free(str);
    return target;
}

void object_property_set_bool(Object *obj, bool value,
                              const char *name, Error **errp)
{
    QBool *qbool = qbool_from_bool(value);
    object_property_set_qobject(obj, QOBJECT(qbool), name, errp);

    QDECREF(qbool);
}

bool object_property_get_bool(Object *obj, const char *name,
                              Error **errp)
{
    QObject *ret = object_property_get_qobject(obj, name, errp);
    QBool *qbool;
    bool retval;

    if (!ret) {
        return false;
    }
    qbool = qobject_to_qbool(ret);
    if (!qbool) {
        error_setg(errp, QERR_INVALID_PARAMETER_TYPE, name, "boolean");
        retval = false;
    } else {
        retval = qbool_get_bool(qbool);
    }

    QDECREF(qbool);
    return retval;
}

void object_property_set_int(Object *obj, int64_t value,
                             const char *name, Error **errp)
{
    QInt *qint = qint_from_int(value);
    object_property_set_qobject(obj, QOBJECT(qint), name, errp);

    QDECREF(qint);
}

int64_t object_property_get_int(Object *obj, const char *name,
                                Error **errp)
{
    QObject *ret = object_property_get_qobject(obj, name, errp);
    QInt *qint;
    int64_t retval;

    if (!ret) {
        return -1;
    }
    qint = qobject_to_qint(ret);
    if (!qint) {
        error_setg(errp, QERR_INVALID_PARAMETER_TYPE, name, "int");
        retval = -1;
    } else {
        retval = qint_get_int(qint);
    }

    QDECREF(qint);
    return retval;
}

typedef struct EnumProperty {
    const char * const *strings;
    int (*get)(Object *, Error **);
    void (*set)(Object *, int, Error **);
} EnumProperty;

int object_property_get_enum(Object *obj, const char *name,
                             const char *typename, Error **errp)
{
    Error *err = NULL;
    Visitor *v;
    char *str;
    int ret;
    ObjectProperty *prop = object_property_find(obj, name, errp);
    EnumProperty *enumprop;

    if (prop == NULL) {
        return 0;
    }

    if (!g_str_equal(prop->type, typename)) {
        error_setg(errp, "Property %s on %s is not '%s' enum type",
                   name, object_class_get_name(
                       object_get_class(obj)), typename);
        return 0;
    }

    enumprop = prop->opaque;

    v = string_output_visitor_new(false, &str);
    object_property_get(obj, v, name, &err);
    if (err) {
        error_propagate(errp, err);
        visit_free(v);
        return 0;
    }
    visit_complete(v, &str);
    visit_free(v);
    v = string_input_visitor_new(str);
    visit_type_enum(v, name, &ret, enumprop->strings, errp);

    g_free(str);
    visit_free(v);

    return ret;
}

void object_property_get_uint16List(Object *obj, const char *name,
                                    uint16List **list, Error **errp)
{
    Error *err = NULL;
    Visitor *v;
    char *str;

    v = string_output_visitor_new(false, &str);
    object_property_get(obj, v, name, &err);
    if (err) {
        error_propagate(errp, err);
        goto out;
    }
    visit_complete(v, &str);
    visit_free(v);
    v = string_input_visitor_new(str);
    visit_type_uint16List(v, NULL, list, errp);

    g_free(str);
out:
    visit_free(v);
}

void object_property_parse(Object *obj, const char *string,
                           const char *name, Error **errp)
{
    Visitor *v = string_input_visitor_new(string);
    object_property_set(obj, v, name, errp);
    visit_free(v);
}

char *object_property_print(Object *obj, const char *name, bool human,
                            Error **errp)
{
    Visitor *v;
    char *string = NULL;
    Error *local_err = NULL;

    v = string_output_visitor_new(human, &string);
    object_property_get(obj, v, name, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        goto out;
    }

    visit_complete(v, &string);

out:
    visit_free(v);
    return string;
}

const char *object_property_get_type(Object *obj, const char *name, Error **errp)
{
    ObjectProperty *prop = object_property_find(obj, name, errp);
    if (prop == NULL) {
        return NULL;
    }

    return prop->type;
}

Object *object_get_root(void)
{
    static Object *root;

    if (!root) {
        root = object_new("container");
    }

    return root;
}

Object *object_get_objects_root(void)
{
    return container_get(object_get_root(), "/objects");
}

static void object_get_child_property(Object *obj, Visitor *v,
                                      const char *name, void *opaque,
                                      Error **errp)
{
    Object *child = opaque;
    gchar *path;

    path = object_get_canonical_path(child);
    visit_type_str(v, name, &path, errp);
    g_free(path);
}

static Object *object_resolve_child_property(Object *parent, void *opaque, const gchar *part)
{
    return opaque;
}

static void object_finalize_child_property(Object *obj, const char *name,
                                           void *opaque)
{
    Object *child = opaque;

    if (child->class->unparent) {
        (child->class->unparent)(child);
    }
    child->parent = NULL;
    object_unref(child);
}

//必须强调，这里指的是对象的父子关系，与类的父子关系不是一回事。
//譬如：一个 bus 可以有很多 child device
void object_property_add_child(Object *obj, const char *name,
                               Object *child, Error **errp)
{
    Error *local_err = NULL;
    gchar *type;
    ObjectProperty *op;

    if (child->parent != NULL) {
        error_setg(errp, "child object is already parented");
        return;
    }

    type = g_strdup_printf("child<%s>", object_get_typename(OBJECT(child)));

    op = object_property_add(obj, name, type, object_get_child_property, NULL,
                             object_finalize_child_property, child, &local_err); //child属性比较有意思，这里将child作为opaque传入进去了, 那么内部的ObjectProperty的opaque就会指向child，进而表达了父子关系。由于父子关系是1对多的，我们可以添加多个child属性的，最终就是体现在object的properties链上有多个child property
    if (local_err) {
        error_propagate(errp, local_err);
        goto out;
    }

    op->resolve = object_resolve_child_property;
    object_ref(child);
    child->parent = obj; //这里又建立了child指向parent的关系

out:
    g_free(type);
}

void object_property_allow_set_link(Object *obj, const char *name,
                                    Object *val, Error **errp)
{
    /* Allow the link to be set, always */
}

typedef struct {
    Object **child; //注意这里是二级指针
    void (*check)(Object *, const char *, Object *, Error **);
    ObjectPropertyLinkFlags flags;
} LinkProperty;

static void object_get_link_property(Object *obj, Visitor *v,
                                     const char *name, void *opaque,
                                     Error **errp)
{
    LinkProperty *lprop = opaque;
    Object **child = lprop->child;
    gchar *path;

    if (*child) {
        path = object_get_canonical_path(*child);
        visit_type_str(v, name, &path, errp);
        g_free(path);
    } else {
        path = (gchar *)"";
        visit_type_str(v, name, &path, errp);
    }
}

/*
 * object_resolve_link:
 *
 * Lookup an object and ensure its type matches the link property type.  This
 * is similar to object_resolve_path() except type verification against the
 * link property is performed.
 *
 * Returns: The matched object or NULL on path lookup failures.
 */
static Object *object_resolve_link(Object *obj, const char *name,
                                   const char *path, Error **errp)
{
    const char *type;
    gchar *target_type;
    bool ambiguous = false;
    Object *target;

    /* Go from link<FOO> to FOO.  */
    type = object_property_get_type(obj, name, NULL);
    target_type = g_strndup(&type[5], strlen(type) - 6);
    target = object_resolve_path_type(path, target_type, &ambiguous);

    if (ambiguous) {
        error_setg(errp, "Path '%s' does not uniquely identify an object",
                   path);
    } else if (!target) {
        target = object_resolve_path(path, &ambiguous);
        if (target || ambiguous) {
            error_setg(errp, QERR_INVALID_PARAMETER_TYPE, name, target_type);
        } else {
            error_set(errp, ERROR_CLASS_DEVICE_NOT_FOUND,
                      "Device '%s' not found", path);
        }
        target = NULL;
    }
    g_free(target_type);

    return target;
}

static void object_set_link_property(Object *obj, Visitor *v,
                                     const char *name, void *opaque,
                                     Error **errp)
{
    Error *local_err = NULL;
    LinkProperty *prop = opaque;
    Object **child = prop->child;
    Object *old_target = *child;
    Object *new_target = NULL;
    char *path = NULL;

    visit_type_str(v, name, &path, &local_err);

    if (!local_err && strcmp(path, "") != 0) {
        new_target = object_resolve_link(obj, name, path, &local_err);
    }

    g_free(path);
    if (local_err) {
        error_propagate(errp, local_err);
        return;
    }

    prop->check(obj, name, new_target, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        return;
    }

    object_ref(new_target);
    *child = new_target; //最关键的地方，建立了两个对象的关系。注意child 是二级指针。这里做了解引用，即设置的是一级指针。 而不是 child = new_target。即 child 的指向的位置还是不变的。但是 child 指向的指针的值变化了
    object_unref(old_target);
}

static Object *object_resolve_link_property(Object *parent, void *opaque, const gchar *part)
{
    LinkProperty *lprop = opaque;

    return *lprop->child;
}

static void object_release_link_property(Object *obj, const char *name,
                                         void *opaque)
{
    LinkProperty *prop = opaque;

    if ((prop->flags & OBJ_PROP_LINK_UNREF_ON_RELEASE) && *prop->child) {
        object_unref(*prop->child);
    }
    g_free(prop);
}

//child指向的指针，一般是obj的一个成员
//表示obj的这个成员引用(link)了某个对象, 具体link的对象在object_set_link_property中设置, 可以动态切换的
//不严谨的说：添加的link属性表明：obj的*child成员指针会指向一个对象，该对象被obj引用。对象的设置是动态切换的
//关于link属性
// 1. 首先调用该函数为一个object添加一个link属性
// 2. object_set_link_property设置该link属性
//
// 给对象obj添加一个类型是link<type>, 名为name的link属性
//  get 函数: object_get_link_property
//  set 函数：objecty_set_link_property
void object_property_add_link(Object *obj, const char *name,
                              const char *type, Object **child, //注意，这里是一个child的二级指针,  **这个child一般都是obj的一个指针成员的指针**
                              void (*check)(Object *, const char *,
                                            Object *, Error **), //判断是否允许set link属性
                              ObjectPropertyLinkFlags flags,
                              Error **errp)
{
    Error *local_err = NULL;
    LinkProperty *prop = g_malloc(sizeof(*prop));
    gchar *full_type;
    ObjectProperty *op;

    prop->child = child;
    prop->check = check;
    prop->flags = flags;

    full_type = g_strdup_printf("link<%s>", type); //构建属性类型link<type>

    op = object_property_add(obj, name, full_type,
                             object_get_link_property,
                             check ? object_set_link_property : NULL,
                             object_release_link_property,
                             prop, //link属性非常有意思，将prop作为opaque传入进去了
                             &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
        goto out;
    }

    op->resolve = object_resolve_link_property;

out:
    g_free(full_type);
}

void object_property_add_const_link(Object *obj, const char *name,
                                    Object *target, Error **errp)
{
    char *link_type;
    ObjectProperty *op;

    link_type = g_strdup_printf("link<%s>", object_get_typename(target));
    op = object_property_add(obj, name, link_type,
                             object_get_child_property, NULL,
                             NULL, target, errp);
    if (op != NULL) {
        op->resolve = object_resolve_child_property;
    }
    g_free(link_type);
}

gchar *object_get_canonical_path_component(Object *obj)
{
    ObjectProperty *prop = NULL;
    GHashTableIter iter;

    g_assert(obj);
    g_assert(obj->parent != NULL);

    g_hash_table_iter_init(&iter, obj->parent->properties);
    while (g_hash_table_iter_next(&iter, NULL, (gpointer *)&prop)) {
        if (!object_property_is_child(prop)) {
            continue;
        }

        if (prop->opaque == obj) {
            return g_strdup(prop->name);
        }
    }

    /* obj had a parent but was not a child, should never happen */
    g_assert_not_reached();
    return NULL;
}

gchar *object_get_canonical_path(Object *obj)
{
    Object *root = object_get_root();
    char *newpath, *path = NULL;

    while (obj != root) {
        char *component = object_get_canonical_path_component(obj);

        if (path) {
            newpath = g_strdup_printf("%s/%s", component, path);
            g_free(component);
            g_free(path);
            path = newpath;
        } else {
            path = component;
        }

        obj = obj->parent;
    }

    newpath = g_strdup_printf("/%s", path ? path : "");
    g_free(path);

    return newpath;
}

Object *object_resolve_path_component(Object *parent, const gchar *part)
{
    ObjectProperty *prop = object_property_find(parent, part, NULL);
    if (prop == NULL) {
        return NULL;
    }

    if (prop->resolve) {
        return prop->resolve(parent, prop->opaque, part);
    } else {
        return NULL;
    }
}

static Object *object_resolve_abs_path(Object *parent,
                                          gchar **parts,
                                          const char *typename,
                                          int index)
{
    Object *child;

    if (parts[index] == NULL) {
        return object_dynamic_cast(parent, typename);
    }

    if (strcmp(parts[index], "") == 0) {
        return object_resolve_abs_path(parent, parts, typename, index + 1);
    }

    child = object_resolve_path_component(parent, parts[index]);
    if (!child) {
        return NULL;
    }

    return object_resolve_abs_path(child, parts, typename, index + 1);
}

static Object *object_resolve_partial_path(Object *parent,
                                              gchar **parts,
                                              const char *typename,
                                              bool *ambiguous)
{
    Object *obj;
    GHashTableIter iter;
    ObjectProperty *prop;

    obj = object_resolve_abs_path(parent, parts, typename, 0);

    g_hash_table_iter_init(&iter, parent->properties);
    while (g_hash_table_iter_next(&iter, NULL, (gpointer *)&prop)) {
        Object *found;

        if (!object_property_is_child(prop)) {
            continue;
        }

        found = object_resolve_partial_path(prop->opaque, parts,
                                            typename, ambiguous);
        if (found) {
            if (obj) {
                if (ambiguous) {
                    *ambiguous = true;
                }
                return NULL;
            }
            obj = found;
        }

        if (ambiguous && *ambiguous) {
            return NULL;
        }
    }

    return obj;
}

Object *object_resolve_path_type(const char *path, const char *typename,
                                 bool *ambiguous)
{
    Object *obj;
    gchar **parts;

    parts = g_strsplit(path, "/", 0);
    assert(parts);

    if (parts[0] == NULL || strcmp(parts[0], "") != 0) {
        if (ambiguous) {
            *ambiguous = false;
        }
        obj = object_resolve_partial_path(object_get_root(), parts,
                                          typename, ambiguous);
    } else {
        obj = object_resolve_abs_path(object_get_root(), parts, typename, 1);
    }

    g_strfreev(parts);

    return obj;
}

Object *object_resolve_path(const char *path, bool *ambiguous)
{
    return object_resolve_path_type(path, TYPE_OBJECT, ambiguous);
}

typedef struct StringProperty
{
    char *(*get)(Object *, Error **);
    void (*set)(Object *, const char *, Error **);
} StringProperty;

static void property_get_str(Object *obj, Visitor *v, const char *name,
                             void *opaque, Error **errp)
{
    StringProperty *prop = opaque;
    char *value;
    Error *err = NULL;

    value = prop->get(obj, &err);
    if (err) {
        error_propagate(errp, err);
        return;
    }

    visit_type_str(v, name, &value, errp);
    g_free(value);
}

static void property_set_str(Object *obj, Visitor *v, const char *name,
                             void *opaque, Error **errp)
{
    StringProperty *prop = opaque;
    char *value;
    Error *local_err = NULL;

    visit_type_str(v, name, &value, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        return;
    }

    prop->set(obj, value, errp);
    g_free(value);
}

static void property_release_str(Object *obj, const char *name,
                                 void *opaque)
{
    StringProperty *prop = opaque;
    g_free(prop);
}

void object_property_add_str(Object *obj, const char *name,
                           char *(*get)(Object *, Error **),
                           void (*set)(Object *, const char *, Error **),
                           Error **errp)
{
    Error *local_err = NULL;
    StringProperty *prop = g_malloc0(sizeof(*prop));

    prop->get = get;
    prop->set = set;

    object_property_add(obj, name, "string",
                        get ? property_get_str : NULL,
                        set ? property_set_str : NULL,
                        property_release_str,
                        prop, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
    }
}

void object_class_property_add_str(ObjectClass *klass, const char *name,
                                   char *(*get)(Object *, Error **),
                                   void (*set)(Object *, const char *,
                                               Error **),
                                   Error **errp)
{
    Error *local_err = NULL;
    StringProperty *prop = g_malloc0(sizeof(*prop));

    prop->get = get;
    prop->set = set;

    object_class_property_add(klass, name, "string",
                              get ? property_get_str : NULL,
                              set ? property_set_str : NULL,
                              property_release_str,
                              prop, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
    }
}

//bool属性简单，就是一个get 一个set就可以了
typedef struct BoolProperty
{
    bool (*get)(Object *, Error **);
    void (*set)(Object *, bool, Error **);
} BoolProperty;

static void property_get_bool(Object *obj, Visitor *v, const char *name,
                              void *opaque, Error **errp)
{
    BoolProperty *prop = opaque;
    bool value;
    Error *err = NULL;

    value = prop->get(obj, &err);
    if (err) {
        error_propagate(errp, err);
        return;
    }

    visit_type_bool(v, name, &value, errp);
}

static void property_set_bool(Object *obj, Visitor *v, const char *name,
                              void *opaque, Error **errp)
{
    BoolProperty *prop = opaque;
    bool value;
    Error *local_err = NULL;

    visit_type_bool(v, name, &value, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        return;
    }

    prop->set(obj, value, errp);
}

static void property_release_bool(Object *obj, const char *name,
                                  void *opaque)
{
    BoolProperty *prop = opaque;
    g_free(prop);
}

void object_property_add_bool(Object *obj, const char *name,
                              bool (*get)(Object *, Error **),
                              void (*set)(Object *, bool, Error **),
                              Error **errp)
{
    Error *local_err = NULL;
    BoolProperty *prop = g_malloc0(sizeof(*prop)); //关键哟，这个就是具体的属性

    prop->get = get;
    prop->set = set;

    object_property_add(obj, name, "bool",
                        get ? property_get_bool : NULL,
                        set ? property_set_bool : NULL,
                        property_release_bool,
                        prop, &local_err); //prop作为opaque传入了
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
    }
}

void object_class_property_add_bool(ObjectClass *klass, const char *name,
                                    bool (*get)(Object *, Error **),
                                    void (*set)(Object *, bool, Error **),
                                    Error **errp)
{
    Error *local_err = NULL;
    BoolProperty *prop = g_malloc0(sizeof(*prop));

    prop->get = get;
    prop->set = set;

    object_class_property_add(klass, name, "bool",
                              get ? property_get_bool : NULL,
                              set ? property_set_bool : NULL,
                              property_release_bool,
                              prop, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
    }
}

static void property_get_enum(Object *obj, Visitor *v, const char *name,
                              void *opaque, Error **errp)
{
    EnumProperty *prop = opaque;
    int value;
    Error *err = NULL;

    value = prop->get(obj, &err);
    if (err) {
        error_propagate(errp, err);
        return;
    }

    visit_type_enum(v, name, &value, prop->strings, errp);
}

static void property_set_enum(Object *obj, Visitor *v, const char *name,
                              void *opaque, Error **errp)
{
    EnumProperty *prop = opaque;
    int value;
    Error *err = NULL;

    visit_type_enum(v, name, &value, prop->strings, &err);
    if (err) {
        error_propagate(errp, err);
        return;
    }
    prop->set(obj, value, errp);
}

static void property_release_enum(Object *obj, const char *name,
                                  void *opaque)
{
    EnumProperty *prop = opaque;
    g_free(prop);
}

void object_property_add_enum(Object *obj, const char *name,
                              const char *typename,
                              const char * const *strings,
                              int (*get)(Object *, Error **),
                              void (*set)(Object *, int, Error **),
                              Error **errp)
{
    Error *local_err = NULL;
    EnumProperty *prop = g_malloc(sizeof(*prop));

    prop->strings = strings;
    prop->get = get;
    prop->set = set;

    object_property_add(obj, name, typename,
                        get ? property_get_enum : NULL,
                        set ? property_set_enum : NULL,
                        property_release_enum,
                        prop, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
    }
}

void object_class_property_add_enum(ObjectClass *klass, const char *name,
                                    const char *typename,
                                    const char * const *strings,
                                    int (*get)(Object *, Error **),
                                    void (*set)(Object *, int, Error **),
                                    Error **errp)
{
    Error *local_err = NULL;
    EnumProperty *prop = g_malloc(sizeof(*prop));

    prop->strings = strings;
    prop->get = get;
    prop->set = set;

    object_class_property_add(klass, name, typename,
                              get ? property_get_enum : NULL,
                              set ? property_set_enum : NULL,
                              property_release_enum,
                              prop, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
    }
}

typedef struct TMProperty {
    void (*get)(Object *, struct tm *, Error **);
} TMProperty;

static void property_get_tm(Object *obj, Visitor *v, const char *name,
                            void *opaque, Error **errp)
{
    TMProperty *prop = opaque;
    Error *err = NULL;
    struct tm value;

    prop->get(obj, &value, &err);
    if (err) {
        goto out;
    }

    visit_start_struct(v, name, NULL, 0, &err);
    if (err) {
        goto out;
    }
    visit_type_int32(v, "tm_year", &value.tm_year, &err);
    if (err) {
        goto out_end;
    }
    visit_type_int32(v, "tm_mon", &value.tm_mon, &err);
    if (err) {
        goto out_end;
    }
    visit_type_int32(v, "tm_mday", &value.tm_mday, &err);
    if (err) {
        goto out_end;
    }
    visit_type_int32(v, "tm_hour", &value.tm_hour, &err);
    if (err) {
        goto out_end;
    }
    visit_type_int32(v, "tm_min", &value.tm_min, &err);
    if (err) {
        goto out_end;
    }
    visit_type_int32(v, "tm_sec", &value.tm_sec, &err);
    if (err) {
        goto out_end;
    }
    visit_check_struct(v, &err);
out_end:
    visit_end_struct(v, NULL);
out:
    error_propagate(errp, err);

}

static void property_release_tm(Object *obj, const char *name,
                                void *opaque)
{
    TMProperty *prop = opaque;
    g_free(prop);
}

void object_property_add_tm(Object *obj, const char *name,
                            void (*get)(Object *, struct tm *, Error **),
                            Error **errp)
{
    Error *local_err = NULL;
    TMProperty *prop = g_malloc0(sizeof(*prop));

    prop->get = get;

    object_property_add(obj, name, "struct tm",
                        get ? property_get_tm : NULL, NULL,
                        property_release_tm,
                        prop, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
    }
}

void object_class_property_add_tm(ObjectClass *klass, const char *name,
                                  void (*get)(Object *, struct tm *, Error **),
                                  Error **errp)
{
    Error *local_err = NULL;
    TMProperty *prop = g_malloc0(sizeof(*prop));

    prop->get = get;

    object_class_property_add(klass, name, "struct tm",
                              get ? property_get_tm : NULL, NULL,
                              property_release_tm,
                              prop, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
    }
}

static char *qdev_get_type(Object *obj, Error **errp)
{
    return g_strdup(object_get_typename(obj));
}

static void property_get_uint8_ptr(Object *obj, Visitor *v, const char *name,
                                   void *opaque, Error **errp)
{
    uint8_t value = *(uint8_t *)opaque;
    visit_type_uint8(v, name, &value, errp);
}

static void property_get_uint16_ptr(Object *obj, Visitor *v, const char *name,
                                    void *opaque, Error **errp)
{
    uint16_t value = *(uint16_t *)opaque;
    visit_type_uint16(v, name, &value, errp);
}

static void property_get_uint32_ptr(Object *obj, Visitor *v, const char *name,
                                    void *opaque, Error **errp)
{
    uint32_t value = *(uint32_t *)opaque;
    visit_type_uint32(v, name, &value, errp);
}

static void property_get_uint64_ptr(Object *obj, Visitor *v, const char *name,
                                    void *opaque, Error **errp)
{
    uint64_t value = *(uint64_t *)opaque;
    visit_type_uint64(v, name, &value, errp);
}

void object_property_add_uint8_ptr(Object *obj, const char *name,
                                   const uint8_t *v, Error **errp)
{
    object_property_add(obj, name, "uint8", property_get_uint8_ptr,
                        NULL, NULL, (void *)v, errp);
}

void object_class_property_add_uint8_ptr(ObjectClass *klass, const char *name,
                                         const uint8_t *v, Error **errp)
{
    object_class_property_add(klass, name, "uint8", property_get_uint8_ptr,
                              NULL, NULL, (void *)v, errp);
}

void object_property_add_uint16_ptr(Object *obj, const char *name,
                                    const uint16_t *v, Error **errp)
{
    object_property_add(obj, name, "uint16", property_get_uint16_ptr,
                        NULL, NULL, (void *)v, errp);
}

void object_class_property_add_uint16_ptr(ObjectClass *klass, const char *name,
                                          const uint16_t *v, Error **errp)
{
    object_class_property_add(klass, name, "uint16", property_get_uint16_ptr,
                              NULL, NULL, (void *)v, errp);
}

void object_property_add_uint32_ptr(Object *obj, const char *name,
                                    const uint32_t *v, Error **errp)
{
    object_property_add(obj, name, "uint32", property_get_uint32_ptr,
                        NULL, NULL, (void *)v, errp);
}

void object_class_property_add_uint32_ptr(ObjectClass *klass, const char *name,
                                          const uint32_t *v, Error **errp)
{
    object_class_property_add(klass, name, "uint32", property_get_uint32_ptr,
                              NULL, NULL, (void *)v, errp);
}

void object_property_add_uint64_ptr(Object *obj, const char *name,
                                    const uint64_t *v, Error **errp)
{
    object_property_add(obj, name, "uint64", property_get_uint64_ptr,
                        NULL, NULL, (void *)v, errp);
}

void object_class_property_add_uint64_ptr(ObjectClass *klass, const char *name,
                                          const uint64_t *v, Error **errp)
{
    object_class_property_add(klass, name, "uint64", property_get_uint64_ptr,
                              NULL, NULL, (void *)v, errp);
}

typedef struct {
    Object *target_obj;
    char *target_name;
} AliasProperty;

static void property_get_alias(Object *obj, Visitor *v, const char *name,
                               void *opaque, Error **errp)
{
    AliasProperty *prop = opaque;

    object_property_get(prop->target_obj, v, prop->target_name, errp);
}

static void property_set_alias(Object *obj, Visitor *v, const char *name,
                               void *opaque, Error **errp)
{
    AliasProperty *prop = opaque;

    object_property_set(prop->target_obj, v, prop->target_name, errp);
}

static Object *property_resolve_alias(Object *obj, void *opaque,
                                      const gchar *part)
{
    AliasProperty *prop = opaque;

    return object_resolve_path_component(prop->target_obj, prop->target_name);
}

static void property_release_alias(Object *obj, const char *name, void *opaque)
{
    AliasProperty *prop = opaque;

    g_free(prop->target_name);
    g_free(prop);
}

//alias属性???
void object_property_add_alias(Object *obj, const char *name,
                               Object *target_obj, const char *target_name,
                               Error **errp)
{
    AliasProperty *prop;
    ObjectProperty *op;
    ObjectProperty *target_prop;
    gchar *prop_type;
    Error *local_err = NULL;

    target_prop = object_property_find(target_obj, target_name, errp);
    if (!target_prop) {
        return;
    }

    if (object_property_is_child(target_prop)) {
        prop_type = g_strdup_printf("link%s",
                                    target_prop->type + strlen("child"));
    } else {
        prop_type = g_strdup(target_prop->type);
    }

    prop = g_malloc(sizeof(*prop));
    prop->target_obj = target_obj;
    prop->target_name = g_strdup(target_name);

    op = object_property_add(obj, name, prop_type,
                             property_get_alias,
                             property_set_alias,
                             property_release_alias,
                             prop, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        g_free(prop);
        goto out;
    }
    op->resolve = property_resolve_alias;

    object_property_set_description(obj, op->name,
                                    target_prop->description,
                                    &error_abort);

out:
    g_free(prop_type);
}

void object_property_set_description(Object *obj, const char *name,
                                     const char *description, Error **errp)
{
    ObjectProperty *op;

    op = object_property_find(obj, name, errp);
    if (!op) {
        return;
    }

    g_free(op->description);
    op->description = g_strdup(description);
}

void object_class_property_set_description(ObjectClass *klass,
                                           const char *name,
                                           const char *description,
                                           Error **errp)
{
    ObjectProperty *op;

    op = g_hash_table_lookup(klass->properties, name);
    if (!op) {
        error_setg(errp, "Property '.%s' not found", name);
        return;
    }

    g_free(op->description);
    op->description = g_strdup(description);
}

static void object_instance_init(Object *obj)
{
    object_property_add_str(obj, "type", qdev_get_type, NULL, NULL);
}

static void register_types(void)
{
    static TypeInfo interface_info = {
        .name = TYPE_INTERFACE,
        .class_size = sizeof(InterfaceClass),
        .abstract = true,
    };

    static TypeInfo object_info = {
        .name = TYPE_OBJECT,
        .instance_size = sizeof(Object),
        .instance_init = object_instance_init,
        .abstract = true,
    };

    type_interface = type_register_internal(&interface_info);
    type_register_internal(&object_info);
}

type_init(register_types)
