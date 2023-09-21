/*
 * Internal definitions for a target's KVM support
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 *
 */

#ifndef QEMU_KVM_INT_H
#define QEMU_KVM_INT_H

#include "sysemu/sysemu.h"
#include "sysemu/accel.h"
#include "sysemu/kvm.h"

typedef struct KVMSlot
{
    hwaddr start_addr; //GPA , refer to kvm_set_phys_mem
    ram_addr_t memory_size;
    void *ram; //HVA
    int slot;
    int flags;
} KVMSlot;

typedef struct KVMMemoryListener {
    MemoryListener listener;
    KVMSlot *slots; //该结构表示KVM内存slot(内存插槽，可以认为 KVM 最多支持这么多段内存)，即对于KVM来说，虚拟机有多少段内存. refer to kvm_init ioctl(KVM_CAP_NR_MEMSLOTS)
    int as_id;
} KVMMemoryListener;

#define TYPE_KVM_ACCEL ACCEL_CLASS_NAME("kvm")

#define KVM_STATE(obj) \
    OBJECT_CHECK(KVMState, (obj), TYPE_KVM_ACCEL)

void kvm_memory_listener_register(KVMState *s, KVMMemoryListener *kml,
                                  AddressSpace *as, int as_id);

#endif
