/*
 * Commandline option parsing functions
 *
 * Copyright (c) 2003-2008 Fabrice Bellard
 * Copyright (c) 2009 Kevin Wolf <kwolf@redhat.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#ifndef QEMU_OPTION_INT_H
#define QEMU_OPTION_INT_H

#include "qemu/option.h"
#include "qemu/error-report.h"

// 一个选项 QemuOpts 有很多属性的，每个属性(或者说小选项，被组织为该结构)
struct QemuOpt {
    char *name; // 属性名
    char *str; // 属性值

    const QemuOptDesc *desc;
    union {
        bool boolean;
        uint64_t uint;
    } value; // 属性值

    QemuOpts     *opts; // 指向 parent
    QTAILQ_ENTRY(QemuOpt) next; // link 到下一个属性
};

// 每个类型(QemuOptsList)的选项都可能有多个的，那么每个选项实例有一个 id 的
struct QemuOpts {
    char *id;
    QemuOptsList *list; // 指向parent
    Location loc;
    QTAILQ_HEAD(QemuOptHead, QemuOpt) head;
    QTAILQ_ENTRY(QemuOpts) next;
};

/* QemuOptsList 表示某个类型的选项，一共有 48 种，被组织在全局数组 vm_config_groups 中
 *	- 在 main 函数开头位置，通过 qemu_add_opts 填充 vm_config_groups 数组的
 *	- QemuOptsList 将所有该类型的选项 (QemuOpts) 组织到一个 list 中，每个 node 表示一个选项实例
 *
 * QemuOpts:
 *      - 某个类型的一个具体选项
 *      - 显然一个选项有很多属性值的，这些属性值被组织为一个 list，通过 QemuOpts 索引
 *
 * QemuOpt:
 *	- 表示某个选项的一个属性值: name + value(str, bool. int)
 * */

#endif
