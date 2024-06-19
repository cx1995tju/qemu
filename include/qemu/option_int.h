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

struct QemuOpt {
    char *name;			// opt name, key
    char *str;			// value  string 类型

    const QemuOptDesc *desc;	// desc
    union {
        bool boolean;
        uint64_t uint;
    } value;			// value	u64/bool 类型

    QemuOpts     *opts;		// link to QemuOpts
    QTAILQ_ENTRY(QemuOpt) next; // QemuOpts has many k-v pairs(aka QemuOpt), organize these k-v pairs with next
};

struct QemuOpts {
    char *id;			 // key
    QemuOptsList *list;		 // link to QemuOptsList
    Location loc; // %LOC_CMDLINE
    QTAILQ_HEAD(, QemuOpt) head; // organize the child struct QemuOpt
    QTAILQ_ENTRY(QemuOpts) next; // link to parent struct QemuOptsList
};

#endif
