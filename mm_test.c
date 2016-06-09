/*-
 * Copyright (c) 2016 Michael F. Plass
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include <stdio.h>
#include <stdint.h>
#include "mm.h"
#include "mm_sys.h"
#undef NDEBUG
#include <assert.h>

static void tr1(mm_sys_blk_t * blk, uintptr_t * p)
{
    /* demo trace routine - prefix contains pointer count */
    unsigned n = 0;
    n = *(p - 1);
    while (n) {
        mm_mark_one(*p, 0);
        p++;
        n--;
    }
}

static struct mm_ob_class myclass = {MM_OB_CLASS_SC, &tr1, sizeof(uintptr_t), 0, 0};

static void *t_alloc(size_t sz, unsigned n_ptr)
{
    uintptr_t *ans = 0;
    ans = mm_alloc(&myclass, sz);
    if (ans) {
        *(ans - 1) = n_ptr;
    }
    return ans;
}

static void *testroot;

void marktestroot(mm_sys_root_marker_t * self)
{
    mm_mark_one((uintptr_t) testroot, 0);
}

static mm_sys_root_marker_t testmark = {&marktestroot};
struct cons {
    uintptr_t car;
    struct cons *cdr;
};

int fin = 0;

static void notefin(void *obj, void *clientData)
{
    fin++;
}

int main(void)
{
    int *a = 0;
    void **b = 0;
    int *c = 0;
    int i = 0;
    struct cons *list = 0;

    mm_register_root_marker(&testmark);
    a = t_alloc(1900, 0);
    a[0] = 10;
    testroot = b = t_alloc(100 * sizeof(b[0]), 100);
    b[0] = (void *) -1;
    b[20] = a;
    b[50] = b;
    c = t_alloc(10001, 0);
    c[0] = 12;
    mm_register_finalizer(c, &notefin, &c);
    a[1] = (intptr_t) c;
    mm_collect();
    assert(c[0] == 12);
    mm_collect();
    assert(c[0] != 12);
    c = t_alloc(16, 2);
    while (i < 20) {
        int j = 505;
        list = 0;
        while (j--) {
            struct cons *cons = 0;
            if (j & 1) {
                cons = t_alloc(sizeof *cons, 2);
            }
            else {
                cons = mm_alloc(mm_pairs, sizeof *cons);
            }
            cons->car = (1000 * j + i);
            cons->cdr = list;
            list = cons;
        }
        i += 1;
        b[42] = list;
        mm_collect();
    }
    testroot = 0;
    mm_collect();
    return ((intptr_t) a + (intptr_t) b + (intptr_t) c + (intptr_t) list);
}
