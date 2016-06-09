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
#include <stdlib.h>
#include <string.h>
#include "scutum.h"
#include "primitives.h"

static struct {
    NativeCode fn;
    const char *name;
} primtab[] = {
#include "prim_items.h"
};
static size_t numprim = sizeof(primtab) / sizeof(*primtab);

static struct {
    PrimInitProc fn;
    const char *name;
} priminittab[] = {
#include "prim_init_items.h"
};
static size_t numpriminit = sizeof(priminittab) / sizeof(*priminittab);

Value sci_enumerate_primitives(Activation act, ThData th, Value(*each) (Activation act, ThData th, NativeCode fn, const char *name))
{
    size_t n = numprim;
    size_t i = 0;
    for (i = 0; i < n; i++) {
        Value res = (*each) (act, th, primtab[i].fn, primtab[i].name);
        if (res && TRUE_P(res))
            return res;
    }
    return HASH_F;
}

CodeHandle sc_new_CodeHandle(NativeCode pc)
{
    CodeHandle c;
    c = mm_alloc(mm_normal, sizeof(*c));
    c->i = c->j = c->k = UNSPEC;
    c->pc = pc;
    return c;
}

Proc sc_new_Proc(CodeHandle x, Value name)
{
    Proc p;
    p = mm_alloc(mm_normal, sizeof(*p));
    p->tc = TAGTY(code_proc);
    p->entry_pt = x;
    p->name = name;
    p->rep = (List) EMPTY_LIST;
    p->env = HASH_F;
    return p;
}

void sc_define_primitives(ThData th)
{
    size_t n = numprim;
    size_t i = 0;
    for (i = 0; i < n; i++) {
        Value name = (Value) sc_symlit(primtab[i].name);
        Pair binding = sc_global_binding(th, name);
        CodeHandle c = sc_new_CodeHandle(primtab[i].fn);
        Proc p = sc_new_Proc(c, name);
        binding->cdr = (Value) p;
    }
    n = numpriminit;
    for (i = 0; i < n; i++) {
        Value name = (Value) sc_symlit(priminittab[i].name);
        Pair binding = sc_global_binding(th, name);
        Value pq = binding->cdr;
        if (PROC_P(pq)) {
            Proc p = (Proc) pq;
            PrimInitProc fn = priminittab[i].fn;
            (*fn) (th, p->entry_pt);
        }
    }
}
