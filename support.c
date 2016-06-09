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
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <stdarg.h>
#include "mm_sys.h"
#include "scutum.h"

Activation WrongNumArgs(Activation act, ThData th)
{
    fprintf(stderr, " Wrong number of args!\n");
    return sc_raise_error(act, th, FIXNUM_FROM_INT(1));
}

Activation WrongType(Activation act, ThData th, int lineno)
{
    fprintf(stderr, " Wrong Type! - (line %d) 0x%lx\n", lineno, (long) (th->culprit));
    return sc_raise_error(act, th, CONS(FIXNUM_FROM_INT(lineno), CONS(th->culprit, EMPTY_LIST)));
}

Activation sc_raise_error(Activation act, ThData th, Value info)
{
    Proc handler = (Proc) (sc_global_binding(th, (Value) sc_symlit("%error-handler"))->cdr);
    if (!PROC_P(handler)) {
        exit(1);
    }
    if (th->narg > FIXNUM_FROM_INT(1)) {
        act->r = CONS(act->b, act->r);
    }
    if (th->narg > FIXNUM_FROM_INT(0)) {
        act->r = CONS(act->a, act->r);
    }
    act->r = CONS(act->r, EMPTY_LIST);
    act->a = info;
    act->b = sc_get_dbgtrace(th);
    act->codeh = handler->entry_pt;
    act->env = handler->env;
    th->narg = FIXNUM_FROM_INT(3);
    return (act);
}

/*** sc_get_code is occasionally useful from the debugger */
enum type_code sc_get_code(Value x)
{
    return (SC_GET_CODE(x));
}

Pair sc_th_cons(ThData th, Value x, Value y)
{
    if (th != NOTH) {
        Pair p = th->pairs;
        if (PAIR_P(p)) {
            th->pairs = (Pair) (p->cdr);
            p->car = x;
            p->cdr = y;
            return p;
        }
    }
    return sc_th_hard_cons(th, x, y);
}

Pair sc_th_hard_cons(ThData th, Value x, Value y)
{
    if (th != NOTH) {
        (void) sc_prime_cons_pump(NULL, th);
    }
    return sc_cons(x, y);
}

Pair sc_cons(Value x, Value y)
{
    Pair p = 0;
    p = mm_alloc(mm_pairs, sizeof(*p));
    p->car = x;
    p->cdr = y;
    return p;
}

Activation sc_prime_cons_pump(Activation act, ThData th)
{
    void *ob[12];
    int i = sizeof(ob) / sizeof(ob[0]);
    Value x = (Value) (th->pairs);
    i = mm_alloc_several(mm_pairs, sizeof(struct PairRep), i, ob);
    while (i--) {
        Pair p = ob[i];
        ob[i] = 0;
        p->car = UNSPEC;
        p->cdr = x;
        x = VAL(p);
    }
    th->pairs = (Pair) x;
    return act;
}

Value sc_assq(Value key, Value assoclist)
{
    uintptr_t limit = 2000000000;
    while (PAIR_P(assoclist) && limit--) {
        Value item = U_CAR(assoclist);
        if (!PAIR_P(item))
            break;
        if (key == U_CAR(item))
            return item;
        assoclist = U_CDR(assoclist);
    }
    return (NULL_P(assoclist)) ? HASH_F : WRONG_TYPE;
}

Value sc_dreverse(Value lst)
{
    Value ans = EMPTY_LIST;
    while (PAIR_P(lst)) {
        Value rest = U_CDR(lst);
        ((Pair) lst)->cdr = ans;
        ans = lst;
        lst = rest;
    }
    return ans;
}

Activation sc_prime_act_pump(Activation act, ThData th)
{
    int i = 2;
    struct Activation_Record dummy = {
        TAGTY(code_activation), 0, HASH_F,
        UNDEF, UNDEF, UNDEF,
        (Activation) HASH_F, (Activation) HASH_F
    };
    Activation z = &dummy;
    Activation x = &dummy;
    (void) th;
    while (i--) {
        Activation y = mm_alloc(mm_normal, sizeof(*y));
        memset(y, UNSPEC_BYTE, sizeof(*y));
        y->tc = TAGTY(code_activation);
        z->cont = y;
        y->link = z;
        y->cont = x;
        x->link = y;
        z = y;
    }
    if (act) {
        x->cont->link = act->link;
        x->link->cont = act;
        if (HEAP_P(act->link)) {
            act->link->cont = x->cont;
        }
        act->link = x->link;
    }
    else {
        x->cont->link = (Activation) HASH_F;
        x->link->cont = (Activation) HASH_F;
        act = x->link;
    }
    return act;
}

Activation sc_copy_continuation(Activation act, ThData th)
{
    Activation old = act;
    struct Activation_Record dummy = {
        code_activation, 0, HASH_F, UNDEF, UNDEF, UNDEF, 0, 0
    };
    Activation d = &dummy;
    if (!HEAP_P(act))
        return act;
    d->link = d->cont = d;
    while (ACTIVATION_P(old)) {
        Activation x = mm_alloc(mm_normal, sizeof(*x));
        memmove(x, old, sizeof(*x));
        x->r = sc_list_copy(th, x->r);
        x->link = d->link;
        x->cont = d;
        x->link->cont = x;
        x->cont->link = x;
        old = old->cont;
    }
    d->link->cont = d->cont->link = (Activation) HASH_F;
    return d->cont;
}

ListValue sc_find_next_dynamic(ListValue old, ListValue new)
{
    ListValue common = sc_common_list_portion(old, new);
    ListValue ans = EMPTY_LIST;
    while (old != common) {
        Value first = U_CAR(old);
        if (PAIR_P(first) && U_CAR(first) == AFTERTHUNK) {
            return old;
        }
        old = U_CDR(old);
    }
    while (new != common) {
        Value first = U_CAR(new);
        if (PAIR_P(first) && U_CAR(first) == BEFORETHUNK) {
            ans = new;
        }
        new = U_CDR(new);
    }
    return ans;
}

Activation sc_punt(Activation act, ThData th, Value binding)
{
    if (PAIR_P(binding)) {
        Value rq = U_CDR(binding);
        if (PROC_P(rq)) {
            Proc r = (Proc) rq;
            act->env = r->env;
            act->codeh = r->entry_pt;
            return act;
        }
    }
    RETURN(UNDEF);
}

ThData sc_new_ThData(void)
{
    ThData th = 0;
    th = mm_alloc(mm_normal, sizeof(*th));
    memset(th, UNSPEC_BYTE, sizeof(*th));
    th->tc = TAGTY(code_thread_data);
    th->dynamic = (List) EMPTY_LIST;
    th->pairs = (List) EMPTY_LIST;
    th->global = sc_new_empty_global();
    th->symtrace = HASH_F;
    th->dbgtracebuf = sc_new_circular_list(th, 20);
    return th;
}

Value sc_new_empty_global(void)
{
    return VAL(sc_cons(VAL(sc_cons(EMPTY_LIST, EMPTY_LIST)), EMPTY_LIST));
}

Pair sc_new_circular_list(ThData th, int count)
{
    Pair first = (Pair) CONS(HASH_F, EMPTY_LIST);
    Pair last = first;
    while (count > 1) {
        Pair t = (Pair) CONS(HASH_F, EMPTY_LIST);
        last->cdr = (Value) t;
        last = t;
        count--;
    }
    last->cdr = (Value) first;
    return first;
}

Value sc_get_dbgtrace(ThData th)
{
    Pair done = th->dbgtracebuf;
    Pair cur = (Pair) done->cdr;
    Pair first = (Pair) CONS(done->car, EMPTY_LIST);
    Pair last = first;
    while (cur != done) {
        Pair t = (Pair) CONS(cur->car, EMPTY_LIST);
        last->cdr = (Value) t;
        last = t;
        cur = (Pair) (cur->cdr);
    }
    return (Value) first;
}

Pair sc_global_binding(ThData th, Value symbol)
{
    Value head = th->global;
    if (PAIR_P(head)) {
        Value old = sc_assq(symbol, U_CDR(head));
        if (PAIR_P(old))
            return ((Pair) old);
        if (FALSE_P(old)) {
            Value binding = CONS(symbol, UNDEF);
            ((Pair) (head))->cdr = CONS(binding, U_CDR(head));
            return ((Pair) binding);
        }
    }
    abort();
}

Value sc_str_global_binding(ThData th, const char *name)
{
    return VAL(sc_global_binding(th, VAL(sc_symlit(name))));
}

Int sc_list_length(Value lst)
{
    Pair lag = (Pair) lst;
    Int n = 0;
    while (PAIR_P(lst)) {
        lst = U_CDR(lst);
        n++;
        if (lst == (Value) lag)
            return -1;
        if (!PAIR_P(lst))
            break;
        lst = U_CDR(lst);
        n++;
        if (lst == (Value) lag)
            return -1;
        lag = (Pair) U_CDR(lag);
    }
    return (NULL_P(lst)) ? n : -1;
}

Value sc_list_copy(ThData th, Value lst)
{
    Pair head;
    Pair last;
    if (!PAIR_P(lst))
        return (lst);
    head = last = (Pair) CONS(U_CAR(lst), EMPTY_LIST);
    lst = (Value) U_CDR(lst);
    while (PAIR_P(lst)) {
        last = (Pair) (last->cdr = CONS(U_CAR(lst), EMPTY_LIST));
        lst = (Value) U_CDR(lst);
    }
    return ((Value) head);
}

ListValue sc_common_list_portion(ListValue a, ListValue b)
{
    Int alen = sc_list_length(a);
    Int blen = sc_list_length(b);
    if (alen < 0 || blen < 0)
        return EMPTY_LIST;
    while (alen > blen) {
        alen -= 1;
        a = U_CDR(a);
    }
    while (blen > alen) {
        blen -= 1;
        b = U_CDR(b);
    }
    while (a != b) {
        a = U_CDR(a);
        b = U_CDR(b);
    }
    return a;
}

Value sc_make_list_0(Value value,... /* Needs terminator! */ )
{
    ThData th = NOTH;
    List head = (Pair) CONS(value, (Pair) EMPTY_LIST);
    List last = head;
    Value v;
    va_list ap;
    va_start(ap, value);
    while (0 != (((v = va_arg(ap, Value))))) {
        last = (Pair) (last->cdr = CONS(v, (Pair) EMPTY_LIST));
    }
    v = va_arg(ap, Value);
    /* The following check is useful for debugging missing terminators */
    if (v != SPECIAL(-1))
        abort();
    va_end(ap);
    return VAL(head);
}

static struct {
    Vector symbol_hash_table;
    Activation act;
    ThData th;
    volatile void *stackbase;
} root;

void sc_set_simple_stack_base(volatile void *stackbase)
{
    root.stackbase = stackbase;
}

static void root_marker_fn(struct mm_sys_root_marker * self)
{
    uintptr_t *p = (void *) &root;
    int i = sizeof(root) / sizeof(uintptr_t);
    while (i > 0) {
        mm_mark_one(p[--i], 0);
    }
    if (root.stackbase) {
        mm_generic_mark_my_stack(root.stackbase);
    }
    (void) self;
}

static mm_sys_root_marker_t root_mark = {&root_marker_fn, 0, 0, 0, 0};

void sc_collect(Activation act, ThData th)
{
    /* This is only OK if there is only one thread. */
    if (root.symbol_hash_table) {
        root.act = act;
        root.th = th;
        if (HEAP_P(act)) {
            act->link = (Activation) HASH_F;
        }
        th->pairs = (List) EMPTY_LIST;
        mm_collect();
    }
}

static pthread_mutex_t symbol_hash_lock = PTHREAD_MUTEX_INITIALIZER;
static size_t default_hash_table_size = 2003;
uintptr_t sc_string_hash(String string)
{
    uintptr_t n = INT_FROM_FIXNUM(string->length);
    uintptr_t hash = n + (n << 7);
    size_t i;
    for (i = 0; i < n; i++) {
        hash = hash + (hash << 5) + SCHARS(string)[i];
    }
    return (hash & 0X3FFFFFFF);
}

Symbol sc_string_to_symbol(ThData th, String string)
{
    Symbol result = 0;
    if (STRING_P(string)) {
        Vector tab = 0;
        uintptr_t hash = sc_string_hash(string);
        size_t index = 0;
        List *slot = 0;
        List p = 0;
        pthread_mutex_lock(&symbol_hash_lock);
        tab = root.symbol_hash_table;
        if (!tab) {
            mm_register_root_marker(&root_mark);
            root.symbol_hash_table = tab = sc_make_vector(default_hash_table_size, EMPTY_LIST);
        }
        assert(VECTOR_P(tab));
        index = hash % VECTOR_LENGTH(tab);
        slot = (List *) (VECTOR_ARR(tab) + index);
        p = *slot;
        while (!(NULL_P(p))) {
            Symbol this;
            assert(PAIR_P(p) && SYMBOL_P(U_CAR(p)));
            this = (Symbol) U_CAR(p);
            if (sc_string_equal(this->pval, string)) {
                result = this;
                goto finish;
            }
            p = (List) U_CDR(p);
        }
        result = mm_alloc(mm_normal, sizeof(*result));
        result->tc = TAGTY(code_symbol);
        result->pval = sc_copy_string(string);
        *slot = (List) CONS(result, *slot);
finish:
        pthread_mutex_unlock(&symbol_hash_lock);
    }
    return result;
}

Symbol sc_symlit(const char *s)
{
    return (sc_string_to_symbol(NOTH, sc_strlit(s)));
}

Value sc_c_string_to_symbol(const char *s)
{
    return (VAL(sc_symlit(s)));
}

String sc_copy_string(String src)
{
    String d = sc_make_string(STRING_LENGTH(src));
    sc_move_string_chars(d, src, 0);
    return d;
}

String sc_make_string(size_t length)
{
    size_t sz = sizeof(struct Aggregate_Header) + length;
    String s = mm_alloc(mm_ptrfree, sz);
    s->tc = TAGTY(code_string);
    s->length = FIXNUM_FROM_INT(length);
    return (s);
}

String sc_strlit(const char *s)
{
    size_t length = strlen(s);
    String string = sc_make_string(length);
    memmove(SCHARS(string), s, length);
    return (string);
}

void sc_fill_string(String dst, uintptr_t ch)
{
    if (STRING_P(dst)) {
        memset(SCHARS(dst), ch, STRING_LENGTH(dst));
    }
}

bool sc_get_string_chars(char buf[], size_t bufsize, Value s)
{
    if (STRING_P(s)) {
        size_t length = STRING_LENGTH(s);
        if (length < bufsize) {
            memmove(buf, SCHARS(s), length);
            buf[length] = 0;
            return 1;
        }
    }
    return 0;
}

bool sc_string_equal(String a, String b)
{
    size_t len;
    assert(STRING_P(a) && STRING_P(b));
    return ((len = STRING_LENGTH(a)) == STRING_LENGTH(b)) && 0 == memcmp(SCHARS(a), SCHARS(b), len);
}

void sc_move_string_chars(String dst, String src, size_t srcStart)
{
    if (STRING_P(dst) && STRING_P(src)) {
        size_t dstlen = STRING_LENGTH(dst);
        size_t srclen = STRING_LENGTH(src);
        if (srcStart < srclen) {
            size_t len = srclen - srcStart;
            if (dstlen < len) {
                len = dstlen;
            }
            memmove(SCHARS(dst), SCHARS(src) + srcStart, len);
        }
    }
}

List sc_string_to_list(ThData th, String src)
{
    size_t len = 0;
    uint8_t *c = 0;
    Pair ans = (Pair) (EMPTY_LIST);
    assert(STRING_P(src));
    len = STRING_LENGTH(src);
    c = SCHARS(src);
    while (len--) {
        ans = (Pair) CONS(CHARVAL_FROM_UINT(c[len]), ans);
    }
    return (ans);
}

Vector sc_make_vector(size_t length, Value fill)
{
    size_t sz = sizeof(struct Aggregate_Header) + length * sizeof(Value);
    Vector v = mm_alloc(mm_normal, sz);
    Value *slot = VECTOR_ARR(v);
    v->tc = TAGTY(code_vector);
    v->length = FIXNUM_FROM_INT(length);
    while (length--) {
        *(slot++) = fill;
    }
    return (v);
}

Value sc_new_Bignum(bool exact, bool neg, Int exponent, Value mag)
{
    Bignum ans;
    if (!FIXNUMABLE(exponent))
        return NYI;
    ans = mm_alloc(mm_normal, sizeof(*ans));
    ans->tc = TAGTY(code_bignum);
    ans->flags = (exact ? 0 : NUM_INEXACT) | (neg ? NUM_NEG : 0) | NUM_FLAG;
    ans->exponent = FIXNUM_FROM_INT(exponent);
    ans->mag = mag;
    return (VAL(ans));
}

Value sc_new_Float64(bool exact, double val)
{
    Float64 ans;
    ans = mm_alloc(mm_ptrfree, sizeof(*ans));
    ans->tc = TAGTY(code_float64);
    ans->flags = (exact ? 0 : NUM_INEXACT) | (val < 0.0 ? NUM_NEG : 0) | NUM_FLAG;
    ans->value = val;
    return (VAL(ans));
}

Value sc_new_Rational(bool neg, Value numer, Value denom)
{
    Rational ans;
    ans = mm_alloc(mm_normal, sizeof(*ans));
    ans->tc = TAGTY(code_rational);
    ans->flags = (neg ? NUM_NEG : 0) | NUM_FLAG;
    ans->numerator = numer;
    ans->denominator = denom;
    return (VAL(ans));
}

Value sc_new_Complex(bool exact, Value realpart, Value imagpart)
{
    Complex ans;
    ans = mm_alloc(mm_normal, sizeof(*ans));
    ans->tc = TAGTY(code_complex);
    ans->flags = (exact ? 0 : NUM_INEXACT) | NUM_FLAG;
    ans->real_part = realpart;
    ans->imag_part = imagpart;
    return (VAL(ans));
}

Value sc_new_Port(Value name, Value data, Value reader, Value writer, Value closer)
{
    Port ans;
    ans = mm_alloc(mm_normal, sizeof(*ans));
    ans->tc = TAGTY(code_port);
    ans->name = name;
    ans->charbuf = HASH_F;
    ans->data = data;
    ans->read_char = (Proc) (PROC_P(reader) ? reader : HASH_F);
    ans->write_char = (Proc) (PROC_P(writer) ? writer : HASH_F);
    ans->close = (Proc) (PROC_P(closer) ? closer : HASH_F);
    return (VAL(ans));
}

Whatever sc_new_box(enum type_code tc, void *sysdata)
{
    Whatever ans;
    if (tc < code_box)
        abort();
    ans = mm_alloc(mm_normal, sizeof(*ans));
    ans->tc = TAGTY(tc);
    ans->sysdata = sysdata;
    return ans;
}

Activation sc_run_main_loop(Activation act, ThData th)
{
    Activation done = act->cont;
    unsigned countdown = 0;
    while (act != done) {
        act = (*(act->codeh->pc)) (act, th);
        if (!countdown) {
            size_t k = mm_get_heap_growth();
            if (k >= 1000000) {
                sc_collect(act, th);
            }
            countdown = 10000;
        }
        countdown--;
    }
    return done;
}
