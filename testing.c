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
#undef NDEBUG
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <setjmp.h>
#include <unistd.h>
#include <limits.h>
#include "scutum.h"
#include "primitives.h"

static Value nn(Int x)
{
    if (!FIXNUMABLE(x))
        fprintf(stderr, "*** %ld ***\n", x);
    assert(FIXNUMABLE(x));
    return FIXNUM_FROM_INT(x);
}

static Value ff(double x)
{
    return sc_new_Float64(false, x);
}

static Value cc(unsigned char ch)
{
    return VAL(CHARVAL_FROM_UINT(ch));
}

#define ATOM sc_c_string_to_symbol
#define list sc_make_list_0
#define _ IV(0), SPECIAL(-1)

static const Value zero = FIXNUM_FROM_INT(0);
static const Value one = FIXNUM_FROM_INT(1);
static const Value two = FIXNUM_FROM_INT(2);
static const Value three = FIXNUM_FROM_INT(3);
static jmp_buf next_test;
static int testnum = 0;
static int failures = 0;
static char *expect_fail = 0;

static void OOPS(const char *msg)
{
    if (expect_fail && 0 == strcmp(msg, expect_fail)) {
        expect_fail = 0;
    }
    else {
        fprintf(stderr, "Test %d FAILED - %s\n", testnum, msg);
        failures++;
    }
    longjmp(next_test, testnum);
}

static void CheckTypeDisjointness(Value x)
{
    int n = 0;
    if (NUMBER_P(x)) {
        n++;
    }
    if (SYMBOL_P(x)) {
        n++;
    }
    if (PROC_P(x)) {
        n++;
    }
    if (PORT_P(x)) {
        n++;
    }
    if (CHAR_P(x)) {
        n++;
    }
    if (NULL_P(x)) {
        n++;
    }
    if (PAIR_P(x)) {
        n++;
    }
    if (VECTOR_P(x)) {
        n++;
    }
    if (STRING_P(x)) {
        n++;
    }
    if (BOOLEAN_P(x)) {
        n++;
    }
    if (x == EOF_OBJECT) {
        n++;
    }
    if (n == 0) {
        if (x != UNSPEC && x != UNDEF)
            fprintf(stderr, "Value of unknown type - %lx\n", (unsigned long) (x));
    }
    if (n > 1)
        OOPS("Types not disjoint");
}

static uintptr_t cycle_limit = 1000;
static uintptr_t cycles_used;

static bool TestMainLoop(Activation act, ThData th)
{
    uintptr_t cycle = 0;
    Activation done = act->cont;
    while (act != done) {
        if (cycle > cycle_limit) {
            OOPS("cycle_limit exceeded");
            th->culprit = VAL(act);
            cycles_used = cycle;
            return false;
        }
        act = (*(act->codeh->pc)) (act, th);
        CheckTypeDisjointness(th->val);
        cycle++;
        if (!(cycle & 127))
            sc_collect(act, th);
    }
    cycles_used = cycle;
    return true;
}

static Activation nop(Activation act, ThData th)
{
    return act;
}

static Activation oops(Activation act, ThData th)
{
    OOPS("hit bottom");
    return 0;                   /* not reached */
}

static struct CodeHandle_Rep t1[] = {
    {HASH_F, HASH_F, HASH_F, &nop},
    {HASH_F, HASH_F, HASH_F, &scp_cons},
    {HASH_F, HASH_F, HASH_F, &oops}
};

Activation test_loop(Activation act, ThData th)
{
    int res = 0;
    act->codeh = t1;
    expect_fail = "cycle_limit exceeded";
    res = TestMainLoop(act, th);
    OOPS("cycle limit failed");
    return act->cont;
}

Activation test_tags(Activation act, ThData th)
{
    int i = 0;
    if (!(FALSE_P(HASH_F) && TRUE_P(HASH_T) && BOOLEAN_P(HASH_F) && BOOLEAN_P(HASH_T) && !BOOLEAN_P(EMPTY_LIST) && TRUE_P(EMPTY_LIST) && !FALSE_P(nn(1))))
        OOPS("booleans");
    for (i = -1111; i <= 1111; i++) {
        Value j = FIXNUM_FROM_INT(i);
        int k = INT_FROM_FIXNUM(j);
        if (!FIXNUMABLE(i) || !FIXNUM_P(j))
            OOPS("F");
        if (k != i)
            OOPS("bad fixnum conversions");
    }
    return act->cont;
}

Activation test_cons(Activation act, ThData th)
{
    Value val = UNDEF;
    Activation done = act->cont;
    int iter = 0;
    act->codeh = t1 + 1;
    act->a = one;
    act->b = two;
    th->val = UNDEF;
    th->narg = two;
    while (act != done) {
        act = (*(act->codeh->pc)) (act, th);
        CheckTypeDisjointness(th->val);
        if ((iter++) > 3) {
            OOPS("looping");
        }
    }
    val = th->val;
    assert(PAIR_P(val));
    assert(U_CAR(val) == one);
    assert(VAL(U_CDR(val)) == two);
    return act;
}

Activation test_basic_strings(Activation act, ThData th)
{
    String A = sc_strlit("Aaaaa");
    String B = sc_strlit("Aaaaa");
    if (A == B)
        OOPS("strings not unique");
    if (!STRING_P(A))
        OOPS("Aaaaa not a string");
    if (STRING_LENGTH(A) != 1)
        if (!sc_string_equal(A, B))
            OOPS("!sc_string_equal");
    SCHARS(B)[0] = 'a';
    if (sc_string_equal(A, B))
        OOPS("sc_string_equal");
    return act->cont;
}

Activation test_basic_symbols(Activation act, ThData th)
{
    char t[] = "@\000";
    Symbol s = (Symbol) ATOM("my_first_symbol");
    if (!SYMBOL_P(s))
        OOPS("my_first_symbol isn't one");
    if (VAL(s) != ATOM("my_first_symbol"))
        OOPS("my_first_symbol isn't reproducible");
    if (s != sc_symlit("my_first_symbol"))
        OOPS("sc_symlit");
    *t = 0;
    while (*t < 126) {
        if (VAL(s) == ATOM(t))
            OOPS("huh");
        (*t)++;
    }
    if (VAL(s) != ATOM("my_first_symbol"))
        OOPS("my_first_symbol isn't reproducible after a while");
    return act->cont;
}

Activation test_lists(Activation act, ThData th)
{
    Value l = list(nn(0), nn(1), ATOM("two"), EMPTY_LIST, list(one, one, one, one, _), _);
    Value a = list(l, list(ATOM("one"), nn(1), _), _);
    if (sc_list_length(l) != 5)
        OOPS("sc_list_length");
    if (sc_assq(nn(0), a) != l)
        OOPS("assq 0");
    if (sc_assq(ATOM("one"), a) != U_CAR(U_CDR(a)))
        OOPS("assq one");
    if (sc_assq(ATOM("one!"), a) != HASH_F)
        OOPS("assq one!");
    return act->cont;
}


Activation test_prim_init(Activation act, ThData th)
{
    Pair b = 0;
    sc_define_primitives(th);
    if (sc_list_length(th->global) <(10))
        OOPS("too short");
    b = sc_global_binding(th, VAL(sc_symlit("car")));
    if (!PAIR_P(b))
        OOPS("no b");
    if (!(PROC_P(U_CDR(b))))
        OOPS("no proc");
    if (((Proc) (U_CDR(b)))->entry_pt->pc != &scp_car)
        OOPS("wrong proc");
    b = sc_global_binding(th, VAL(sc_symlit("cart")));
    if (U_CDR(b) != UNDEF)
        OOPS("expected UNDEF");
    return act->cont;
}

Activation test_interp_eval_var(Activation act, ThData th)
{
    act->codeh = sc_new_CodeHandle(&scp_interp);
    act->env = EMPTY_LIST;
    act->a = ATOM("cdr");
    th->narg = FIXNUM_FROM_INT(1);
    TestMainLoop(act, th);
    if (th->val != U_CDR(sc_global_binding(th, VAL(sc_symlit("cdr")))))
        OOPS("variable");
    return act->cont;
}


static Activation interp_eval_help(Activation act, ThData th, Value in, Value expect, const char *what)
{
    act->codeh = sc_new_CodeHandle(&scp_interp);
    act->env = EMPTY_LIST;
    act->a = in;
    th->narg = FIXNUM_FROM_INT(1);
    TestMainLoop(act, th);
    if (th->val != expect)
        OOPS(what);
    return act->cont;
}

Activation test_interp_eval_num(Activation act, ThData th)
{
    Value val = FIXNUM_FROM_INT(1);
    return interp_eval_help(act, th, val, val, "fixnum");
}

Activation test_interp_eval_string(Activation act, ThData th)
{
    Value val = VAL(sc_strlit("Some string"));
    return interp_eval_help(act, th, val, val, "string");
}

Activation test_interp_eval_char(Activation act, ThData th)
{
    Value val = VAL(CHARVAL_FROM_UINT('Q'));
    return interp_eval_help(act, th, val, val, "character");
}

Activation test_interp_eval_quote(Activation act, ThData th)
{
    Value val = VAL(sc_strlit("another string"));
    Value expr = list(ATOM("quote"), val, _);
    return interp_eval_help(act, th, expr, val, "quote");
}

Activation test_interp_eval_combination(Activation act, ThData th)
{
    Value str = VAL(sc_strlit("this"));
    Value expr = list(ATOM("string->symbol"), str, _);
    return interp_eval_help(act, th, expr, ATOM("this"), "combination");
}

Activation test_interp_eval_nested_combination(Activation act, ThData th)
{
    /* (car (cdr (cdr (list 1 2 3 4 5)))) */
    Value expr = list(ATOM("car"), list(ATOM("cdr"), list(ATOM("cdr"), list(ATOM("list"), one, two, three, nn(4), nn(5), _), _), _), _);
    return interp_eval_help(act, th, expr, three, "nested combination");
}

Activation test_interp_eval_sequence(Activation act, ThData th)
{
    /** (begin (define a (quote ())) (set! a (cons 2 a)) (set! a (cons 1 a)) (length a)) **/
    Value a = ATOM("a");
    Value begin = ATOM("begin");
    Value define = ATOM("define");
    Value quote = ATOM("quote");
    Value set = ATOM("set!");
    Value cons = ATOM("cons");
    Value length = ATOM("length");
    Value expr = list(begin,
                      list(define, a, list(quote, EMPTY_LIST, _), _),
                      list(set, a, list(cons, two, a, _), _),
                      list(set, a, list(cons, one, a, _), _),
                      list(length, a, _), _);
    sc_global_binding(th, ATOM("a"))->cdr = zero;
    return interp_eval_help(act, th, expr, two, "sequence");
}

Activation test_if(Activation act, ThData th)
{
    /** (if (list? (cons 1 2)) (car 1) (if (pair? (cons 1 2)) 3)) **/
    Value If = ATOM("if");
    Value pairp = ATOM("pair?");
    Value listp = ATOM("list?");
    Value car = ATOM("car");
    Value cons = ATOM("cons");
    Value expr = list(If, list(listp, list(cons, one, two, _), _),
                      list(car, one, _),
                      list(If, list(pairp, list(cons, one, two, _), _),
                           three, _), _);
    return interp_eval_help(act, th, expr, three, "if test");
}

Activation test_lambda(Activation act, ThData th)
{
    /** ((lambda (x y) x) 1 2) **/
    Value lambda = ATOM("lambda");
    Value x = ATOM("x");
    Value y = ATOM("y");
    Value expr = list(list(lambda, list(x, y, _), x, _), one, two, _);
    return interp_eval_help(act, th, expr, one, "lambda");
}

Activation test_plus(Activation act, ThData th)
{
    Value plus = ATOM("+");
    Value expr = list(plus, one, one, _);
    return interp_eval_help(act, th, expr, two, "1 + 1 is 2");
}

Activation test_minus(Activation act, ThData th)
{
    Value minus = ATOM("-");
    Value expr = list(minus, two, one, _);
    return interp_eval_help(act, th, expr, one, "2 - 1 is 1");
}

Activation test_fib(Activation act, ThData th)
{
    /** (begin (define fib (lambda (n) (if (> n 1) (+ (fib (- n 1)) (fib (- n 2))) n))) (fib 10)) **/
    Value begin = ATOM("begin");
    Value lambda = ATOM("lambda");
    Value define = ATOM("define");
    Value fib = ATOM("fib");
    Value n = ATOM("n");
    Value If = ATOM("if");
    Value gt = ATOM(">");
    Value plus = ATOM("+");
    Value minus = ATOM("-");
    Value expr = list(begin, list(define, fib, list(lambda, list(n, _), list(If, list(gt, n, one, _), list(plus, list(fib, list(minus, n, one, _), _), list(fib, list(minus, n, two, _), _), _), n, _), _), _), list(fib, nn(10), _), _);
    cycle_limit = 99999;
    return interp_eval_help(act, th, expr, nn(55), "fib");
}

Activation test_bignum_compare(Activation act, ThData th)
{
    Value n111 = sc_new_Bignum(true, true, 0, list(nn(1), nn(1), nn(1), _));
    Value n11 = sc_new_Bignum(true, true, 0, list(nn(1), nn(1), _));
    Value c11 = sc_new_Bignum(true, false, 0, list(nn(1), nn(1), _));
    Value c101 = sc_new_Bignum(true, false, 0, list(nn(1), nn(0), nn(1), _));
    Value c102 = sc_new_Bignum(true, false, 0, list(nn(2), nn(0), nn(1), _));
    Value c100 = sc_new_Bignum(false, false, 2, list(nn(1), _));
    Value c200 = sc_new_Bignum(true, false, 2, list(nn(2), _));
    Value le = ATOM("<=");
    Value expr = list(le, n111, n111, n11, n11, c11, c100, c100, c101, c101, c102, c200, _);
    return interp_eval_help(act, th, expr, HASH_T, "<=");
}

Activation test_float_compare(Activation act, ThData th)
{
    Value gt = ATOM(">");
    Value expr = list(gt, ff(100.), ff(99.99999), ff(0.0), ff(-1.0), _);
    return interp_eval_help(act, th, expr, HASH_T, ">");
}

Activation test_basic_string(Activation act, ThData th)
{
    /** ((lambda (a b) (string-set! b 1 (string-ref a 1)) (string->symbol b)) "123" (make-string 3 #\P)) **/
    Value a = ATOM("a");
    Value b = ATOM("b");
    Value string_set = ATOM("string-set!");
    Value lambda = ATOM("lambda");
    Value string_ref = ATOM("string-ref");
    Value string2symbol = ATOM("string->symbol");
    Value make_string = ATOM("make-string");
    Value P2P = ATOM("P2P");
    Value OTT = VAL(sc_strlit("123"));
    Value P = CHARVAL_FROM_UINT('P');
    Value expr = list(
                      list(lambda, list(a, b, _),
                           list(string_set, b, one, list(string_ref, a, one, _), _), list(string2symbol, b, _), _),
                      OTT, list(make_string, three, P, _), _);
    return interp_eval_help(act, th, expr, P2P, "basic strings");
}

Activation test_exact(Activation act, ThData th)
{
    /** (if (exact? 1) (if (inexact? 1.0) (not (exact? 2.0)))) **/
    Value If = ATOM("if");
    Value exact = ATOM("exact?");
    Value inexact = ATOM("inexact?");
    Value not = ATOM("not");
    Value expr = list(If, list(exact, one, _), list(If, list(inexact, ff(1.0), _), list(not, list(exact, ff(2.0), _), _), _), _);
    return interp_eval_help(act, th, expr, HASH_T, "exactness predicates");
}

Activation test_and(Activation act, ThData th)
{
    /** (and (eq? 0 (and (and) (and not 0))) (not (and #f (cons))) 1) **/
    Value eq = ATOM("eq?");
    Value and = ATOM("and");
    Value not = ATOM("not");
    Value cons = ATOM("cons");
    Value expr = list(and, list(eq, zero, list(and, list(and, _), list(and, not, zero, _), _), _),
                      list(not, list(and, HASH_F, list(cons, _), _), _),
                      one, _);
    return interp_eval_help(act, th, expr, nn(1), "and syntax");
}

Activation test_eqv(Activation act, ThData th)
{
    /** (and (eqv? eqv? eqv?) (not (eqv? 1 1.0)) (eqv? 1.0 1.0) (eqv? (quote quote) (quote quote))) **/
    Value eqv = ATOM("eqv?");
    Value and = ATOM("and");
    Value not = ATOM("not");
    Value quote = ATOM("quote");
    Value expr = list(and,
                      list(eqv, eqv, eqv, _),
                      list(not, list(eqv, one, ff(1.0), _), _),
                      list(eqv, ff(1.0), ff(1.0), _),
             list(eqv, list(quote, quote, _), list(quote, quote, _), _), _);
    return interp_eval_help(act, th, expr, HASH_T, "eqv?");
}

Activation test_or(Activation act, ThData th)
{
    /** (and (eq? #f (or)) (or #f (or 2 1) (cons))) **/
    Value eq = ATOM("eq?");
    Value and = ATOM("and");
    Value or = ATOM("or");
    Value cons = ATOM("cons");
    Value expr = list(and, list(eq, HASH_F, list(or, _), _), list(or, HASH_F, list(or, two, one, _), list(cons, _), _), _);
    return interp_eval_help(act, th, expr, nn(2), "or syntax");
}

Activation test_char(Activation act, ThData th)
{
    /** (and (eq? (char->integer (integer->char 1097)) 1097) (char->integer (string-ref (symbol->string (quote a)) 0))) **/
    Value eq = ATOM("eq?");
    Value and = ATOM("and");
    Value a = ATOM("a");
    Value integer2char = ATOM("integer->char");
    Value char2integer = ATOM("char->integer");
    Value symbol2string = ATOM("symbol->string");
    Value string_ref = ATOM("string-ref");
    Value quote = ATOM("quote");
    Value expr = list(and,
                      list(eq, list(char2integer, list(integer2char, nn(1097), _), _), nn(1097), _),
                      list(char2integer, list(string_ref, list(symbol2string, list(quote, a, _), _), zero, _), _), _);
    return interp_eval_help(act, th, expr, nn('a'), "char/int conv");
}

Activation test_vector(Activation act, ThData th)
{
    /** ((lambda (v a) (vector-set! v 0 #t) (and (vector? v) (= 3 (vector-length v)) (eq? a (vector-ref v 2)) (vector-ref v 0))) (make-vector 3 -1) -1) **/
    Value a = ATOM("a");
    Value and = ATOM("and");
    Value eq = ATOM("eq?");
    Value eql = ATOM("=");
    Value lambda = ATOM("lambda");
    Value make_vector = ATOM("make-vector");
    Value vectorp = ATOM("vector?");
    Value vector_ref = ATOM("vector-ref");
    Value vector_length = ATOM("vector-length");
    Value vector_set = ATOM("vector-set!");
    Value v = ATOM("v");
    Value expr = list(list(lambda, list(v, a, _),
                           list(vector_set, v, zero, HASH_T, _),
                           list(and, list(vectorp, v, _),
                             list(eql, nn(3), list(vector_length, v, _), _),
                                list(eq, a, list(vector_ref, v, two, _), _),
                                list(vector_ref, v, zero, _), _), _),
                      list(make_vector, three, nn(-1), _), nn(-1), _);
    return interp_eval_help(act, th, expr, HASH_T, "vector");
}

Activation test_make_input(Activation act, ThData th)
{
    /** (begin (define x (lambda (d w) (set! w (car (cdr d))) (set-cdr! d (cdr (cdr d))) w)) (define p (make-input-port x #f (list #f #\a #\b <eof>))) (and (eq? (read-char p) #\a) (eq? (read-char p) #\b) (char-ready? p) (eof-object? (read-char p)))) **/
    Value a = cc('a');
    Value b = cc('b');
    Value p = ATOM("p");
    Value begin = ATOM("begin");
    Value define = ATOM("define");
    Value x = ATOM("x");
    Value lambda = ATOM("lambda");
    Value d = ATOM("d");
    Value w = ATOM("w");
    Value set = ATOM("set!");
    Value car = ATOM("car");
    Value cdr = ATOM("cdr");
    Value set_cdr = ATOM("set-cdr!");
    Value make_input_port = ATOM("make-input-port");
    Value List = ATOM("list");
    Value eof = ATOM("*EOF*");
    Value and = ATOM("and");
    Value eq = ATOM("eq?");
    Value readc = ATOM("read-char");
    Value char_ready = ATOM("char-ready?");
    Value eof_object = ATOM("eof-object?");
    Value expr = list(begin,
                      list(define, x,
                           list(lambda, list(d, w, _),
                             list(set, w, list(car, list(cdr, d, _), _), _),
                         list(set_cdr, d, list(cdr, list(cdr, d, _), _), _),
                                w, _), _),
                      list(define, p, list(make_input_port, x, HASH_F, list(List, HASH_F, a, b, eof, _), _), _),
                      list(and, list(eq, list(readc, p, _), a, _),
                           list(eq, list(readc, p, _), b, _),
                           list(char_ready, p, _),
                           list(eof_object, list(readc, p, _), _), _), _);
    sc_global_binding(th, eof)->cdr = EOF_OBJECT;
    return interp_eval_help(act, th, expr, HASH_T, "character input");
}

Activation test_cond(Activation act, ThData th)
{
    /** (cond (#f 1) (#t 2) (#f 3)) **/
    Value cond = ATOM("cond");
    Value expr = list(cond, list(HASH_F, one, _), list(HASH_T, two, _), list(HASH_F, three, _), _);
    return interp_eval_help(act, th, expr, two, "cond");
}

Activation test_internal_defines(Activation act, ThData th)
{
    /** (((lambda () (define a (lambda (x) (x) b)) (define b two) a)) (lambda () (set! b zero))) **/
    Value lambda = ATOM("lambda");
    Value define = ATOM("define");
    Value a = ATOM("a");
    Value b = ATOM("b");
    Value x = ATOM("x");
    Value set = ATOM("set!");
    Value expr = list(list(list(lambda, EMPTY_LIST, list(define, a, list(lambda, list(x, _), list(x, _), b, _), _), list(define, b, two, _), a, _), _), list(lambda, EMPTY_LIST, list(set, b, zero, _), _), _);
    sc_global_binding(th, b)->cdr = HASH_T;
    return interp_eval_help(act, th, expr, two, "internal def");
}

Activation test_basic_string_to_number(Activation act, ThData th)
{
    /** (and (eqv? (string->number s123456) nn123456) (not (string->number spaces)) (string->number s2)) **/
    Value s123456 = VAL(sc_strlit("123456"));
    Value nn123456 = VAL(nn(123456));
    Value s2 = VAL(sc_strlit("2"));
    Value string2number = ATOM("string->number");
    Value eqv = ATOM("eqv?");
    Value and = ATOM("and");
    Value not = ATOM("not");
    Value spaces = VAL(sc_strlit("  "));
    Value expr = list(and, list(eqv, list(string2number, s123456, _), nn123456, _), list(not, list(string2number, spaces, _), _), list(string2number, s2, _), _);
    return interp_eval_help(act, th, expr, two, "string->number (basic)");
}

Activation test_mit_define(Activation act, ThData th)
{
    /** (begin (define (fib n) (if (> n 1) (+ (fib (- n 1)) (fib (- n 2))) n)) (fib 6)) **/
    Value begin = ATOM("begin");
    Value define = ATOM("define");
    Value fib = ATOM("fib");
    Value n = ATOM("n");
    Value If = ATOM("if");
    Value gt = ATOM(">");
    Value plus = ATOM("+");
    Value minus = ATOM("-");
    Value expr = list(begin, list(define, list(fib, n, _), list(If, list(gt, n, one, _), list(plus, list(fib, list(minus, n, one, _), _), list(fib, list(minus, n, two, _), _), _), n, _), _), list(fib, nn(6), _), _);
    return interp_eval_help(act, th, expr, nn(8), "mit fib");
}

Activation test_reader_install(Activation act, ThData th)
{
    /** (define reader (lambda (port) ...)) **/
    Value define = ATOM("define");
    Value read = ATOM("%reader");
    Value readlambda = sc_bootstrap_reader_exp();
    Value expr = list(define, read, readlambda, _);
    assert(PAIR_P(readlambda));
    assert(U_CAR(U_CAR(readlambda)) == ATOM("lambda"));
    return interp_eval_help(act, th, expr, UNSPEC, "install reader");
}

Activation test_read_symbol(Activation act, ThData th)
{
    /** (read (open-input-string "  Symbol 1")) **/
    Value read = ATOM("read");
    Value openst = ATOM("open-input-string");
    Value str = VAL(sc_strlit("  symbol"));
    Value expr = list(read, list(openst, str, _), _);
    return interp_eval_help(act, th, expr, ATOM("symbol"), "read symbol");
}

static Activation eval_c_string(Activation act, ThData th, const char *s, const char *what)
{
    Value read = ATOM("read");
    Value openst = ATOM("open-input-string");
    Value str = VAL(sc_strlit(s));
    Value expr = list(read, list(openst, str, _), _);
    act->codeh = sc_new_CodeHandle(&scp_interp);
    act->env = EMPTY_LIST;
    act->a = expr;
    th->narg = FIXNUM_FROM_INT(1);
    sc_run_main_loop(act, th);
    expr = th->val;
    return interp_eval_help(act, th, expr, HASH_T, what ? what : s);
}

Activation test_eval_c_string(Activation act, ThData th)
{
    return eval_c_string(act, th, "(eqv? 10 (+ 4 6))", 0);
}

Activation test_number2string(Activation act, ThData th)
{
    return eval_c_string(act, th, "(= -42 (string->number (number->string -42)))", 0);
}

Activation test_base_addto(Activation act, ThData th)
{
    Value quote = ATOM("quote");
    Value a = list(FIXNUM_FROM_INT(1), _);
    Value b = list(FIXNUM_FROM_INT(99), FIXNUM_FROM_INT(99), _);
    Value t = a;
    int i = 0;
    short ref[] = {1, 26, 76, 50, 60, 2, 28, 22, 94, 1, 49, 67, 3, 20, 53, 76};
    sc_base_addto(th, b, a, FIXNUM_FROM_INT(100), FIXNUM_FROM_INT(0));
    assert(3 == sc_list_length(b));
    assert(FIXNUM_FROM_INT(1) == U_CAR(U_CDR(U_CDR(b))));
    while (i < 100) {
        sc_base_addto(th, a, a, FIXNUM_FROM_INT(100), FIXNUM_FROM_INT(0));
        i++;
    }
    i = sizeof(ref) / sizeof(ref[0]);
    while (i > 0) {
        i--;
        assert(PAIR_P(t));
        assert(U_CAR(t) == FIXNUM_FROM_INT(ref[i]));
        t = U_CDR(t);
    }
    return interp_eval_help(act, th, list(quote, t, _), EMPTY_LIST, "base_addto");
}


Activation test_base_expand(Activation act, ThData th)
{
    Value quote = ATOM("quote");
    Value a = list(FIXNUM_FROM_INT(123456789), _);
    int i = 0;
    short ref[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    Value t = sc_base_expand_bignum(th, FIXNUM_FROM_INT(0), a, FIXNUM_FROM_INT(10));
    i = sizeof(ref) / sizeof(ref[0]);
    while (i > 0) {
        i--;
        assert(PAIR_P(t));
        assert(U_CAR(t) == FIXNUM_FROM_INT(ref[i]));
        t = U_CDR(t);
    }
    return interp_eval_help(act, th, list(quote, t, _), EMPTY_LIST, "base_expand");
}

Activation test_write_char(Activation act, ThData th)
{
    th->outport = (PortValue) sc_stdout(th);
    return eval_c_string(act, th,
                         "((lambda (str) "
                         "(define (hello world) "
                         "(if (pair? world) "
                     "(begin (write-char (car world)) (hello (cdr world))) "
                         "#t)) "
                      "(hello (string->list str))) \"Hello World!\n\")", 0);
}

Activation test_read_stdio(Activation act, ThData th)
{
    th->inport = (PortValue) sc_stdin(th);
    return eval_c_string(act, th, "(eq? 'ok (read (open-stdio-file \"ok.txt\" \"r\")))", 0);
}

Activation test_sc_eval_c_string(Activation act, ThData th)
{
    Value sum = sc_eval_c_string(th, "(+ 1 2)");
    if (sum != FIXNUM_FROM_INT(3))
        OOPS("huh");
    return act->cont;
}

#define FIXNUM_BITS UV(CHAR_BIT*sizeof(intptr_t)-1)
#define MAX_FIX_INT IV((1UL<<(FIXNUM_BITS-1))-1)

Activation test_fixnumlist_product(Activation act, ThData th)
{
    Value a = list(nn(MAX_FIX_INT), nn(MAX_FIX_INT), _);
    Value aa = sc_fixnumlist_product(th, a, a);
    while (!NULL_P(aa)) {
        if (!PAIR_P(aa))
            OOPS("huhaa");
        if (!FIXNUM_P(U_CAR(aa)))
            OOPS("huhcar");
        printf("%ld\n", INT_FROM_FIXNUM(U_CAR(aa)));
        aa = U_CDR(aa);
    }
    return act->cont;
}

typedef Activation(*testfn) (Activation, ThData);

#define DUMMY_ENT {0,0}
static struct {
    testfn fn;
    const char *name;
} testtab[] = {
    DUMMY_ENT,
#include "test_items.h"
    DUMMY_ENT
};

int main(int argc, char **argv)
{
    volatile Activation act = 0;
    volatile ThData th = 0;
    volatile int testsrun = 0;
    volatile int ntests = (sizeof(testtab) / sizeof(testtab[0]) - 2);
    volatile int lasttest = ntests;
    int i = argv[1] ? atoi(argv[1]) : 0;
    volatile int skiptest = -1;
    sc_set_simple_stack_base(&act);
    th = sc_new_ThData();
    if (i < 0)
        skiptest = -i;
    if (0 < i && i <= ntests) {
        if (i > 7)
            sc_define_primitives(th);
        if (i > 20)
            cycle_limit = 99999;
        testnum = i - 1;
        if (i > 35)
            testnum = 34;
        lasttest = i;
    }
    setjmp(next_test);
    while (true) {
        testnum += 1;
        expect_fail = 0;
        if (testnum > lasttest || !(testtab[testnum].fn))
            break;
        fprintf(stderr, "*** Test %d (%s)\n", testnum, testtab[testnum].name);
        testsrun += 1;
        if (testnum == skiptest) {
            fprintf(stderr, "[Skipped]\n");
            continue;
        }
        th->culprit = HASH_F;
        act = sc_prime_act_pump(0, th);
        act = (*testtab[testnum].fn) (act, th);
    }
    if (isatty(2)) {
        fprintf(stderr, "%c[%dm", 033, (failures) ? 101 : 102);
    }
    fprintf(stderr, "Ran %d of %d tests, %d failed.", testsrun, ntests, failures);
    if (isatty(2)) {
        fprintf(stderr, "%c[m", 033);
    }
    fprintf(stderr, "\n");
    exit(0 != failures);
}
