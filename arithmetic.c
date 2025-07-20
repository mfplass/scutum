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
#define SC_MODULE_ID 3000
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <assert.h>
#include "scutum.h"
#include "primitives.h"

#define FIXNUM_BITS UV(CHAR_BIT*sizeof(intptr_t)-1)
#define UFIXNUM_BITS (FIXNUM_BITS-1)
#define HUFIXNUM_BITS (UFIXNUM_BITS/2)
#define MAX_FIX_INT IV((1UL<<UFIXNUM_BITS)-1)
#define MAX_SAFE_FIXNUM_TERM UV((1UL<<HUFIXNUM_BITS)-1)

#define WANT_QUOTIENT   FIXNUM_FROM_INT(1)
#define WANT_REMAINDER  FIXNUM_FROM_INT(2)
#define WANT_MODULO     FIXNUM_FROM_INT(3)

/* The following could be made into a runtime parameter */
#define SC_MAX_BIGNUM_WORDS(th) (11111)

Activation scp_exact_P(Activation act, ThData th)
{
    Value a = act->a;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    RETURN(MAKE_BOOLEAN(EXACT_P(a)));
}

Activation scp_inexact_P(Activation act, ThData th)
{
    Value a = act->a;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    RETURN(MAKE_BOOLEAN(INEXACT_P(a)));
}

bool sc_integer_p(Value a)
{
    Value zero = FIXNUM_FROM_INT(0);
    double aa;
    if (FIXNUM_P(a))
        return (true);
    switch (NUM_CODE(a)) {
    case code_bignum:
        return (((Bignum) a)->exponent >= zero);
    case code_float64:
        aa = ((Float64) a)->value;
        return (aa == nearbyint(aa));
    default:
        return (false);
    }
}

bool sc_negative_p(Value a)
{
    Value zero = FIXNUM_FROM_INT(0);
    if (FIXNUM_P(a))
        return (a < zero);
    if (RATIONAL_P(a)) {
        /* all flags are in the same spot */
        return (((((Bignum) a)->flags) & NUM_NEG) != 0);
    }
    return (false);
}

bool sc_zero_p(Value a)
{
    if (a == FIXNUM_FROM_INT(0))
        return true;
    if (NUM_CODE(a) == code_float64 && ((Float64) a)->value == 0.0)
        return true;
    return false;
}

Activation scp_integer_P(Activation act, ThData th)
{
    Value a = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(sc_integer_p(a)));
}

Activation scp_rational_P(Activation act, ThData th)
{
    Value a = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(RATIONAL_P(a)));
}

Activation scp_PLUS(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    uintptr_t n = INT_FROM_FIXNUM(th->narg);
    Value a = act->a;
    Value b = act->b;
    Value r = act->r;
    while (n >= 2) {
        if (FIXNUMS_P(a, b)) {
            Int ans = INT_FROM_FIXNUM(a) + INT_FROM_FIXNUM(b);
            if (FIXNUMABLE(ans)) {
                if (n == 2) {
                    RETURN(FIXNUM_FROM_INT(ans));
                }
                a = FIXNUM_FROM_INT(ans);
                b = U_CAR(r);
                r = U_CDR(r);
                n -= 1;
                continue;
            }
        }
        return sc_sum(act, th, a, b, r);
    }
    if (n) {
        DEMAND(NUMBER_P(a), a);
        RETURN(a);
    }
    RETURN(zero);
}

Activation sc_sum(Activation act, ThData th, Value a, Value b, Value r)
{
    DEMAND(NUMBER_P(a), a);
    while (1) {
        DEMAND(NUMBER_P(b), b);
        a = sc_sum2(th, a, b, false);
        if (NULL_P(r))
            RETURN(a);
        b = U_CAR(r);
        r = U_CDR(r);
    }
}

Value sc_sum2(ThData th, Value a, Value b, bool bnegate)
{
    Value zero = FIXNUM_FROM_INT(0);
    enum type_code ac, bc, cc;
    if (a == zero && !bnegate)
        return b;
    if (b == zero)
        return a;
    if (FIXNUMS_P(a, b)) {
        Int ans = INT_FROM_FIXNUM(b);
        if (bnegate)
            ans = -ans;
        ans += INT_FROM_FIXNUM(a);
        if (FIXNUMABLE(ans))
            return (FIXNUM_FROM_INT(ans));
        return sc_bignum_from_int(th, ans);
    }
    ac = NUM_CODE(a);
    bc = NUM_CODE(b);
    cc = (ac > bc) ? ac : bc;
    if (cc <= code_rational && !(EXACT_P(a) && EXACT_P(b)))
        return sc_float64_sum2(th, a, b, bnegate);
    switch (cc) {
    case code_float64:
        return sc_float64_sum2(th, a, b, bnegate);
    case code_bignum:
        a = sc_promote(th, a, cc);
        b = sc_promote(th, b, cc);
        if (NUM_CODE(a) == cc && NUM_CODE(b) == cc)
            return sc_bignum_sum2(th, (Bignum) a, (Bignum) b, bnegate);
        abort();                /* bug */
    case code_rational:
        return sc_rational_sum2(th, a, b, bnegate);
    case code_complex:
        return sc_complex_sum2(th, a, b, bnegate);
    default:
        return NYI;
    }
}

Value sc_float64_sum2(ThData th, Value a, Value b, bool bnegate)
{
    double af = sc_floval(th, a, 0);
    double bf = sc_floval(th, b, 0);
    double ans = (bnegate) ? (af - bf) : (af + bf);
    /* Could check (!(isfinite(ans)) && (isfinite(af)) && (isfinite(bf))) */
    return sc_new_Float64(false, ans);
}

Value sc_bignum_sum2(ThData th, Bignum a, Bignum b, bool bnegate)
{
    Value zero = FIXNUM_FROM_INT(0);
    Int sx = 0;
    Value sh = EMPTY_LIST;
    Value st = EMPTY_LIST;
    Value nw = EMPTY_LIST;
    Int cy = 0;
    Int bflip = 0;
    Value af = a->flags;
    Value bf = b->flags;
    bool exact = !((af | bf) & NUM_INEXACT);
    Value ax = a->exponent;
    Value bx = b->exponent;
    Value am = a->mag;
    Value bm = b->mag;
    if (!(FIXNUM_P(ax) && FIXNUM_P(bx)))
        return NYI;
    if (bnegate)
        bf ^= NUM_NEG;
    if ((af ^ bf) & NUM_NEG) {
        /* opposite signs - need to subtract magnitudes */
        unsigned leg = sc_compare2_bignums(NUM_FLAG, ax, am, NUM_FLAG, bx, bm);
        if (leg == 2 && exact)
            return zero;
        if (leg == 4) {
            /* a is smaller magnitude - need to swap */
            Value t;
            (t = ax, ax = bx, bx = t);
            (t = af, af = bf, bf = t);
            (t = am, am = bm, bm = t);
        }
        cy = 1;
        bflip = MAX_FIX_INT;
    }
    ax = INT_FROM_FIXNUM(ax);
    bx = INT_FROM_FIXNUM(bx);
    if (ax > bx) {
        sx = bx;
        ax = ax - bx;
        bx = 0;
    }
    else {
        sx = ax;
        bx = bx - ax;
        ax = 0;
    }
    if (ax + bx >= SC_MAX_BIGNUM_WORDS(th))
        return sc_float64_sum2(th, VAL(a), VAL(b), bnegate);
    while (cy || PAIR_P(am) || PAIR_P(bm) || bflip) {
        Int ad = 0;
        Int bd = bflip;
        if (ax) {
            ax--;
        }
        else if (PAIR_P(am)) {
            ad = INT_FROM_FIXNUM(U_CAR(am));
            am = U_CDR(am);
        }
        if (bx) {
            bx--;
        }
        else if (PAIR_P(bm)) {
            bd ^= INT_FROM_FIXNUM(U_CAR(bm));
            bm = U_CDR(bm);
        }
        else if (bflip && ad + cy > 0) {
            bd = -1;
            bflip = 0;
        }
        ad = ad + bd + cy;
        if (ad < 0)
            return NYI;         /* bug */
        cy = ad >> UFIXNUM_BITS;
        ad &= MAX_FIX_INT;
        if (0 == ad && NULL_P(sh)) {
            sx += 1;
        }
        else {
            nw = CONS(FIXNUM_FROM_INT(ad), EMPTY_LIST);
            if (PAIR_P(st)) {
                ((Pair) st)->cdr = nw;
            }
            else {
                sh = nw;
            }
            st = nw;
        }
    }
    sh = sc_fixnumlist_trim(th, sh);
    return sc_make_bignum(th, exact, 0 != (af & NUM_NEG), sx, sh);
}

Value sc_rational_sum2(ThData th, Value a, Value b, bool bnegate)
{
    Value an = sc_numerator(th, a);
    Value ad = sc_denominator(th, a);
    Value bn = sc_numerator(th, b);
    Value bd = sc_denominator(th, b);
    Value numer = sc_sum2(th, sc_product2(th, an, bd), sc_product2(th, ad, bn), bnegate);
    Value denom = sc_product2(th, ad, bd);
    return sc_make_rational(th, numer, denom);
}

Activation scp_numerator(Activation act, ThData th)
{
    Value a = act->a;
    Value ans = a;
    DEMAND(RATIONAL_P(a), a);
    ans = sc_numerator(th, a);
    if (INEXACT_P(a))
        ans = sc_exact_to_inexact(th, ans);
    RETURN(ans);
}

Activation scp_denominator(Activation act, ThData th)
{
    Value a = act->a;
    Value ans = a;
    DEMAND(RATIONAL_P(a), a);
    ans = sc_denominator(th, a);
    if (INEXACT_P(a))
        ans = sc_exact_to_inexact(th, ans);
    RETURN(ans);
}

Value sc_numerator(ThData th, Value a)
{
    a = sc_inexact_to_exact(th, a);
    if (sc_integer_p(a))
        return a;
    if (NUM_CODE(a) == code_rational)
        return ((Rational) a)->numerator;
    return NYI;
}

Value sc_denominator(ThData th, Value a)
{
    if (sc_integer_p(a))
        return FIXNUM_FROM_INT(1);
    a = sc_inexact_to_exact(th, a);
    if (NUM_CODE(a) == code_rational)
        return ((Rational) a)->denominator;
    return NYI;
}

Value sc_complex_sum2(ThData th, Value a, Value b, bool bnegate)
{
    Value re_a = sc_real_part(th, a);
    Value im_a = sc_imag_part(th, a);
    Value re_b = sc_real_part(th, b);
    Value im_b = sc_imag_part(th, b);
    Value re = sc_sum2(th, re_a, re_b, bnegate);
    Value im = sc_sum2(th, im_a, im_b, bnegate);
    Value ans = sc_make_complex(th, re, im);
    return ans;
}

Activation scp_make__rectangular(Activation act, ThData th)
{
    Value a = act->a;
    Value b = act->b;
    Value ans = a;
    DEMAND_ARGS(2);
    DEMAND(RATIONAL_P(a), a);
    DEMAND(RATIONAL_P(b), b);
    ans = sc_make_complex(th, a, b);
    RETURN(ans);
}

Activation scp_real__part(Activation act, ThData th)
{
    Value a = act->a;
    Value ans = a;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    ans = sc_real_part(th, a);
    RETURN(ans);
}

Activation scp_imag__part(Activation act, ThData th)
{
    Value a = act->a;
    Value ans = a;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    ans = sc_imag_part(th, a);
    RETURN(ans);
}

Value sc_make_complex(ThData th, Value realpart, Value imagpart)
{
    if (sc_zero_p(imagpart)) {
        if (INEXACT_P(imagpart))
            return sc_exact_to_inexact(th, realpart);
        return realpart;
    }
    return sc_new_complex(th, realpart, imagpart);
}

/* This is only used in a couple of routines, and eventually should go away */
Value sc_new_complex(ThData th, Value realpart, Value imagpart)
{
    if (INEXACT_P(realpart) || INEXACT_P(imagpart))
        return sc_new_Complex(false,
                              sc_exact_to_inexact(th, realpart),
                              sc_exact_to_inexact(th, imagpart));
    return sc_new_Complex(true, realpart, imagpart);
}

Value sc_real_part(ThData th, Value z)
{
    (void) th;
    switch (NUM_CODE(z)) {
    case code_complex:
        return ((Complex) z)->real_part;
    default:
        return z;
    }
}

Value sc_imag_part(ThData th, Value z)
{
    (void) th;
    switch (NUM_CODE(z)) {
    case code_complex:
        return ((Complex) z)->imag_part;
    default:
        return FIXNUM_FROM_INT(0);
        /* is it OK to always be exact here? */
    }
}

Value sc_bignum_from_int(ThData th, Int i)
{
    bool neg = 0;
    uintptr_t lo, hi;
    Value mag = EMPTY_LIST;
    int e = 0;
    if (i < 0) {
        neg = 1;
        i = -i;
    }
    lo = i & MAX_FIX_INT;
    hi = UV(i) >> UFIXNUM_BITS;
    if (hi) {
        mag = (Value) CONS(FIXNUM_FROM_INT(hi), mag);
    }
    if (lo) {
        mag = (Value) CONS(FIXNUM_FROM_INT(lo), mag);
    }
    else if (hi) {
        e += 1;
    }
    return sc_new_Bignum(1, neg, e, mag);
}

Value sc_make_bignum(ThData th, bool exact, bool neg, Int exponent, Value mag)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value one = FIXNUM_FROM_INT(1);
    while (PAIR_P(mag) && U_CAR(mag) == zero) {
        mag = U_CDR(mag);
        exponent++;
    }
    if (exact) {
        if (NULL_P(mag))
            return (zero);
        if (exponent == 0 && NULL_P(U_CDR(mag))) {
            Value m = U_CAR(mag);
            if (neg) {
                m = FIXNUM_FROM_INT(-INT_FROM_FIXNUM(m));
            }
            return (m);
        }
        /* oddball most-negative fixnum case */
        if (neg && exponent == 1 && NULL_P(U_CDR(mag)) && U_CAR(mag) == one) {
            return (FIXNUM_FROM_INT(-1 - MAX_FIX_INT));
        }
        if (exponent < 0) {
            return sc_make_rational(th,
                                    sc_make_bignum(th, exact, neg, 0, mag),
                                    sc_make_bignum(th, exact, false, -exponent, CONS(one, EMPTY_LIST)));
        }
    }
    return sc_new_Bignum(exact, neg, exponent, mag);
}

void sc_pi_MINUS(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%-");
}

Activation scp_MINUS(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = act->a;
    Value b = act->b;
    Proc p = 0;
    if (ARGS_N(1)) {
        act->b = b = act->a;
        act->a = a = zero;
        th->narg = FIXNUM_FROM_INT(2);
    }
    if (ARGS_N(2)) {
        if (FIXNUMS_P(a, b)) {
            Int ans = INT_FROM_FIXNUM(a) - INT_FROM_FIXNUM(b);
            if (FIXNUMABLE(ans)) {
                RETURN(FIXNUM_FROM_INT(ans));
            }
        }
        DEMAND(NUMBER_P(a), a);
        DEMAND(NUMBER_P(b), b);
        RETURN(sc_sum2(th, a, b, 1));
    }
    p = (Proc) U_CDR(act->codeh->i);
    DEMAND(PROC_P(p), act->codeh->i);
    act->env = p->env;
    act->codeh = p->entry_pt;
    return act;
}

void sc_pi_STAR(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%*");
}

Activation scp_STAR(Activation act, ThData th)
{
    Value one = FIXNUM_FROM_INT(1);
    Value a = act->a;
    Value b = act->b;
    Proc p = 0;
    if (ARGS_N(0)) {
        RETURN(one);
    }
    if (ARGS_N(1)) {
        DEMAND(NUMBER_P(a), a);
        RETURN(a);
    }
    if (ARGS_N(2)) {
        RETURN(sc_product2(th, a, b));
    }
    p = (Proc) U_CDR(act->codeh->i);
    DEMAND(PROC_P(p), act->codeh->i);
    act->env = p->env;
    act->codeh = p->entry_pt;
    return act;
}

Value sc_product2(ThData th, Value a, Value b)
{
    Value one = FIXNUM_FROM_INT(1);
    enum type_code ac, bc, cc;
    if (a == one)
        return b;
    if (b == one)
        return a;
    if (FIXNUMS_P(a, b)) {
        Int aa = INT_FROM_FIXNUM(a);
        Int bb = INT_FROM_FIXNUM(b);
        Int prod = aa * bb;
        if (UV(aa | bb) <= MAX_SAFE_FIXNUM_TERM ||
            (((double) aa) * ((double) bb) == (double) prod && FIXNUMABLE(prod))) {
            return FIXNUM_FROM_INT(prod);
        }
        a = sc_promote(th, a, code_bignum);
    }
    ac = NUM_CODE(a);
    bc = NUM_CODE(b);
    cc = (ac > bc) ? ac : bc;
    if (cc <= code_rational && !(EXACT_P(a) && EXACT_P(b)))
        return sc_float64_product2(th, a, b);
    switch (cc) {
    case code_float64:
        return sc_float64_product2(th, a, b);
    case code_bignum:
        a = sc_promote(th, a, cc);
        b = sc_promote(th, b, cc);
        if (NUM_CODE(a) == cc && NUM_CODE(b) == cc)
            return sc_bignum_product2(th, (Bignum) a, (Bignum) b);
        abort();                /* bug */
    case code_rational:
        return sc_rational_product2(th, a, b);
    case code_complex:
        return sc_complex_product2(th, a, b);
    default:
        return NYI;
    }
}

Value sc_float64_product2(ThData th, Value a, Value b)
{
    double af = sc_floval(th, a, 0);
    double bf = sc_floval(th, b, 0);
    double ans = af * bf;
    /* Could check  (!(isfinite(ans)) && (isfinite(af)) && (isfinite(bf)))  */
    return sc_new_Float64(false, ans);
}

Value sc_bignum_product2(ThData th, Bignum a, Bignum b)
{
    bool neg = 0 != (((a->flags) ^ (b->flags)) & NUM_NEG);
    bool exact = 0 == (((a->flags) | (b->flags)) & NUM_INEXACT);
    Int exponent = INT_FROM_FIXNUM(a->exponent) + INT_FROM_FIXNUM(b->exponent);
    return sc_make_bignum(th, exact, neg, exponent, sc_fixnumlist_product(th, a->mag, b->mag));
}

Value sc_rational_product2(ThData th, Value a, Value b)
{
    Value an = sc_numerator(th, a);
    Value ad = sc_denominator(th, a);
    Value bn = sc_numerator(th, b);
    Value bd = sc_denominator(th, b);
    Value numer = sc_product2(th, an, bn);
    Value denom = sc_product2(th, ad, bd);
    return sc_make_rational(th, numer, denom);
}

Value sc_complex_product2(ThData th, Value a, Value b)
{
    Value re_a = sc_real_part(th, a);
    Value im_a = sc_imag_part(th, a);
    Value re_b = sc_real_part(th, b);
    Value im_b = sc_imag_part(th, b);
    Value re_re = sc_product2(th, re_a, re_b);
    Value re_im = sc_product2(th, re_a, im_b);
    Value im_re = sc_product2(th, im_a, re_b);
    Value im_im = sc_product2(th, im_a, im_b);
    Value re = sc_sum2(th, re_re, im_im, true);
    Value im = sc_sum2(th, re_im, im_re, false);
    Value ans = sc_make_complex(th, re, im);
    return ans;
}

void sc_pi_SLASH(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%/");
}

Activation scp_SLASH(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value one = FIXNUM_FROM_INT(1);
    Value a = act->a;
    Value b = act->b;
    Proc p = 0;
    if (ARGS_N(1)) {
        act->b = b = act->a;
        act->a = a = one;
        th->narg = FIXNUM_FROM_INT(2);
    }
    if (ARGS_N(2)) {
        if (FIXNUMS_P(a, b) && b != zero) {
            Int aa = INT_FROM_FIXNUM(a);
            Int bb = INT_FROM_FIXNUM(b);
            Int ans = aa / bb;
            if (FIXNUMABLE(ans) && ans * bb == aa) {
                RETURN(FIXNUM_FROM_INT(ans));
            }
        }
        RETURN(sc_divide2(th, a, b));
    }
    p = (Proc) U_CDR(act->codeh->i);
    DEMAND(PROC_P(p), act->codeh->i);
    act->env = p->env;
    act->codeh = p->entry_pt;
    return act;
}

Value sc_divide2(ThData th, Value a, Value b)
{
    enum type_code ac, bc, cc;
    Value zero = FIXNUM_FROM_INT(0);
    Value one = FIXNUM_FROM_INT(1);
    if (b == one)
        return a;
    if (FIXNUMS_P(a, b) && b != zero) {
        Int aa = INT_FROM_FIXNUM(a);
        Int bb = INT_FROM_FIXNUM(b);
        if (bb > 0 && (aa % bb == 0))
            return FIXNUM_FROM_INT(aa / bb);
        return sc_make_rational(th, a, b);
    }
    ac = NUM_CODE(a);
    bc = NUM_CODE(b);
    cc = (ac > bc) ? ac : bc;
    if (cc <= code_rational && (INEXACT_P(a) || INEXACT_P(b) || sc_zero_p(b)))
        return sc_float64_divide2(th, a, b);
    switch (cc) {
    case code_bignum:
        return sc_make_rational(th, a, b);
    case code_rational:
        return sc_rational_divide2(th, a, b);
    case code_complex:
        return sc_complex_divide2(th, a, b);
    default:
        return NYI;
    }
}

Value sc_float64_divide2(ThData th, Value a, Value b)
{
    double af = sc_floval(th, a, 0);
    double bf = sc_floval(th, b, 0);
    double ans = (af / bf);
    /*
     * Could check (!(isfinite(ans)) && (isfinite(af)) && (isfinite(bf)) && bf
     * != 0.0)
     */
    return sc_new_Float64(0, ans);
}

Value sc_rational_divide2(ThData th, Value a, Value b)
{
    Value an = sc_numerator(th, a);
    Value ad = sc_denominator(th, a);
    Value bn = sc_numerator(th, b);
    Value bd = sc_denominator(th, b);
    Value numer = sc_product2(th, an, bd);
    Value denom = sc_product2(th, ad, bn);
    Value ans = sc_make_rational(th, numer, denom);
    return ans;
}

Value sc_complex_divide2(ThData th, Value a, Value b)
{
    Value re_a = sc_real_part(th, a);
    Value im_a = sc_imag_part(th, a);
    Value re_b = sc_real_part(th, b);
    Value im_b = sc_imag_part(th, b);
    Value re_re = sc_product2(th, re_a, re_b);
    Value re_im = sc_product2(th, re_a, im_b);
    Value im_re = sc_product2(th, im_a, re_b);
    Value im_im = sc_product2(th, im_a, im_b);
    Value r2_b = sc_product2(th, re_b, re_b);
    Value i2_b = sc_product2(th, im_b, im_b);
    Value m2_b = sc_sum2(th, r2_b, i2_b, false);
    Value re_z = sc_sum2(th, re_re, im_im, false);
    Value im_z = sc_sum2(th, im_re, re_im, true);
    Value re = sc_divide2(th, re_z, m2_b);
    Value im = sc_divide2(th, im_z, m2_b);
    Value ans = sc_make_complex(th, re, im);
    return ans;
}

Activation scp_quotient(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = act->a;
    Value b = act->b;
    Value ans = NYI;
    DEMAND_ARGS(2);
    DEMAND(sc_integer_p(a), a);
    DEMAND(sc_integer_p(b), b);
    if (FIXNUMS_P(a, b) && a >= zero && b > zero) {
        /* We use general case for negatives, due to latitude in c language */
        uintptr_t aa = INT_FROM_FIXNUM(a);
        uintptr_t bb = INT_FROM_FIXNUM(b);
        ans = FIXNUM_FROM_INT(aa / bb);
    }
    else
        ans = sc_big_quorem(th, a, b, WANT_QUOTIENT);
    RETURN(ans);
}

Activation scp_remainder(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = act->a;
    Value b = act->b;
    Value ans = NYI;
    DEMAND_ARGS(2);
    DEMAND(sc_integer_p(a), a);
    DEMAND(sc_integer_p(b), b);
    if (FIXNUMS_P(a, b) && a > 0 && b > zero) {
        /* We use general case for negatives, due to latitude in c language */
        uintptr_t aa = INT_FROM_FIXNUM(a);
        uintptr_t bb = INT_FROM_FIXNUM(b);
        ans = FIXNUM_FROM_INT(aa % bb);
    }
    else
        ans = sc_big_quorem(th, a, b, WANT_REMAINDER);
    RETURN(ans);
}

Activation scp_modulo(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = act->a;
    Value b = act->b;
    Value ans = NYI;
    DEMAND_ARGS(2);
    DEMAND(sc_integer_p(a), a);
    DEMAND(sc_integer_p(b), b);
    if (false && FIXNUMS_P(a, b) && b != zero) {
        Int aa = INT_FROM_FIXNUM(a);
        Int bb = INT_FROM_FIXNUM(b);
        Int rr = aa % bb;
        if (rr) {
            if ((rr > 0) != (bb > 0))
                rr += bb;
        }
        ans = FIXNUM_FROM_INT(rr);
    }
    else {
        ans = sc_big_quorem(th, a, b, WANT_MODULO);
    }
    RETURN(ans);
}

Value sc_negate(ThData th, Value a)
{
    Value zero = FIXNUM_FROM_INT(0);
    return sc_sum2(th, zero, a, true);
}

Value sc_big_quorem(ThData th, Value dividend, Value divisor, Value want)
{
    /* Assumes integers. */
    Value zero = FIXNUM_FROM_INT(0);
    bool negtop = sc_negative_p(dividend);
    bool negbot = sc_negative_p(divisor);
    Value quotient = zero;
    Value remainder = negtop ? sc_negate(th, dividend) : dividend;
    Value orig_divisor = divisor;
    /* dn is chosen to keep flonums in sc_approx_quo happy */
    Int dn = sc_bigdigits(th, divisor) - 1;
    Int limit = 12 * (sc_bigdigits(th, remainder) - dn + 1);
    if (negbot)
        divisor = sc_negate(th, divisor);
    while (sc_compare2(th, remainder, divisor) & 3) {
        Int n = sc_bigdigits(th, remainder) - dn;
        while (n >= 0) {
            FixnumValue digit = sc_approx_quo(th, remainder, -n - dn, divisor, -dn);
            /* We do not assume that digit is completely accurate. */
            Int adjust = 1;
            while (digit != zero) {
                Value dbn = sc_bigshift(th, digit, n);
                Value pp = sc_product2(th, dbn, divisor);
                Value newrem = sc_sum2(th, remainder, pp, true);
                if (sc_negative_p(newrem)) {
                    Int d = INT_FROM_FIXNUM(digit);
                    if (d > adjust) {
                        d -= adjust;
                        adjust <<= 1;
                    }
                    else {
                        d -= 1;
                    }
                    digit = FIXNUM_FROM_INT(d);
                }
                else {
                    remainder = newrem;
                    if (want == WANT_QUOTIENT)
                        quotient = sc_sum2(th, quotient, dbn, false);
                    if (digit == FIXNUM_FROM_INT(MAX_FIX_INT)) {
                        n = 0;
                    }
                    digit = zero;
                }
                if (--limit < 0)
                    return NYI;
            }
            n = n - 1;
        }
    }
    if (want == WANT_QUOTIENT) {
        if (negtop != negbot) {
            quotient = sc_negate(th, quotient);
        }
        if (INEXACT_P(orig_divisor) || INEXACT_P(dividend)) {
            quotient = sc_exact_to_inexact(th, quotient);
        }
        return quotient;
    }
    if (negtop)
        remainder = sc_negate(th, remainder);
    if (want == WANT_MODULO &&
        (negtop != negbot) && (sc_compare2(th, remainder, zero) != 2)) {
        remainder = sc_sum2(th, remainder, orig_divisor, false);
    }
    if (INEXACT_P(orig_divisor) || INEXACT_P(dividend)) {
        remainder = sc_exact_to_inexact(th, remainder);
    }
    return remainder;
}

Value sc_bigshift(ThData th, Value digit, Int exponent)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value one = FIXNUM_FROM_INT(1);
    if (exponent == 0)
        return digit;
    if (FIXNUM_P(digit)) {
        Value d = digit;
        if (d < zero) {
            d = zero - (d - zero);
            if (d == digit) {
                exponent++;
                d = one;
            }
        }
        return sc_make_bignum(th, 1, digit < 0, exponent, CONS(d, EMPTY_LIST));
    }
    return NYI;
}

Activation scp_test__approx__quo(Activation act, ThData th)
{
    Value a = act->a;
    Value b = act->b;
    Value r = act->r;
    Value c, d;
    DEMAND_ARGS(4);
    c = U_CAR(r);
    r = U_CDR(r);
    d = U_CAR(r);
    DEMAND(FIXNUMS_P(b, d), b);
    RETURN(sc_approx_quo(th, a, INT_FROM_FIXNUM(b), c, INT_FROM_FIXNUM(d)));
}

FixnumValue sc_approx_quo(ThData th, Value a, Int ashift, Value b, Int bshift)
{
    double aa = sc_floval(th, a, ashift);
    double bb = sc_floval(th, b, bshift);
    double lim = (MAX_FIX_INT + 1);
    double q = aa / bb;
    Int ans = 0;
    if (q >= lim)
        return FIXNUM_FROM_INT(MAX_FIX_INT);
    if (q <= -lim)
        return FIXNUM_FROM_INT(-MAX_FIX_INT);
    ans = (Int) q;
    return (FIXNUM_FROM_INT(ans));
}

Activation scp_odd_P(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = act->a;
    DEMAND_ARGS(1);
    if (FIXNUM_P(a))
        RETURN(MAKE_BOOLEAN(INT_FROM_FIXNUM(a) & 1));
    DEMAND(sc_integer_p(a), a);
    a = sc_promote(th, a, code_bignum);
    if (NUM_CODE(a) == code_bignum) {
        Bignum aa = (Bignum) a;
        if (aa->exponent == zero
            && PAIR_P(aa->mag)
            && (INT_FROM_FIXNUM(U_CAR(aa->mag)) & 1))
            RETURN(HASH_T);
    }
    RETURN(HASH_F);
}

Activation scp_exact_TO_inexact(Activation act, ThData th)
{
    Value a = act->a;
    Value ans = a;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    ans = sc_exact_to_inexact(th, a);
    RETURN(ans);
}

Value sc_exact_to_inexact(ThData th, Value a)
{
    Value ans = a;
    if (EXACT_P(a)) {
        if (RATIONAL_P(a)) {
            ans = sc_new_Float64(0, sc_floval(th, a, 0));
        }
        else if (NUM_CODE(a) == code_complex) {
            Complex aa = ((Complex) a);
            Value re = sc_exact_to_inexact(th, aa->real_part);
            Value im = sc_exact_to_inexact(th, aa->imag_part);
            ans = sc_make_complex(th, re, im);
        }
    }
    return ans;
}

Activation scp_inexact_TO_exact(Activation act, ThData th)
{
    Value a = act->a;
    Value ans = a;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    ans = sc_inexact_to_exact(th, a);
    RETURN(ans);
}

Value sc_inexact_to_exact(ThData th, Value a)
{
    Value ans = a;
    if (INEXACT_P(a)) {
        switch (NUM_CODE(a)) {
        case code_float64:
            a = sc_promote(th, a, code_bignum);
            if (NUM_CODE(a) != code_bignum)
                return NYI;
            /* FALLTHRU */
        case code_bignum:
            {
                Bignum aa = (Bignum) a;
                Int exponent = INT_FROM_FIXNUM(aa->exponent);
                ans = sc_make_bignum(th, true, 0 != (NUM_NEG & (aa->flags)), exponent, aa->mag);
            }
            break;
        case code_rational:
            {
                ans = sc_make_rational(th, sc_numerator(th, a), sc_denominator(th, a));
            }
            break;
        case code_complex:
            {
                Complex aa = (Complex) a;
                ans = sc_make_complex(th, sc_inexact_to_exact(th, aa->real_part), sc_inexact_to_exact(th, aa->imag_part));
            }
            break;
        default:
            break;
        }
    }
    return ans;
}

double sc_floval(ThData th, Value a, Int shiftamt)
{
    double base = ((double) (MAX_FIX_INT + 1));
    double ans = 0.0;
    switch (NUM_CODE(a)) {
    case code_fixnum:
        ans = INT_FROM_FIXNUM(a);
        break;
    case code_float64:
        ans = ((Float64) a)->value;
        break;
    case code_bignum:
        {
            Bignum aa = (Bignum) a;
            Value mag = aa->mag;
            Int n = sc_list_length(mag);
            shiftamt += INT_FROM_FIXNUM(aa->exponent) - 1;
            while (n > 3) {
                n--;
                shiftamt++;
                mag = U_CDR(mag);
            }
            while (PAIR_P(mag)) {
                ans /= base;
                ans += (double) INT_FROM_FIXNUM(U_CAR(mag));
                shiftamt++;
                mag = U_CDR(mag);
            }
            if (aa->flags & NUM_NEG)
                ans = -ans;
        }
        break;
    case code_rational:
        {
            Value numer = sc_numerator(th, a);
            Value denom = sc_denominator(th, a);
            Int k1 = sc_bigdigits(th, numer);
            Int k2 = sc_bigdigits(th, denom);
            double aa = sc_floval(th, sc_numerator(th, a), 1 - k1);
            double bb = sc_floval(th, sc_denominator(th, a), 1 - k2);
            ans = aa / bb;
            shiftamt += (k1 - k2);
        }
        break;
    default:
        return (0.0 / 0.0);
    }
    while (shiftamt > 0) {
        ans *= base;
        shiftamt--;
    }
    while (shiftamt < 0) {
        ans /= base;
        shiftamt++;
    }
    return ans;
}

Int sc_bigdigits(ThData th, Value k)
{
    Value zero = FIXNUM_FROM_INT(0);
    if (FIXNUM_P(k))
        return ((k != zero) + (k == FIXNUM_FROM_INT(MAX_FIX_INT)));
    k = sc_promote(th, k, code_bignum);
    if (NUM_CODE(k) == code_bignum) {
        Bignum kk = (Bignum) k;
        return (sc_list_length(kk->mag) + INT_FROM_FIXNUM(kk->exponent));
    }
    return 0;                   /* should not happen */
}

Value sc_make_rational(ThData th, Value num, Value denom)
{
    Value one = FIXNUM_FROM_INT(1);
    Value gcd = sc_gcd2(th, num, denom);
    if (gcd != one) {
        num = sc_big_quorem(th, num, gcd, WANT_QUOTIENT);
        denom = sc_big_quorem(th, denom, gcd, WANT_QUOTIENT);
    }
    if (sc_negative_p(denom)) {
        num = sc_product2(th, FIXNUM_FROM_INT(-1), num);
        denom = sc_product2(th, FIXNUM_FROM_INT(-1), denom);
    }
    if (denom == one)
        return num;
    return sc_new_Rational(sc_negative_p(num), num, denom);
}

Value sc_gcd2(ThData th, Value a, Value b)
{
    if (sc_negative_p(a))
        a = sc_negate(th, a);
    if (sc_negative_p(b))
        b = sc_negate(th, b);
    while (!(sc_zero_p(b))) {
        Value c = sc_big_quorem(th, a, b, WANT_REMAINDER);
        a = b;
        b = c;
    }
    return a;
}

#define NUMBER_COMPARE(OP,LEG) { \
  Value a = act->a; \
  Value b = act->b; \
  if (ARGS_N(2) && FIXNUMS_P(a, b)) { RETURN(MAKE_BOOLEAN(a OP b)); } \
  return sc_number_compare(act, th, LEG); }

Activation scp_LT(Activation act, ThData th)
{
    NUMBER_COMPARE(<, 4);
}

Activation scp_EQ(Activation act, ThData th)
{
    NUMBER_COMPARE(==, 2);
}

Activation scp_GT(Activation act, ThData th)
{
    NUMBER_COMPARE(>, 1);
}

Activation scp_LT_EQ(Activation act, ThData th)
{
    NUMBER_COMPARE(<=, 6);
}

Activation scp_GT_EQ(Activation act, ThData th)
{
    NUMBER_COMPARE(>=, 3);
}

unsigned sc_compare2_bignums(Value af, Value ax, Value am, Value bf, Value bx, Value bm)
{
    unsigned less = ((af & NUM_NEG) ? 1 : 4);
    unsigned greater = 5 - less;
    unsigned equal = 2;
    unsigned ans = equal;
    if ((af ^ bf) & NUM_NEG) {
        return ((af & NUM_NEG) ? 4 : 1);
    }
    if (!(FIXNUM_P(ax) & FIXNUM_P(bx)))
        return 0;
    ax = INT_FROM_FIXNUM(ax);
    bx = INT_FROM_FIXNUM(bx);
    if (ax > bx) {
        ax = ax - bx;
        bx = 0;
    }
    else {
        bx = bx - ax;
        ax = 0;
    }
    while (PAIR_P(am) && PAIR_P(bm)) {
        Int ad = 0;
        Int bd = 0;
        if (ax) {
            ax--;
        }
        else {
            ad = INT_FROM_FIXNUM(U_CAR(am));
            am = U_CDR(am);
        }
        if (bx) {
            bx--;
        }
        else {
            bd = INT_FROM_FIXNUM(U_CAR(bm));
            bm = U_CDR(bm);
        }
        if (ad < bd)
            ans = less;
        if (ad > bd)
            ans = greater;
    }
    if (PAIR_P(am))
        return greater;
    if (PAIR_P(bm))
        return less;
    return ans;
}

unsigned sc_compare2(ThData th, Value a, Value b)
{
    unsigned cmp = 0;
    enum type_code ac, bc, cc;
    if (FIXNUMS_P(a, b)) {
        cmp = ((a == b) ? 2 : (a < b) ? 4 : 1);
        return cmp;
    }
    ac = NUM_CODE(a);
    bc = NUM_CODE(b);
    cc = (ac > bc) ? ac : bc;
    switch (cc) {
    case code_float64:
        {
            /* Minor imperfection here if wordsize is 64 bits. */
            double af = sc_floval(th, a, 0);
            double bf = sc_floval(th, b, 0);
            cmp = ((af == bf) ? 2 : (af < bf) ? 4 : 1);
        }
        break;
    case code_bignum:
        {
            Bignum aa = ((Bignum) sc_promote(th, a, cc));
            Bignum bb = ((Bignum) sc_promote(th, b, cc));
            if (NUM_CODE(aa) == cc && NUM_CODE(bb) == cc) {
                Value af = aa->flags;
                Value bf = bb->flags;
                Value ax = aa->exponent;
                Value bx = bb->exponent;
                Value am = aa->mag;
                Value bm = bb->mag;
                cmp = sc_compare2_bignums(af, ax, am, bf, bx, bm);
            }
        }
        break;
    case code_rational:
        {
            Value an = sc_numerator(th, a);
            Value ad = sc_denominator(th, a);
            Value bn = sc_numerator(th, b);
            Value bd = sc_denominator(th, b);
            Value left = sc_product2(th, an, bd);
            Value right = sc_product2(th, bn, ad);
            return sc_compare2(th, left, right);
        }
        break;
    case code_complex:
        {
            Value re_a = sc_real_part(th, a);
            Value re_b = sc_real_part(th, b);
            cmp = 2 & sc_compare2(th, re_a, re_b);
            if (cmp) {
                Value im_a = sc_imag_part(th, a);
                Value im_b = sc_imag_part(th, b);
                cmp &= sc_compare2(th, im_a, im_b);
            }
        }
        break;
    default:
        break;
    }
    return cmp;
}

Activation sc_number_compare(Activation act, ThData th, unsigned leg)
{
    Value a = act->a;
    Value b = act->b;
    Value r = act->r;
    Value ans = HASH_T;
    enum type_code ac, bc;
    enum type_code max_code = code_complex - (leg != 2);
    uintptr_t n = INT_FROM_FIXNUM(th->narg);
    if (n == 0)
        RETURN(HASH_T);
    ac = NUM_CODE(a);
    DEMAND(code_fixnum <= ac && ac <= max_code, a);
    while (n > 1) {
        unsigned cmp = 0;
        if (FIXNUMS_P(a, b)) {
            cmp = ((a == b) ? 2 : (a < b) ? 4 : 1);
            bc = code_fixnum;
        }
        else {
            bc = NUM_CODE(b);
            DEMAND(code_fixnum <= bc && bc <= max_code, b);
            if (TRUE_P(ans)) {
                cmp = sc_compare2(th, a, b);
            }
        }
        if ((cmp & leg) == 0) {
            ans = HASH_F;
        }
        n -= 1;
        if (n < 2)
            break;
        a = b;
        ac = bc;
        b = U_CAR(r);
        r = U_CDR(r);
    }
    RETURN(ans);
}

Value sc_promote(ThData th, Value a, enum type_code target)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value one = FIXNUM_FROM_INT(1);
    enum type_code ac;
    while ((ac = NUM_CODE(a)) < target) {
        switch (ac) {
        case code_fixnum:
            switch (target) {
            case code_float64:
                return sc_new_Float64(0, INT_FROM_FIXNUM(a));
            case code_bignum:
                return sc_bignum_from_int(th, INT_FROM_FIXNUM(a));
            case code_rational:
                return sc_new_Rational(sc_negative_p(a), a, one);
            case code_complex:
                return sc_new_complex(th, a, zero);
            default:
                return NYI;
            }
            break;
        case code_float64:
            switch (target) {
            case code_bignum:
                {
                    Float64 aa = (Float64) a;
                    double x = aa->value;
                    double base = MAX_FIX_INT + 1;
                    Int exponent = 1;
                    FixnumList mag = EMPTY_LIST;
                    if (x < 0.0)
                        x = -x;
                    while (x >= base) {
                        if (exponent > 40)
                            return NYI; /* Maybe x is infinite */
                        exponent++;
                        x /= base;
                    }
                    if (x > 0.0) {
                        while (x < 1.0) {
                            exponent--;
                            x *= base;
                        }
                    }
                    while (x > 0.0) {
                        Int k = (Int) x;
                        mag = CONS(FIXNUM_FROM_INT(k), mag);
                        exponent--;
                        x = (x - k) * base;
                    }
                    if (NULL_P(mag))
                        exponent = 0;
                    return sc_new_Bignum(EXACT_P(a), (aa->value < 0.0), exponent, mag);
                }
                return NYI;
            case code_rational:
                return NYI;
            case code_complex:
                return sc_new_complex(th, a, zero);
            default:
                return NYI;
            }
            break;
        case code_bignum:
            switch (target) {
            case code_rational:
                return sc_new_Rational(sc_negative_p(a), a, one);
            case code_complex:
                return sc_new_complex(th, a, zero);
            default:
                return NYI;
            }
            break;
        case code_rational:
            return sc_new_complex(th, a, zero);
        default:
            return NYI;
        }
    }
    return a;
}

Activation scp_test__flp(Activation act, ThData th)
{
    Value a = act->a;
    Value b = act->b;
    DEMAND_ARGS(2);
    DEMAND(sc_list_length(a) >= 0, a);
    DEMAND(sc_list_length(b) >= 0, b);
    RETURN(sc_fixnumlist_product(th, a, b));
}

FixnumList sc_fixnumlist_product(ThData th, FixnumList a, FixnumList b)
{
    Value zero = FIXNUM_FROM_INT(0);
    FixnumList ans = EMPTY_LIST;
    FixnumList p, aa, bb;
    Int n = sc_list_length(a) + sc_list_length(b);
    Int i = 0;
    for (i = 0; i < n; i++) {
        ans = CONS(zero, ans);
    }
    p = ans;
    for (aa = a; PAIR_P(aa); aa = U_CDR(aa)) {
        FixnumList q = p;
        uintptr_t av = INT_FROM_FIXNUM(U_CAR(aa));
        uintptr_t alo = av & MAX_SAFE_FIXNUM_TERM;
        uintptr_t ahi = av >> HUFIXNUM_BITS;
        for (bb = b; PAIR_P(bb); bb = U_CDR(bb)) {
            uintptr_t bv = INT_FROM_FIXNUM(U_CAR(bb));
            FixnumList qq = U_CDR(q);
            uintptr_t blo = bv & MAX_SAFE_FIXNUM_TERM;
            uintptr_t bhi = bv >> HUFIXNUM_BITS;
            uintptr_t ll = alo * blo + UV(INT_FROM_FIXNUM(U_CAR(q)));
            uintptr_t mm = alo * bhi + ahi * blo;
            uintptr_t hh = ahi * bhi;
            ll += ((mm & MAX_SAFE_FIXNUM_TERM) << HUFIXNUM_BITS);
            mm >>= HUFIXNUM_BITS;
            hh += ll >> UFIXNUM_BITS;
            ll &= MAX_FIX_INT;
            hh += mm;
            U_CAR(q) = FIXNUM_FROM_INT(ll);
            q = qq;
            while (hh) {
                hh += UV(INT_FROM_FIXNUM(U_CAR(qq)));
                U_CAR(qq) = FIXNUM_FROM_INT(hh & MAX_FIX_INT);
                hh >>= UFIXNUM_BITS;
                qq = U_CDR(qq);
            }
        }
        p = U_CDR(p);
    }
    return sc_fixnumlist_trim(th, ans);
}

FixnumList sc_fixnumlist_trim(ThData th, FixnumList a)
{
    /* Trim high-order zeros */
    Value zero = FIXNUM_FROM_INT(0);
    FixnumList p;
    FixnumList aa = EMPTY_LIST;
    (void) th;
    for (p = a; PAIR_P(p); p = U_CDR(p)) {
        if (U_CAR(p) != zero)
            aa = p;
    }
    if (PAIR_P(aa))
        U_CDR(aa) = EMPTY_LIST;
    return a;
}

void sc_base_addto(ThData th, Value dst, Value addend, Value base, Value carry)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value one = FIXNUM_FROM_INT(1);
    assert(FIXNUM_P(base));
    assert(PAIR_P(dst));
    assert(FIXNUM_P(carry));
    while (PAIR_P(addend)) {
        Value a = carry + U_CAR(addend) + U_CAR(dst) - 2 * zero;
        carry = zero;
        addend = U_CDR(addend);
        if (a >= base) {
            a = a - base + zero;
            carry = one;
        }
        U_CAR(dst) = a;
        if (PAIR_P(U_CDR(dst))) {
            dst = U_CDR(dst);
        }
        else {
            if (carry == zero && !PAIR_P(addend)) {
                return;
            }
            dst = U_CDR(dst) = CONS(zero, EMPTY_LIST);
        }
    }
    while (carry != zero) {
        Value a = carry + U_CAR(dst) - zero;
        carry = zero;
        if (a >= base) {
            a = a - base + zero;
            carry = one;
        }
        U_CAR(dst) = a;
        if (carry == zero)
            break;
        if (PAIR_P(U_CDR(dst))) {
            dst = U_CDR(dst);
        }
        else {
            dst = U_CDR(dst) = CONS(zero, EMPTY_LIST);
        }
    }
}

Activation scp_PCT_radix__expand(Activation act, ThData th)
{
    Value a = act->a;
    Value b = act->b;
    DEMAND_ARGS(2);
    DEMAND(NUMBER_P(a) && EXACT_P(a), a);
    DEMAND(FIXNUM_P(b), b);
    a = sc_promote(th, a, code_bignum);
    if (NUM_CODE(a) == code_bignum) {
        Bignum aa = (Bignum) a;
        RETURN(sc_base_expand_bignum(th, aa->exponent, aa->mag, b));
    }
    RETURN(HASH_F);
}

Activation scp_test__base__expand__bignum(Activation act, ThData th)
{
    DEMAND_ARGS(3);
    RETURN(sc_base_expand_bignum(th, act->a, act->b, U_CAR(act->r)));
}

Value sc_base_expand_bignum(ThData th, Value exponent, Value mag, Value base)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value one = FIXNUM_FROM_INT(1);
    if (!(FIXNUM_P(base) && FIXNUM_P(exponent) && exponent >= zero)) {
        return (HASH_F);
    }
    else {
        Value ans = CONS(zero, EMPTY_LIST);
        Value vbit = CONS(one, EMPTY_LIST);
        while (PAIR_P(mag)) {
            Value v = zero;
            intptr_t testbit = one - zero;
            if (exponent > zero) {
                exponent = exponent - (one - zero);
            }
            else {
                v = U_CAR(mag);
                mag = U_CDR(mag);
            }
            while (testbit > 0) {
                if (testbit & v) {
                    sc_base_addto(th, ans, vbit, base, zero);
                }
                testbit <<= 1;
                sc_base_addto(th, vbit, vbit, base, zero);
            }
        }
        return (ans);
    }
}

void sc_pi_number_TO_string(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%number->string");
}

Activation scp_number_TO_string(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value ten = FIXNUM_FROM_INT(10);
    Value a = (act->a);
    Value b = (act->b);
    Value ans = HASH_F;
    char buffer[400];
    if (ARGS_N(1)) {
        act->b = b = ten;       /* supply default radix */
        th->narg = FIXNUM_FROM_INT(2);
    }
    else {
        DEMAND_ARGS(2);
        DEMAND(FIXNUM_P(b) && b > FIXNUM_FROM_INT(1) && b <= FIXNUM_FROM_INT(36), b);
    }
    DEMAND(NUMBER_P(a), a);
    if (b == ten) {
        if (FIXNUM_P(a)) {
            intptr_t i = INT_FROM_FIXNUM(a);
            int len = snprintf(buffer, sizeof(buffer), "%ld", (long) i);
            DEMAND(0 < len && len < (int) (sizeof(buffer)), a);
            ans = VAL(sc_strlit(buffer));
        }
        switch (NUM_CODE(a)) {
        case code_float64:
            {
                Float64 x = (Float64) a;
                Int len = snprintf(buffer, sizeof(buffer), "%.16g", x->value);
                if (0 < len && len < (Int) (sizeof(buffer))) {
                    ans = VAL(sc_strlit(buffer));
                }
            }
            break;
        case code_bignum:
            {
                Bignum x = (Bignum) a;
                if (x->exponent >= zero && PAIR_P(x->mag) && !((x->flags) & NUM_INEXACT)) {
                    Value t = sc_dreverse(sc_base_expand_bignum(th, x->exponent, x->mag, FIXNUM_FROM_INT(100000000)));
                    Int maxlen = 2 + 8 * sc_list_length(t);
                    Int len = 0;
                    String ansbuf = sc_make_string(maxlen);
                    char *s = (char *) (SCHARS(ansbuf));
                    Int i = INT_FROM_FIXNUM(U_CAR(t));
                    if ((x->flags) & NUM_NEG) {
                        i = -i;
                    }
                    len = snprintf(s, maxlen, "%ld", (long) i);
                    t = U_CDR(t);
                    while (PAIR_P(t)) {
                        if (len >= maxlen)
                            break;
                        i = INT_FROM_FIXNUM(U_CAR(t));
                        len += snprintf(s + len, maxlen - len, "%08ld", (long) i);
                        t = U_CDR(t);
                    }
                    if (len < maxlen) {
                        ansbuf->length = FIXNUM_FROM_INT(len);
                        ans = VAL(ansbuf);
                    }
                }
            }
            break;
        default:
            break;
        }
    }
    if (ans != HASH_F) {
        RETURN(ans);
    }
    return sc_punt(act, th, act->codeh->i);
}

void sc_pi_exp(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%exp");
}

Activation scp_exp(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a)) {
        double x = sc_floval(th, a, 0);
        double y = exp(x);
        ans = sc_new_Float64(false, y);
        RETURN(ans);
    }
    return sc_punt(act, th, act->codeh->i);
}

void sc_pi_log(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%log");
}

Activation scp_log(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    if (a == FIXNUM_FROM_INT(1))
        RETURN(ans);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a) && !sc_negative_p(a)) {
        double x = sc_floval(th, a, 0);
        double y = log(x);
        if (!(a == zero || isfinite(y) || TCODE(a) == code_float64)) {
            /* a is either too tiny or too huge for floating point */
            Value numer = sc_numerator(th, a);
            Value denom = sc_denominator(th, a);
            Int k1 = sc_bigdigits(th, numer);
            Int k2 = sc_bigdigits(th, denom);
            double aa = sc_floval(th, numer, 1 - k1);
            double bb = sc_floval(th, denom, 1 - k2);
            double logbase = log(MAX_FIX_INT + 1);
            double xnorm = aa / bb;
            double logxnorm = log(xnorm);
            y = log(xnorm) + (k1 - k2) * logbase;
        }
        ans = sc_new_Float64(false, y);
        RETURN(ans);
    }
    return sc_punt(act, th, act->codeh->i);
}

void sc_pi_sin(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%sin");
}

Activation scp_sin(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a)) {
        double x = sc_floval(th, a, 0);
        double y = sin(x);
        ans = sc_new_Float64(false, y);
        RETURN(ans);
    }
    return sc_punt(act, th, act->codeh->i);
}

void sc_pi_cos(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%cos");
}

Activation scp_cos(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a)) {
        double x = sc_floval(th, a, 0);
        double y = cos(x);
        ans = sc_new_Float64(false, y);
        RETURN(ans);
    }
    return sc_punt(act, th, act->codeh->i);
}

void sc_pi_tan(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%tan");
}

Activation scp_tan(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a)) {
        double x = sc_floval(th, a, 0);
        double y = tan(x);
        ans = sc_new_Float64(false, y);
        RETURN(ans);
    }
    return sc_punt(act, th, act->codeh->i);
}

void sc_pi_asin(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%asin");
}

Activation scp_asin(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a)) {
        double x = sc_floval(th, a, 0);
        if (-1.0 <= x && x <= 1.0) {
            double y = asin(x);
            ans = sc_new_Float64(false, y);
            RETURN(ans);
        }
    }
    return sc_punt(act, th, act->codeh->i);
}

void sc_pi_acos(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%acos");
}

Activation scp_acos(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a)) {
        double x = sc_floval(th, a, 0);
        if (-1.0 <= x && x <= 1.0) {
            double y = acos(x);
            ans = sc_new_Float64(false, y);
            RETURN(ans);
        }
    }
    return sc_punt(act, th, act->codeh->i);
}

void sc_pi_atan(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%atan");
}

Activation scp_atan(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value b = (act->b);
    Value ans = zero;
    if (ARGS_N(1)) {
        DEMAND(NUMBER_P(a), a);
        if (RATIONAL_P(a)) {
            double x = sc_floval(th, a, 0);
            double y = atan(x);
            ans = sc_new_Float64(false, y);
            RETURN(ans);
        }
    }
    else {
        DEMAND_ARGS(2);
        DEMAND(NUMBER_P(a), a);
        DEMAND(NUMBER_P(b), b);
        if (RATIONAL_P(a) && RATIONAL_P(b)) {
            double y = sc_floval(th, a, 0);
            double x = sc_floval(th, b, 0);
            double v = atan2(y, x);
            ans = sc_new_Float64(false, v);
            RETURN(ans);
        }
    }
    return sc_punt(act, th, act->codeh->i);
}

void sc_pi_sqrt(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%sqrt");
}

Activation scp_sqrt(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a)) {
        double x = sc_floval(th, a, 0);
        if (x >= 0.0) {
            double y = sqrt(x);
            ans = sc_new_Float64(false, y);
            RETURN(ans);
        }
    }
    return sc_punt(act, th, act->codeh->i);
}

Activation scp_magnitude(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a)) {
        if (sc_negative_p(a)) {
            ans = sc_negate(th, a);
        }
        else {
            ans = a;
        }
    }
    else {
        double x = sc_floval(th, sc_real_part(th, a), 0);
        double y = sc_floval(th, sc_imag_part(th, a), 0);
        double mag = hypot(y, x);
        ans = sc_new_Float64(false, mag);
        RETURN(ans);
    }
    RETURN(ans);
}

Activation scp_angle(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a) && !sc_negative_p(a)) {
        if (INEXACT_P(a))
            ans = sc_exact_to_inexact(th, ans);
    }
    else {
        double x = sc_floval(th, sc_real_part(th, a), 0);
        double y = sc_floval(th, sc_imag_part(th, a), 0);
        double arg = atan2(y, x);
        ans = sc_new_Float64(false, arg);
        RETURN(ans);
    }
    RETURN(ans);
}

/* TEMPLATE */
static double myfunc(double x)
{
    return 1 / (1 + x * x);
}

void sc_pi_myfunc(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%myfunc");
}

Activation scp_myfunc(Activation act, ThData th)
{
    Value zero = FIXNUM_FROM_INT(0);
    Value a = (act->a);
    Value ans = zero;
    DEMAND_ARGS(1);
    DEMAND(NUMBER_P(a), a);
    if (RATIONAL_P(a)) {
        double x = sc_floval(th, a, 0);
        double y = myfunc(x);
        ans = sc_new_Float64(false, y);
        RETURN(ans);
    }
    return sc_punt(act, th, act->codeh->i);
}
