#define SC_MODULE_ID 20000
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <errno.h>
#include "scutum.h"
#include "primitives.h"

Activation scp_boolean_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(BOOLEAN_P(x)));
}

Activation scp_cons(Activation act, ThData th)
{
    Pair p = th->pairs;
    if (NULL_P(p)) {
        return sc_prime_cons_pump(act, th);
    }
    DEMAND_ARGS(2);
    th->pairs = (List) U_CDR(p);
    p->car = act->a;
    p->cdr = act->b;
    RETURN((Value) p);
}

Activation scp_pair_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(PAIR_P(x)));
}

Activation scp_car(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(PAIR_P(x), x);
    RETURN(U_CAR(x));
}

Activation scp_cdr(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(PAIR_P(x), x);
    RETURN(U_CDR(x));
}

Activation scp_set__car_BANG(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(2);
    DEMAND(PAIR_P(x), x);
    ((Pair) x)->car = act->b;
    RETURN(UNSPEC);
}

Activation scp_set__cdr_BANG(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(2);
    DEMAND(PAIR_P(x), x);
    ((Pair) x)->cdr = act->b;
    RETURN(UNSPEC);
}

Activation scp_null_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(NULL_P(x)));
}

Activation scp_list_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(sc_list_length(x) >= 0));
}

Activation scp_list(Activation act, ThData th)
{
    Pair head = (th->pairs);
    Pair last = head;
    Int len = INT_FROM_FIXNUM(th->narg);
    Value tail = EMPTY_LIST;
    if (len <= 0)
        RETURN(EMPTY_LIST);
    if (NULL_P(last) || NULL_P(U_CDR(last)))
        return sc_prime_cons_pump(act, th);
    last->car = act->a;
    if (len > 1) {
        last = (Pair) U_CDR(last);
        last->car = act->b;
        if (len > 2) {
            tail = act->r;
        }
    }
    th->pairs = (List) U_CDR(last);
    last->cdr = tail;
    RETURN((Value) head);
}

Activation scp_length(Activation act, ThData th)
{
    Value x = act->a;
    Int len = sc_list_length(x);
    DEMAND_ARGS(1);
    DEMAND(len >= 0, x);
    RETURN(FIXNUM_FROM_INT(len));
}

Activation scp_memq(Activation act, ThData th)
{
    Value x = act->a;
    Value l = act->b;
    DEMAND_ARGS(2);
    while (l != EMPTY_LIST) {
        DEMAND(PAIR_P(l), l);
        if (U_CAR(l) == x) {
            RETURN(l);
        }
        l = U_CDR(l);
    }
    RETURN(HASH_F);
}

Activation scp_assq(Activation act, ThData th)
{
    Value key = act->a;
    Value assoclist = act->b;
    Value ans = HASH_F;
    DEMAND_ARGS(2);
    ans = sc_assq(key, assoclist);
    DEMAND(ans != WRONG_TYPE, assoclist);
    RETURN(ans);
}

Activation scp_reverse(Activation act, ThData th)
{
    Value x = act->a;
    Value rem = x;
    Value ans = EMPTY_LIST;
    DEMAND_ARGS(1);
    while (rem != EMPTY_LIST) {
        DEMAND(PAIR_P(rem), x);
        ans = CONS(U_CAR(rem), ans);
        rem = U_CDR(rem);
    }
    RETURN(ans);
}

Activation scp_eq_P(Activation act, ThData th)
{
    Value x = act->a;
    Value y = act->b;
    DEMAND_ARGS(2);
    RETURN(MAKE_BOOLEAN(x == y));
}

Activation scp_eqv_P(Activation act, ThData th)
{
    Value a = act->a;
    Value b = act->b;
    DEMAND_ARGS(2);
    if (!HEAP_P(a))
        RETURN(MAKE_BOOLEAN(a == b));
    if (NUMBER_P(a) && NUMBER_P(b) && EXACT_P(a) == EXACT_P(b))
        return sc_number_compare(act, th, 2);
    RETURN(MAKE_BOOLEAN(a == b));
}

Activation scp_not(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(FALSE_P(x)));
}

static Activation CopyRestAndSetNarg(Activation act, ThData th)
{
    Value lst = act->r;
    int len = sc_list_length(lst);
    DEMAND(len >= 0, lst);
    act->r = sc_list_copy(th, lst);
    th->narg = FIXNUM_FROM_INT(2 + len);
    return act;
}

void sc_pi_apply(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%apply");
}

Activation scp_apply(Activation act, ThData th)
{
    Value x = act->a;
    Value y = act->b;
    Value a = UNSPEC;
    Value b = UNSPEC;
    int i = 0;
    if (th->narg != FIXNUM_FROM_INT(2)) {
        return sc_punt(act, th, act->codeh->i);
    }
    DEMAND(PROC_P(x), x);
    if (!NULL_P(y)) {
        DEMAND(PAIR_P(y), y);
        a = U_CAR(y);
        y = (Value) U_CDR(y);
        i++;
        if (!NULL_P(y)) {
            DEMAND(PAIR_P(y), y);
            b = U_CAR(y);
            y = (Value) U_CDR(y);
            i++;
        }
    }
    act->a = a;
    act->b = b;
    act->r = y;
    th->narg = FIXNUM_FROM_INT(i);      /* may be adjusted by
                                         * CopyRestAndSetNarg */
    act->env = (((Proc) x)->env);
    act->codeh = (((Proc) x)->entry_pt);
    if (!NULL_P(y)) {
        return CopyRestAndSetNarg(act, th);
    }
    return (act);
}

void sc_pi_call__with__current__continuation(ThData th, CodeHandle c)
{
    c->i = (Value) sc_symlit("continuation...");
    (void) th;
}

Activation scp_call__with__current__continuation(Activation act, ThData th)
{
    Proc action = (Proc) (act->a);
    Proc continuation = 0;
    CodeHandle h = 0;
    DEMAND_ARGS(1);
    DEMAND(PROC_P(action), action);
    h = sc_new_CodeHandle(&sc_continuation);
    h->i = (Value) sc_copy_continuation(act->cont, th);
    h->j = (Value) (th->dynamic);
    continuation = sc_new_Proc(h, act->codeh->i);
    act->a = (Value) continuation;
    act->env = action->env;
    act->codeh = action->entry_pt;
    return act;
}

void sc_package_multiple_values(Activation act, ThData th)
{
    Int k = INT_FROM_FIXNUM(th->narg);
    Value v = EMPTY_LIST;
    if (k > 2) {
        v = act->r;
        act->r = EMPTY_LIST;
    }
    if (k > 1) {
        v = CONS(act->b, v);
        act->b = UNSPEC;
    }
    if (k > 0) {
        v = CONS(act->a, v);
    }
    act->a = CONS(MULTVALUES, v);
    th->narg = FIXNUM_FROM_INT(1);
}

#define RECYCLE_ACTS 1
Activation sc_continuation(Activation act, ThData th)
{
    Activation newact = 0;
    Value saved_act = act->codeh->i;
    Value saved_dynamic = act->codeh->j;
    Value next_dynamic = EMPTY_LIST;
    if (!HEAP_P(act->link)) {
        return sc_prime_act_pump(act, th);
    }
    if (th->narg != FIXNUM_FROM_INT(1) && act->b != VAL(act->codeh)) {
        sc_package_multiple_values(act, th);
    }
    DEMAND(ACTIVATION_P(saved_act), FIXNUM_FROM_INT(SC_MODULE_ID + __LINE__));
    next_dynamic = sc_find_next_dynamic(VAL(th->dynamic), saved_dynamic);
    if (PAIR_P(next_dynamic)) {
        /*
         * Process one before/after proc, then come back to sc_continuation
         * again
         */
        Proc p = (Proc) (U_CDR(U_CAR(next_dynamic)));
        DEMAND(PROC_P(p), p);
        act->b = VAL(act->codeh);       /* marker to prevent re-wrap of mult
                                         * args */
        act = act->link;
        act->codeh = p->entry_pt;
        th->narg = FIXNUM_FROM_INT(0);
        th->dynamic = (List) ((U_CAR(U_CAR(next_dynamic)) == AFTERTHUNK) ? U_CDR(next_dynamic) : next_dynamic);
        return act;
    }
    th->val = act->a;
    th->dynamic = (List) (saved_dynamic);
    newact = sc_copy_continuation((Activation) saved_act, th);
    if (RECYCLE_ACTS) {
        while (HEAP_P(act->cont)) {
            act->a = act->b = UNSPEC;
            act->r = EMPTY_LIST;
            act = act->cont;
        }
        newact->link = act;
        act->cont = newact;
    }
    return newact;
}

void sc_pi_dynamic__wind(ThData th, CodeHandle c)
{
    c->i = VAL(sc_new_CodeHandle(&sc_dynamic_wind));
    (void) th;
}

Activation scp_dynamic__wind(Activation act, ThData th)
{
    Value before = act->a;
    Value thunk = act->b;
    Value after = 0;
    if (!HEAP_P(act->link)) {
        return sc_prime_act_pump(act, th);
    }
    DEMAND_ARGS(3);
    after = U_CAR(act->r);
    DEMAND(PROC_P(before), before);
    DEMAND(PROC_P(thunk), thunk);
    DEMAND(PROC_P(after), after);

    act->a = CONS(CONS(BEFORETHUNK, before), CONS(CONS(AFTERTHUNK, after), th->dynamic));
    act->r = EMPTY_LIST;
    act->codeh = (CodeHandle) (act->codeh->i);
    act = act->link;
    act->a = act->b = UNDEF;
    act->r = EMPTY_LIST;
    act->codeh = ((Proc) before)->entry_pt;
    th->narg = FIXNUM_FROM_INT(0);
    return act;
}

Activation sc_dynamic_wind(Activation act, ThData th)
{
    Value thunk = act->b;
    if (!HEAP_P(act->link)) {
        return sc_prime_act_pump(act, th);
    }
    if (PROC_P(thunk)) {
        DEMAND(U_CDR(U_CDR(act->a)) == VAL(th->dynamic), act->a);
        th->dynamic = (List) (act->a);
        act->b = HASH_F;
        act = act->link;
        act->a = act->b = UNDEF;
        act->r = EMPTY_LIST;
        act->codeh = ((Proc) thunk)->entry_pt;
        th->narg = FIXNUM_FROM_INT(0);
        return act;
    }
    if (act->b == HASH_F) {
        Value after = U_CDR(U_CAR(U_CDR(act->a)));
        DEMAND(act->a == VAL(th->dynamic) && U_CAR(U_CAR(U_CDR(act->a))) == AFTERTHUNK, act->a);
        DEMAND(PROC_P(after), after);
        th->dynamic = (List) (U_CDR(U_CDR(act->a)));
        act->a = th->val;
        act->b = HASH_T;
        act = act->link;
        act->a = act->b = UNDEF;
        act->r = EMPTY_LIST;
        act->codeh = ((Proc) after)->entry_pt;
        th->narg = FIXNUM_FROM_INT(0);
        return act;
    }
    RETURN(act->a);
}

Activation scp_symbol_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(SYMBOL_P(x)));
}

Activation scp_string_TO_symbol(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(STRING_P(x), x);
    RETURN((Value) sc_string_to_symbol(th, (String) x));
}

Activation scp_symbol_TO_string(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(SYMBOL_P(x), x);
    RETURN((Value) (((Symbol) x)->pval));
}

Activation scp_string_TO_list(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(STRING_P(x), x);
    RETURN((Value) sc_string_to_list(th, (String) x));
}

Activation scp_list_TO_string(Activation act, ThData th)
{
    Value x = act->a;
    intptr_t len = 0;
    intptr_t i = 0;
    Value rem = 0;
    String str = 0;
    DEMAND_ARGS(1);
    len = sc_list_length(x);
    DEMAND(len >= 0, x);
    str = sc_make_string(len);
    rem = x;
    for (i = 0; i < len; i++) {
        Value c;
        DEMAND(PAIR_P(rem), rem);
        c = U_CAR(rem);
        DEMAND(CHAR_P(c), c);
        rem = U_CDR(rem);
        SCHARS(str)[i] = UINT_FROM_CHARVAL(c);
    }
    RETURN(str);
}

Activation scp_char_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(CHAR_P(x)));
}

Activation scp_char_TO_integer(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(CHAR_P(x), x);
    RETURN(FIXNUM_FROM_INT(UINT_FROM_CHARVAL(x)));
}

Activation scp_integer_TO_char(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(FIXNUM_P(x) && INT_FROM_FIXNUM(x) >= 0, x);
    RETURN(CHARVAL_FROM_UINT(INT_FROM_FIXNUM(x)));
}

Activation scp_char__alphabetic_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(CHAR_P(x), x);
    RETURN(MAKE_BOOLEAN(CHAR_LOWER_CASE_P(x) || CHAR_UPPER_CASE_P(x)));
}

Activation scp_char__upper__case_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(CHAR_P(x), x);
    RETURN(MAKE_BOOLEAN(CHAR_UPPER_CASE_P(x)));
}

Activation scp_char__lower__case_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(CHAR_P(x), x);
    RETURN(MAKE_BOOLEAN(CHAR_LOWER_CASE_P(x)));
}

Activation scp_char__whitespace_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(CHAR_P(x), x);
    RETURN(MAKE_BOOLEAN(CHAR_WHITESP_P(x)));
}

Activation scp_char__numeric_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(CHAR_P(x), x);
    RETURN(MAKE_BOOLEAN(CHAR_NUMERIC_P(x)));
}

Activation scp_char__upcase(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(CHAR_P(x), x);
    RETURN(CHAR_LOWER_CASE_P(x) ? (x ^ (SC_CHAR_a ^ SC_CHAR_A)) : x);
}

Activation scp_char__downcase(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    DEMAND(CHAR_P(x), x);
    RETURN(CHAR_UPPER_CASE_P(x) ? (x ^ (SC_CHAR_a ^ SC_CHAR_A)) : x);
}

Activation scp_char_EQ_P(Activation act, ThData th)
{
    Value x = act->a;
    Value y = act->b;
    DEMAND_ARGS(2);
    DEMAND(CHAR_P(x) && CHAR_P(y), x);
    RETURN(MAKE_BOOLEAN(x == y));
}

Activation scp_char__ci_EQ_P(Activation act, ThData th)
{
    Value x = act->a;
    Value y = act->b;
    DEMAND_ARGS(2);
    DEMAND(CHAR_P(x) && CHAR_P(y), x);
    if (CHAR_UPPER_CASE_P(x)) {
        x ^= (SC_CHAR_a ^ SC_CHAR_A);
    }
    if (CHAR_UPPER_CASE_P(y)) {
        y ^= (SC_CHAR_a ^ SC_CHAR_A);
    }
    RETURN(MAKE_BOOLEAN(x == y));
}

Activation scp_procedure_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(PROC_P(x)));
}

Activation scp_number_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(NUMBER_P(x)));
}

Activation scp_string_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(STRING_P(x)));
}

Activation scp_make__string(Activation act, ThData th)
{
    Value a = act->a;
    Value b;
    String ans = 0;
    if (ARGS_N(1)) {
        b = CHARVAL_FROM_UINT('?');
    }
    else {
        DEMAND_ARGS(2);
        b = act->b;
    }
    DEMAND(FIXNUM_P(a) && a >= 0, a);
    DEMAND(CHAR_P(b), b);
    ans = sc_make_string(INT_FROM_FIXNUM(a));
    sc_fill_string(ans, UINT_FROM_CHARVAL(b));
    RETURN(ans);
}

Activation scp_string__length(Activation act, ThData th)
{
    String a = (String) (act->a);
    DEMAND_ARGS(1);
    DEMAND(STRING_P(a), a);
    RETURN(a->length);
}

Activation scp_string__ref(Activation act, ThData th)
{
    String a = (String) (act->a);
    Value b = act->b;
    DEMAND_ARGS(2);
    DEMAND(STRING_P(a), a);
    DEMAND(FIXNUM_P(b) && (UV(b) < UV(a->length)), b);
    RETURN(CHARVAL_FROM_UINT(SCHARS(a)[INT_FROM_FIXNUM(b)]));
}

Activation scp_string__set_BANG(Activation act, ThData th)
{
    String a = (String) (act->a);
    Value b = act->b;
    Value c;
    DEMAND_ARGS(3);
    c = U_CAR(act->r);
    DEMAND(STRING_P(a), a);
    DEMAND(FIXNUM_P(b) && (UV(b) < UV(a->length)), b);
    DEMAND(CHAR_P(c), c);
    SCHARS(a)[INT_FROM_FIXNUM(b)] = UINT_FROM_CHARVAL(c);
    RETURN(UNSPEC);
}

Activation scp_string_EQ_P(Activation act, ThData th)
{
    String a = (String) (act->a);
    String b = (String) (act->b);
    uint8_t *ap = 0;
    uint8_t *bp = 0;
    intptr_t i = 0;
    intptr_t n = 0;
    DEMAND_ARGS(2);
    DEMAND(STRING_P(a), a);
    DEMAND(STRING_P(b), b);
    ap = SCHARS(a);
    bp = SCHARS(b);
    if (a->length != b->length)
        RETURN(HASH_F);
    DEMAND(FIXNUM_P(a->length), a->length);
    n = INT_FROM_FIXNUM(a->length);
    while (i < n) {
        if (ap[i] != bp[i])
            RETURN(HASH_F);
        i++;
    }
    RETURN(HASH_T);
}
void sc_pi_string_TO_number(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%string->number");
}

Activation scp_PCT_string_TO_number(Activation act, ThData th)
{
    RETURN(HASH_F);
}

Activation scp_string_TO_number(Activation act, ThData th)
{
    Value ten = FIXNUM_FROM_INT(10);
    String a = (String) (act->a);
    Value b = (act->b);
    intptr_t len = 0;
    intptr_t radix = 10;
    char buffer[400];
    char *endp = 0;
    long val = 0;
    if (ARGS_N(1)) {
        act->b = b = ten;       /* supply default radix */
        th->narg = FIXNUM_FROM_INT(2);
    }
    else {
        DEMAND_ARGS(2);
        DEMAND(FIXNUM_P(b), b);
    }
    DEMAND(STRING_P(a), a);
    radix = INT_FROM_FIXNUM(b);
    DEMAND(1 < radix && radix <= 36, b);
    len = INT_FROM_FIXNUM(a->length);
    if (0 > len || (size_t) len >= sizeof(buffer))
        return sc_punt(act, th, act->codeh->i);
    memcpy(buffer, SCHARS(a), len);
    buffer[len] = 0;
    if (radix == 16 && (strchr(buffer, 'X') || strchr(buffer, 'x')))
        return sc_punt(act, th, act->codeh->i);
    errno = 0;
    val = strtol(buffer, &endp, radix);
    if (errno || (endp != buffer + len) || !FIXNUMABLE(val))
        return sc_punt(act, th, act->codeh->i);
    RETURN(FIXNUM_FROM_INT(val));
}

Activation scp_vector_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(VECTOR_P(x)));
}

Activation scp_make__vector(Activation act, ThData th)
{
    Value a = act->a;
    Value b = UNSPEC;
    Vector ans = 0;
    if (!ARGS_N(1)) {
        DEMAND_ARGS(2);
        b = act->b;
    }
    DEMAND(FIXNUM_P(a) && a >= 0, a);
    ans = sc_make_vector(INT_FROM_FIXNUM(a), b);
    RETURN(ans);
}

Activation scp_vector__length(Activation act, ThData th)
{
    Value a = act->a;
    DEMAND_ARGS(1);
    DEMAND(VECTOR_P(a), a);
    RETURN(((Vector) (a))->length);
}

Activation scp_vector__ref(Activation act, ThData th)
{
    Vector a = (Vector) (act->a);
    Value b = act->b;
    DEMAND_ARGS(2);
    DEMAND(VECTOR_P(a), a);
    DEMAND(FIXNUM_P(b) && (UV(b) < UV(a->length)), b);
    RETURN(VECTOR_ARR(a)[INT_FROM_FIXNUM(b)]);
}

Activation scp_vector__set_BANG(Activation act, ThData th)
{
    Vector a = (Vector) (act->a);
    Value b = act->b;
    Value c;
    DEMAND_ARGS(3);
    c = U_CAR(act->r);
    DEMAND(VECTOR_P(a), a);
    DEMAND(FIXNUM_P(b) && (UV(b) < UV(a->length)), b);
    VECTOR_ARR(a)[INT_FROM_FIXNUM(b)] = c;
    RETURN(UNSPEC);
}

Activation scp_port_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(PORT_P(x)));
}

Activation scp_input__port_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(INPUT_PORT_P(x)));
}

Activation scp_current__input__port(Activation act, ThData th)
{
    DEMAND_ARGS(0);
    RETURN(th->inport);
}

Activation scp_eof__object_P(Activation act, ThData th)
{
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(act->a == EOF_OBJECT));
}

Activation scp_read__char(Activation act, ThData th)
{
    Port p = (Port) (act->a);
    Value ans = EOF_OBJECT;
    Proc rc;
    if (ARGS_N(0)) {
        p = (Port) (th->inport);
    }
    else
        DEMAND_ARGS(1);
    DEMAND(INPUT_PORT_P(p), VAL(p));
    ans = p->charbuf;
    if (TRUE_P(ans)) {
        if (CHAR_P(ans)) {
            p->charbuf = HASH_F;
        }
        RETURN(ans);
    }
    rc = (Proc) (p->read_char);
    act->a = p->data;
    act->b = HASH_T;            /* wait? */
    act->env = rc->env;
    act->codeh = rc->entry_pt;
    th->narg = FIXNUM_FROM_INT(2);
    return act;
}

void sc_pi_peek__char(ThData th, CodeHandle c)
{
    c->i = VAL(sc_new_CodeHandle(&sc_peek_char_1));
    (void) th;
}

Activation scp_peek__char(Activation act, ThData th)
{
    /* INIT: i = CODEH(sc_peek_char_1); */
    Port p = (Port) (act->a);
    Value ans = EOF_OBJECT;
    Proc rc;
    CodeHandle cont1 = (CodeHandle) (act->codeh->i);
    DEMAND(HEAP_P(cont1), act->codeh);
    if (!HEAP_P(act->link)) {
        return sc_prime_act_pump(act, th);
    }
    if (ARGS_N(0)) {
        p = (Port) (th->inport);
    }
    else
        DEMAND_ARGS(1);
    DEMAND(INPUT_PORT_P(p), VAL(p));
    ans = p->charbuf;
    if (TRUE_P(ans)) {
        RETURN(ans);
    }
    rc = (Proc) (p->read_char);
    act->a = VAL(p);
    act->codeh = cont1;
    act = act->link;
    act->a = p->data;
    act->b = HASH_T;            /* wait? */
    act->env = rc->env;
    act->codeh = rc->entry_pt;
    th->narg = FIXNUM_FROM_INT(2);
    return act;
}

Activation sc_peek_char_1(Activation act, ThData th)
{
    Port p = (Port) (act->a);
    p->charbuf = th->val;
    return act->cont;
}

void sc_pi_char__ready_P(ThData th, CodeHandle c)
{
    c->i = VAL(sc_new_CodeHandle(&sc_char_ready_1));
    (void) th;
}

Activation scp_char__ready_P(Activation act, ThData th)
{
    /* INIT: i = CODEH(sc_char_ready_1); */
    Port p = (Port) (act->a);
    Proc rc;
    CodeHandle cont1 = (CodeHandle) (act->codeh->i);
    DEMAND(HEAP_P(cont1), act->codeh);
    if (!HEAP_P(act->link)) {
        return sc_prime_act_pump(act, th);
    }
    if (ARGS_N(0)) {
        p = (Port) (th->inport);
    }
    else
        DEMAND_ARGS(1);
    DEMAND(INPUT_PORT_P(p), VAL(p));
    if (TRUE_P(p->charbuf)) {
        RETURN(HASH_T);
    }
    rc = (Proc) (p->read_char);
    act->a = VAL(p);
    act->codeh = cont1;
    act = act->link;
    act->a = p->data;
    act->b = HASH_F;            /* do not wait */
    act->env = rc->env;
    act->codeh = rc->entry_pt;
    th->narg = FIXNUM_FROM_INT(2);
    return act;
}

Activation sc_char_ready_1(Activation act, ThData th)
{
    Port p = (Port) (act->a);
    if (TRUE_P(th->val)) {
        p->charbuf = th->val;
        RETURN(HASH_T);
    }
    RETURN(HASH_F);
}

Activation scp_close__port(Activation act, ThData th)
{
    Port p = (Port) (act->a);
    Proc c;
    DEMAND(PORT_P(p), VAL(p));
    c = (Proc) (p->close);
    if (PROC_P(c)) {
        act->a = p->data;
        act->env = c->env;
        act->codeh = c->entry_pt;
        th->narg = FIXNUM_FROM_INT(1);
        return act;
    }
    RETURN(UNSPEC);
}

Activation scp_stdio__close(Activation act, ThData th)
{
    Whatever d = (Whatever) (act->a);
    Value ans = HASH_F;
    if (d->tc == TAGTY(code_filep)) {
        int res = fclose(d->sysdata);
        if (res == 0) {
            ans = HASH_T;
        }
    }
    RETURN(ans);
}

Activation scp_make__input__port(Activation act, ThData th)
{
    Value a = act->a;
    Value b = act->b;
    DEMAND_ARGS(3);
    DEMAND(PROC_P(a), a);
    DEMAND(FALSE_P(b) || PROC_P(b), b);
    RETURN(sc_new_Port(VAL(sc_strlit("IN")), U_CAR(act->r), a, HASH_F, b));
}

Activation scp_string__read__char(Activation act, ThData th)
{
    Value a = act->a;
    intptr_t n;
    Value s;
    DEMAND_ARGS(2);
    DEMAND(PAIR_P(a) && FIXNUM_P(U_CAR(a)) && STRING_P(U_CDR(a)), a);
    n = INT_FROM_FIXNUM(U_CAR(a));
    s = U_CDR(a);
    if (n < 0 || (uintptr_t) n >= STRING_LENGTH(s))
        RETURN(EOF_OBJECT);
    ((Pair) a)->car = FIXNUM_FROM_INT(n + 1);
    RETURN(CHARVAL_FROM_UINT(SCHARS(s)[n]));
}

void sc_pi_open__input__string(ThData th, CodeHandle c)
{
    c->i = U_CDR(sc_str_global_binding(th, "string-read-char"));
    c->j = VAL(sc_strlit("STRING IN"));
}

Activation scp_open__input__string(Activation act, ThData th)
{
    Value a = act->a;
    ProcedureValue readchar = act->codeh->i;
    StringValue ident = act->codeh->j;
    DEMAND_ARGS(1);
    DEMAND(STRING_P(a), a);
    RETURN(sc_new_Port(ident, CONS(FIXNUM_FROM_INT(0), a), readchar, HASH_F, HASH_F));
}

void sc_pi_read(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%reader");
}

Activation scp_read(Activation act, ThData th)
{
    Port p = (Port) (act->a);
    Value t = act->codeh->i;
    if (ARGS_N(0)) {
        p = (Port) (th->inport);
    }
    else
        DEMAND_ARGS(1);
    DEMAND(INPUT_PORT_P(p), VAL(p));
    if (PAIR_P(t)) {
        Value rq = U_CDR(t);
        if (PROC_P(rq)) {
            Proc r = (Proc) rq;
            th->narg = FIXNUM_FROM_INT(1);
            act->a = (Value) p;
            act->env = r->env;
            act->codeh = r->entry_pt;
            return act;
        }
    }
    RETURN(EOF_OBJECT);
}

Activation scp_make__output__port(Activation act, ThData th)
{
    Value a = act->a;
    Value b = act->b;
    DEMAND_ARGS(3);
    DEMAND(PROC_P(a), a);
    DEMAND(FALSE_P(b) || PROC_P(b), b);
    RETURN(sc_new_Port(VAL(sc_strlit("OUT")), U_CAR(act->r), HASH_F, a, b));
}

Activation scp_output__port_P(Activation act, ThData th)
{
    Value x = act->a;
    DEMAND_ARGS(1);
    RETURN(MAKE_BOOLEAN(PORT_P(x) && TRUE_P(((Port) x)->write_char)));
}

Activation scp_current__output__port(Activation act, ThData th)
{
    DEMAND_ARGS(0);
    RETURN(th->outport);
}

Activation scp_PCT_set__current__port_BANG(Activation act, ThData th)
{
    Value which = act->a;
    Value port = act->b;
    DEMAND_ARGS(2);
    DEMAND(FIXNUM_P(which), which);
    DEMAND(PORT_P(port), port);
    if (which == FIXNUM_FROM_INT(0)) {
        th->inport = port;
    }
    else if (which == FIXNUM_FROM_INT(1)) {
        th->outport = port;
    }
    RETURN(UNSPEC);
}

Activation scp_write__char(Activation act, ThData th)
{
    Value ch = act->a;
    Port p = (Port) (act->b);
    Proc wc;
    if (ARGS_N(1)) {
        p = (Port) (th->outport);
    }
    else
        DEMAND_ARGS(2);
    DEMAND(CHAR_P(ch), ch);
    DEMAND(OUTPUT_PORT_P(p), VAL(p));
    wc = (Proc) (p->write_char);
    act->a = p->data;
    act->b = ch;
    act->env = wc->env;
    act->codeh = wc->entry_pt;
    th->narg = FIXNUM_FROM_INT(2);
    return act;
}

Activation scp_stdio__write__char(Activation act, ThData th)
{
    Whatever d = (Whatever) (act->a);
    Value ch = act->b;
    unsigned char s[1];
    size_t res = 0;
    DEMAND_ARGS(2);
    DEMAND(CHAR_P(ch) && (d->tc == TAGTY(code_filep)), ch);
    s[0] = UINT_FROM_CHARVAL(ch);       /* FIXTHIS UTF-8 */
    res = fwrite(s, 1, 1, d->sysdata);
    DEMAND(res == 1, ch);
    RETURN(UNSPEC);
}

Port sc_stdout(ThData th)
{
    ProcedureValue writechar = U_CDR(sc_global_binding(th, VAL(sc_symlit("stdio-write-char"))));
    Whatever dat = sc_new_box(code_filep, stdout);
    PortValue ans = sc_new_Port(VAL(sc_strlit("STDOUT")), VAL(dat), HASH_F, writechar, HASH_F);
    return ((Port) ans);
}

Activation scp_read__trace_TO(Activation act, ThData th)
{
    Value ch = act->a;
    DEMAND_ARGS(1);
    if (CHAR_P(th->symtrace)) {
        if (CHAR_P(ch)) {
            fprintf(stderr, "-|%c|-", (int) UINT_FROM_CHARVAL(ch));
        }
        else if (ch == EOF_OBJECT) {
            fprintf(stderr, "-|EOF|-\n");
        }
        else
            abort();
    }
    RETURN(ch);
}

Activation scp_stdio__read__char(Activation act, ThData th)
{
    Whatever d = (Whatever) (act->a);
    Value ans = EOF_OBJECT;
    if (d->tc == TAGTY(code_filep)) {
        int c = fgetc(d->sysdata);
        if (c != EOF) {
            ans = CHARVAL_FROM_UINT(c);
        }
    }
    RETURN(ans);
}

Port sc_stdin(ThData th)
{
    ProcedureValue readchar = U_CDR(sc_global_binding(th, VAL(sc_symlit("stdio-read-char"))));
    StringValue ident = VAL(sc_strlit("STDIN"));
    Whatever dat = sc_new_box(code_filep, stdin);
    PortValue ans = sc_new_Port(ident, VAL(dat), readchar, HASH_F, HASH_F);
    return ((Port) ans);
}

void sc_pi_write(ThData th, CodeHandle c)
{
    c->i = sc_str_global_binding(th, "%writer");
}

Activation scp_write(Activation act, ThData th)
{
    Port p = (Port) (act->b);
    Value t = act->codeh->i;
    if (ARGS_N(1)) {
        p = (Port) (th->outport);
    }
    else
        DEMAND_ARGS(2);
    DEMAND(OUTPUT_PORT_P(p), VAL(p));
    if (PAIR_P(t)) {
        Value wq = U_CDR(t);
        if (PROC_P(wq)) {
            Proc w = (Proc) wq;
            th->narg = FIXNUM_FROM_INT(2);
            act->b = (Value) p;
            act->env = w->env;
            act->codeh = w->entry_pt;
            return act;
        }
    }
    RETURN(UNSPEC);
}

void sc_pi_open__stdio__file(ThData th, CodeHandle c)
{
    c->i = U_CDR(sc_str_global_binding(th, "stdio-read-char"));
    c->j = U_CDR(sc_str_global_binding(th, "stdio-write-char"));
    c->k = U_CDR(sc_str_global_binding(th, "stdio-close"));
}

Activation scp_open__stdio__file(Activation act, ThData th)
{
    StringValue a = act->a;
    StringValue b = act->b;
    char pathbuf[PATH_MAX + 1];
    char modebuf[8] = {0, 0};
    FILE *fp = 0;
    Value ans = HASH_F;
    Value readproc = act->codeh->i;
    Value writeproc = act->codeh->j;
    Value closeproc = act->codeh->k;
    DEMAND_ARGS(2);
    DEMAND(sc_get_string_chars(pathbuf, sizeof(pathbuf), a), a);
    DEMAND(sc_get_string_chars(modebuf, sizeof(modebuf), b), b);
    fp = fopen(pathbuf, modebuf);
    if (fp) {
        Whatever dat = sc_new_box(code_filep, fp);
        bool rd = modebuf[0] == 'r' || modebuf[1] == '+';
        bool wr = modebuf[0] != 'r' || modebuf[1] == '+';
        ans = sc_new_Port(a, VAL(dat),
                          rd ? readproc : HASH_F,
                          wr ? writeproc : HASH_F,
                          closeproc);
    }
    RETURN(ans);
}

Activation scp_splain(Activation act, ThData th)
{
    Value a = act->a;
    char *ty = "?";
    Value ans = EMPTY_LIST;
    DEMAND_ARGS(1);
    switch (SC_GET_CODE(a)) {
    case code_undefined:
        if (a == UNDEF)
            ty = "undefined";
        if (a == NYI)
            ty = "nyi";
        if (a == WRONG_TYPE)
            ty = "wrong-type";
        if (a == MULTVALUES)
            ty = "multiple-values";
        break;
    case code_boolean:
        ty = "boolean";
        break;
    case code_empty_list:
        ty = "empty_list";
        break;
    case code_pair:
        ty = "pair";
        break;
    case code_character:
        ty = "character";
        break;
    case code_fixnum:
        ty = "fixnum";
        break;
    case code_float64:
        ty = "float64";
        break;
    case code_bignum:
        ty = "bignum";
        ans = CONS(((Bignum) a)->flags, CONS(((Bignum) a)->exponent, ((Bignum) a)->mag));
        break;
    case code_rational:
        ty = "rational";
        ans = CONS(((Rational) a)->flags, CONS(((Rational) a)->numerator, CONS(((Rational) a)->denominator, ans)));
        break;
    case code_complex:
        ty = "complex";
        ans = CONS(((Complex) a)->flags, CONS(((Complex) a)->real_part, CONS(((Complex) a)->imag_part, ans)));
        break;
    case code_number:
        ty = "number";
        break;
    case code_nan:
        ty = "nan";
        break;
    case code_string:
        ty = "string";
        ans = CONS(((String) a)->length, ans);
        break;
    case code_symbol:
        ty = "symbol";
        break;
    case code_vector:
        ty = "vector";
        ans = CONS(((Vector) a)->length, ans);
        break;
    case code_port:
        ty = "port";
        break;
    case code_proc:
        ty = "proc";
        break;
    case code_activation:
        ty = "activation";
        break;
    case code_thread_data:
        ty = "thread-data";
        break;
    case code_box:
        ty = "box";
        break;
    case code_filep:
        ty = "filep";
        break;
    default:
        ty = "code";
        break;
    }
    ans = CONS((Value) sc_symlit(ty), ans);
    RETURN(ans);
}

Activation scp_PCT_gcollect(Activation act, ThData th)
{
    sc_collect(act, th);
    RETURN(UNSPEC);
}
