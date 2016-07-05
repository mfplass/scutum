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
/* mm.c */
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#undef NDEBUG
#include <assert.h>
#include <setjmp.h>
#include "mm_sys.h"
#include "mm_static_tables.h"

/*
 * The next link is used during the marking phase to keep track of // the
 * blks that contain objects that have been marked but not yet // scanned.
 * This list is terminated with the nonzero NOMORE value // so we can tell
 * easily whether a given blk is on the list. // The normal chains (by size
 * within object class) are constructed // during the sweep phase.
 */

static pthread_mutex_t mm_lock = PTHREAD_MUTEX_INITIALIZER;
static mm_sys_blk_t *mm_splay_tree = 0;
#define NOMORE ((mm_sys_blk_t *)(-1))
static mm_sys_blk_t *mm_needs_scan = NOMORE;
static mm_sys_root_marker_t mm_head_root_marker =
{0, 0, 0, &mm_head_root_marker, &mm_head_root_marker};
static uintptr_t mm_growth_since_collect = 0;

static int mm_quantize(size_t sz);
static mm_sys_blk_t *mm_new_blk(mm_ob_class_t * oc, size_t sz);
static unsigned mm_link_blk_avail_list(mm_sys_blk_t *);

typedef mm_sys_blk_t *st_node_t;
typedef uintptr_t st_key_t;
#define ST_KEYVAL(s) ((st_key_t)(s))
/*
 * splay: Sleator-Tarjan splay tree implementation. // The binary tree s is
 * rebalanced so that a node with key k becomes // the new root; if none such
 * exists, then dummy becomes the new // root and caller needs to ensure that
 * it is initialized.
 */
static st_node_t splay(st_node_t s, st_key_t k, st_node_t dummy)
{
    int state = 0;              /* -1 = L, 0 = N, 1 = R */
    st_node_t l, r, p;
    st_key_t sk;
    l = r = dummy;
    p = 0;
    dummy->left = dummy->right = 0;
    while (s) {
        sk = ST_KEYVAL(s);
        if (sk < k) {
            if (state >= 0) {
                l->right = s;
                p = l;
                l = s;
                s = s->right;
                state = -1;
            }
            else {
                l->right = s->left;
                p->right = s;
                s->left = l;
                p = 0;
                l = s;
                s = s->right;
                state = 0;
            }
        }
        else if (sk > k) {
            if (state <= 0) {
                r->left = s;
                p = r;
                r = s;
                s = s->left;
                state = +1;
            }
            else {
                r->left = s->right;
                p->left = s;
                s->right = r;
                p = 0;
                r = s;
                s = s->left;
                state = 0;
            }
        }
        else {                  /* (sk == k) */
            l->right = s->left;
            r->left = s->right;
            s->left = dummy->right;
            s->right = dummy->left;
            dummy->left = dummy->right = 0;
            return s;
        }
    }
    l->right = r->left = 0;
    l = dummy->right;
    r = dummy->left;
    s = dummy;
    s->left = l;
    s->right = r;
    return s;
}

/*
 * splay_extant: bring node near k to top // The new root will have the
 * largest existing key <= k, // or, if none such, it will have the least
 * key.
 */
static st_node_t splay_extant(st_node_t s, st_key_t k)
{
    mm_sys_blk_t d;             /* temp dummy */
    st_node_t t = 0;
    st_node_t l = 0;
    st_node_t r = 0;
    st_key_t sk;
    if (!s)
        return s;
    t = s;
    while (t) {
        assert(t->signature == MM_SYS_BLK_SG);
        sk = ST_KEYVAL(t);
        if (sk < k) {
            l = t;
            t = t->right;
        }
        else if (sk > k) {
            r = t;
            t = t->left;
        }
        else
            return splay(s, k, &d);
    }
    if (l)
        return splay(s, ST_KEYVAL(l), &d);
    assert(r && !r->left);
    return splay(s, ST_KEYVAL(r), &d);
}

/*
 * st_right: find next node in order, without changing tree
 */
static st_node_t st_right(st_node_t s)
{
    if (!s)
        return s;
    s = s->right;
    if (!s)
        return s;
    while (s->left)
        s = s->left;
    return s;
}

void *mm_culprit = 0;
static void *mm_unlocked_alloc(mm_ob_class_t * oc, size_t sz)
{
    void *ans = 0;
    size_t p_sz = 0;            /* prefix size */
    unsigned qi = 0;            /* quantum index for small sizes */
    size_t q_sz = 0;            /* quantized size, including prefix */
    mm_sys_blk_t *blk = 0;
    p_sz = oc->prefix_sz;
    sz += p_sz;
    if (sz > mm_quantum_sz[MM_MAX_QUANTA - 1]) {
        size_t blk_sz, pad, a;
        a = oc->alignment;
        blk_sz = MM_TOTAL_BLK_SZ(p_sz, sz, 1, a);
        pad = ((blk_sz + 0x3f) & ~0x3f) - blk_sz;
        q_sz = sz + pad;
        goto NeedAlloc;
    }
    qi = mm_quantize(sz);
    blk = oc->quant[qi];
    q_sz = mm_quantum_sz[qi];
    while (blk) {
        mm_sys_blk_t *nxt = 0;
        uintptr_t d = 0;
        mm_culprit = blk;
        assert(blk->signature == MM_SYS_BLK_SG);
        d = blk->avail;         /* avail links are byte offsets from blk */
        if (d) {
            assert(d >= blk->prefix0sz);
            assert(d + sz <= blk->blk_sz);
            ans = (char *) blk + d;
            blk->avail = *((uintptr_t *) ans);
            goto Ok;
        }
        nxt = blk->next;
        blk->next = 0;
        oc->quant[qi] = blk = nxt;
    }
NeedAlloc:
    blk = mm_new_blk(oc, q_sz);
    if (!blk)
        goto Fail;
    ans = (char *) blk + blk->avail;
    blk->avail = *((uintptr_t *) ans);
Ok:
    ((uintptr_t *) ans)[0] = 0; /* always clear link we used for avail */
    memset(((char *) ans) - p_sz, 0, q_sz);     /* should be redundant */
Fail:
    return ans;
}

void *mm_alloc(mm_ob_class_t * oc, size_t sz)
{
    void *ans = 0;
    pthread_mutex_lock(&mm_lock);
    ans = mm_unlocked_alloc(oc, sz);
    pthread_mutex_unlock(&mm_lock);
    return ans;
}

int mm_alloc_several(mm_ob_class_t * oc, size_t sz, int count, void **result)
{
    int n = 0;
    pthread_mutex_lock(&mm_lock);
    while (n < count) {
        void *ob = mm_unlocked_alloc(oc, sz);
        if (!ob)
            break;
        result[n++] = ob;
    }
    pthread_mutex_unlock(&mm_lock);
    return n;
}

static int mm_quantize(size_t sz)
{
    int q = MM_MAX_QUANTA;
    unsigned w;
    w = (sz + sizeof(void *) - 1) / sizeof(void *);
    if (w < sizeof(mm_q_from_wsz) / sizeof(mm_q_from_wsz[0])) {
        return (mm_q_from_wsz[w]);
    }
    while (q > 0 && mm_quantum_sz[q - 1] >= sz)
        q -= 1;
    assert(q < MM_MAX_QUANTA);
    return q;
}

static unsigned mm_choose_blk_n(unsigned prefix_sz, unsigned tsz, unsigned a)
{
    unsigned n = 1;
    unsigned e = 2 * sizeof(void *);
    uintptr_t k = e * 1024 - 1;
    if (tsz <= mm_quantum_sz[MM_MAX_QUANTA - 1]) {
        while (k - sizeof(mm_sys_blk_t) - e <= 2 * tsz)
            k = 2 * k + 1;
        k += 1;
        n = k / (tsz + 1);
        /* CHECK THIS - does a!=0 throw this off? */
        while (MM_TOTAL_BLK_SZ(prefix_sz, tsz, n + 1, a) < k)
            n++;
    }
    return n;
}

static mm_sys_blk_t *mm_new_blk(mm_ob_class_t * oc, size_t tsz)
{
    mm_lba_class_t *lba = 0;
    mm_sys_blk_t *ans = 0;
    uintptr_t k = 0;
    uintptr_t n = 1;
    uintptr_t pz = 0;
    int q = -1;
    size_t a = 0;
    uintptr_t adj = 0;
    uintptr_t slop = 0;
    lba = oc->lba;
    if (!lba) {
        oc->lba = lba = mm_default_lba;
    }
    pz = oc->prefix_sz;
    a = oc->alignment;
    n = mm_choose_blk_n(pz, tsz, a);    /* note - tsz has already been
                                         * quantized */
    for (;;) {
        adj = 0;
        k = MM_TOTAL_BLK_SZ(pz, tsz, n, a) + slop;
        ans = (*(lba->alloc_fn)) (k);
        if (!ans)
            return 0;
        /* The rest of this loop ensures proper alignment */
        if (tsz <= S7 || 0 == (S7 & (uintptr_t) ans))
            break;
        adj = S7 + 1 - (S7 & (uintptr_t) ans);
        if (adj <= slop)
            break;
        (*(lba->free_fn)) (ans);
        slop += sizeof(void *);
    }
    mm_growth_since_collect += k;
    if (!(lba->new_is_clear))
        memset(ans, 0, k);
    ans->signature = MM_SYS_BLK_SG;
    if (n > 1) {
        q = mm_quantize(tsz);
        ans->next = oc->quant[q];
        oc->quant[q] = ans;
    }
    ans->ob_class = oc;
    ans->blk_sz = k;
    ans->obj_sz = tsz;
    ans->n_ob = n;
    ans->prefix0sz = adj + MM_FIRST_OB(pz, n, a) - pz;
    mm_link_blk_avail_list(ans);
    mm_splay_tree = splay(mm_splay_tree, (uintptr_t) ans, ans);
    return (ans);
}

static unsigned mm_link_blk_avail_list(mm_sys_blk_t * blk)
{
    uintptr_t t = 0;
    unsigned i = 0;
    unsigned n = 0;
    unsigned n_avail = 0;
    marknscan_t m = 0;
    size_t sz = 0;
    size_t p_sz = 0;
    uintptr_t *p = 0;
    sz = blk->obj_sz;
    p_sz = blk->ob_class->prefix_sz;
    t = blk->prefix0sz + p_sz;
    n = blk->n_ob;
    p = (uintptr_t *) (&(blk->avail));
    while (i < n) {
        if (i % MM_MARKS_PER_WORD == 0) {
            m = blk->mksc[i / MM_MARKS_PER_WORD];
        }
        else {
            m = (m << 2);
        }
        if (!(m & MM_HIGH_MARK)) {
            memset((char *) blk + t - p_sz, 0, sz);
            *p = t;
            p = (uintptr_t *) ((char *) blk + t);
            n_avail += 1;
        }
        t += sz;
        i++;
    }
    *p = 0;
    return n_avail;
}

void mm_register_root_marker(mm_sys_root_marker_t * a)
{
    mm_sys_root_marker_t *head = &mm_head_root_marker;
    pthread_mutex_lock(&mm_lock);
    a->next = head;
    a->prev = head->prev;
    a->prev->next = a;
    a->next->prev = a;
    pthread_mutex_unlock(&mm_lock);
}

void mm_unregister_root_marker(mm_sys_root_marker_t * a)
{
    pthread_mutex_lock(&mm_lock);
    a->prev->next = a->next;
    a->next->prev = a->prev;
    a->next = a->prev = 0;
    pthread_mutex_unlock(&mm_lock);
}

static void mm_mark_setup(void)
{
    mm_sys_blk_t *st = 0;
    mm_ob_class_t *oc = 0;
    mm_ob_class_t *oc1 = 0;
    mm_ob_class_t *oc2 = 0;
    mm_ob_class_t *oc3 = 0;
    unsigned n = 0;
    mm_needs_scan = NOMORE;
    mm_splay_tree = st = splay_extant(mm_splay_tree, 0);
    while (st) {
        mm_splay_tree = st = splay_extant(mm_splay_tree, ST_KEYVAL(st));
        oc = st->ob_class;
        n = st->n_ob;
        st->next = 0;
        if (oc != oc1 && oc != oc2 && oc != oc3) {
            memset(oc->quant, 0, sizeof(oc->quant));
            (oc1 = oc2, oc2 = oc3, oc3 = oc);
        }
        memset(st->mksc, 0, MM_MARK_SZ(n));
        st = st_right(st);
    }
}

static void mm_mark_from_roots(void)
{
    mm_sys_root_marker_t *head = &mm_head_root_marker;
    mm_sys_root_marker_t *p;
    p = head->next;
    while (p != head) {
        (*(p->root_marker)) (p);
        p = p->next;
    }
}

static void mm_generic_mark_stack_inner(sigjmp_buf env,
                                        volatile void *cold,
                                        volatile uintptr_t * warm)
{
    volatile uintptr_t here = 0;
    uintptr_t lo = (uintptr_t) & here;
    uintptr_t hi = (uintptr_t) & here;
    uintptr_t *p = 0;
    if (((uintptr_t) cold) < lo) {
        lo = ((uintptr_t) cold) & ~1023;
    }
    else {
        hi = ((uintptr_t) cold) | (1024 - sizeof(uintptr_t));
    }
    p = (void *) lo;
    while ((uintptr_t) p < hi) {
        mm_mark_one(p[0], MM_MARK_FLAG_INTERNAL);
        p++;
    }
    *warm = hi - lo;            /* This is to disallow a tail call */
    (void) env;                 /* otherwise unused */
}

void mm_generic_mark_my_stack(volatile void *coldpoint)
{
    struct {
        sigjmp_buf env;
        volatile uintptr_t dummy;
    } datablock;
    memset(&datablock, 0, sizeof(datablock));
    datablock.dummy = sigsetjmp(datablock.env, 0);
    mm_generic_mark_stack_inner(datablock.env, coldpoint, &datablock.dummy);
    if (!datablock.dummy)
        abort();
}

static void mm_scan(void)
{
    mm_sys_blk_t *p = 0;
    uintptr_t r = 0;
    uintptr_t sz = 0;
    unsigned n = 0;
    marknscan_t m = 0;
    marknscan_t *mb = 0;
    while (mm_needs_scan != NOMORE) {
        unsigned i = 0;
        p = mm_needs_scan;
        mm_needs_scan = p->next;
        p->next = 0;
        n = p->n_ob;
        sz = p->obj_sz;
        r = (uintptr_t) p + p->prefix0sz + p->ob_class->prefix_sz;
        while (i < n) {
            m = MM_HIGH_MARK >> (2 * (i % MM_MARKS_PER_WORD));
            mb = p->mksc + (i / MM_MARKS_PER_WORD);
            if (((*mb) & m) > (((*mb) * 2) & m)) {
                (*mb) |= (m >> 1);
                (*p->ob_class->marker) (p, (uintptr_t *) r);
            }
            r += sz;
            i += 1;
        }
    }
}

static void mm_scan_object(void *obj)
{
    /* scan an object, even if it is not marked */
    /* useful for finalization */
    uintptr_t q = 0;
    q = (uintptr_t) obj;
    if (q) {
        mm_sys_blk_t *r = 0;
        uintptr_t base;
        mm_splay_tree = r = splay_extant(mm_splay_tree, q);
        if (r) {
            base = (uintptr_t) r + r->prefix0sz;
            if (base <= q && q < (uintptr_t) r + r->blk_sz) {
                unsigned i;
                marknscan_t m;
                marknscan_t *mb;
                i = (q - base) / (r->obj_sz);   /* expensive divide */
                m = MM_HIGH_SCAN >> (2 * (i % MM_MARKS_PER_WORD));
                mb = r->mksc + (i / MM_MARKS_PER_WORD);
                if (!((*mb) & m)) {
                    if (q == base + (r->ob_class->prefix_sz) + i * (r->obj_sz)) {
                        (*mb) |= m;
                        (*r->ob_class->marker) (r, (uintptr_t *) q);
                    }
                }
            }
        }
    }
}


static unsigned mm_mark_scan_access(void *obj, unsigned bot, unsigned top)
{
    /* examine and/or change mark and scan bits of an object */
    /* encoding is mark * 2 + scan */
    /* change is bits = (bits | bot) & top */
    /* result is old encoding + 4 if obj is valid (might be interior pointer) */
    unsigned oldbits = 0;
    uintptr_t q = 0;
    q = (uintptr_t) obj;
    if (q) {
        mm_sys_blk_t *r = 0;
        uintptr_t base;
        mm_splay_tree = r = splay_extant(mm_splay_tree, q);
        if (r) {
            base = (uintptr_t) r + r->prefix0sz;
            if (base <= q && q < (uintptr_t) r + r->blk_sz) {
                unsigned i;
                marknscan_t mbot;
                marknscan_t mtop;
                marknscan_t *mb;
                i = (q - base) / (r->obj_sz);   /* expensive divide */
                mbot = (((marknscan_t) bot) << (2 * (MM_MARKS_PER_WORD - 1))) >> (2 * (i % MM_MARKS_PER_WORD));
                mtop = ~((((marknscan_t) ~ top) << (2 * (MM_MARKS_PER_WORD - 1))) >> (2 * (i % MM_MARKS_PER_WORD)));
                mb = r->mksc + (i / MM_MARKS_PER_WORD);
                oldbits = 4 + (((*mb) >> (2 * (MM_MARKS_PER_WORD - 1 - (i % MM_MARKS_PER_WORD)))) & 3);
                (*mb) = ((*mb) | mbot) & mtop;
            }
        }
    }
    return oldbits;
}

static int mm_obj_is_marked(void *obj)
{
    return ((mm_mark_scan_access(obj, 0, 3) >> 1) & 1);
}

static void mm_mark_object(void *obj)
{
    mm_mark_scan_access(obj, 2, 3);
}

static void mm_unmark_object(void *obj)
{
    mm_mark_scan_access(obj, 0, 1);
}

static int mm_about_to_collect(void *obj)
{
    return ((mm_mark_scan_access(obj, 0, 3) & 6) == 4);
}

void mm_mark_one(uintptr_t q, unsigned mark_flags)
{
    if (q) {
        mm_sys_blk_t *r = 0;
        uintptr_t base;
        mm_splay_tree = r = splay_extant(mm_splay_tree, q);
        if (r) {
            base = (uintptr_t) r + r->prefix0sz;
            if (base <= q && q < (uintptr_t) r + r->blk_sz) {
                unsigned i;
                marknscan_t m;
                marknscan_t *mb;
                i = (q - base) / (r->obj_sz);   /* expensive divide */
                m = MM_HIGH_MARK >> (2 * (i % MM_MARKS_PER_WORD));
                mb = r->mksc + (i / MM_MARKS_PER_WORD);
                if (!((*mb) & m)) {
                    if ((mark_flags & MM_MARK_FLAG_INTERNAL) ||
                        (q == base + (r->ob_class->prefix_sz) + i * (r->obj_sz))) {
                        (*mb) |= m;
                        if (!r->next) {
                            r->next = mm_needs_scan;
                            mm_needs_scan = r;
                        }
                    }
                }
            }
        }
    }
}

typedef struct finalizer {
    void *obj;
    void (*finalize) (void *obj, void *clientData);
    void *clientData;
    struct finalizer *next;
} finalizer_t;

static finalizer_t *mm_finalizers = 0;
static finalizer_t *mm_weaklinks = 0;

static void mm_tr_fin(mm_sys_blk_t * blk, uintptr_t * p)
{
    finalizer_t *f = 0;
    f = (void *) p;
    /* do not mark f->obj, or it will never get collected */
    if (f->finalize) {
        mm_mark_one((uintptr_t) (f->finalize), MM_MARK_FLAG_INTERNAL);
        mm_mark_one((uintptr_t) (f->clientData), MM_MARK_FLAG_INTERNAL);
    }
    mm_mark_one((uintptr_t) (f->next), 0);
    (void) blk;                 /* otherwise unused */
}

static struct mm_ob_class mm_fin_class = {MM_OB_CLASS_SC, &mm_tr_fin, 0, 0, 0, {0}};

static void mm_rem_from_finlist(void *obj, finalizer_t ** lst)
{
    finalizer_t *f = 0;
    finalizer_t *prev = 0;
    finalizer_t *next = 0;
    pthread_mutex_lock(&mm_lock);
    prev = 0;
    f = *lst;
    while (f) {
        next = f->next;
        if (obj == f->obj) {
            ((void **) (f->clientData))[0] = 0;
            if (prev) {
                prev->next = next;
            }
            else {
                *lst = next;
            }
        }
        else {
            prev = f;
        }
        f = next;
    }
    pthread_mutex_unlock(&mm_lock);
    next = prev = 0;
}

void mm_register_finalizer(void *obj, void (*finalize) (void *obj, void *clientData), void *clientData) {
    finalizer_t *f = 0;
    if (!finalize) {
        mm_rem_from_finlist(obj, &mm_finalizers);
        return;
    }
    f = mm_alloc(&mm_fin_class, sizeof(*f));
    f->obj = obj;
    f->finalize = finalize;
    f->clientData = clientData;
    pthread_mutex_lock(&mm_lock);
    if (4 == (4 & mm_mark_scan_access(obj, 0, 3))) {
        f->next = mm_finalizers;
        mm_finalizers = f;
    }
    pthread_mutex_unlock(&mm_lock);
}

void mm_register_weak_link(void *obj, void **target)
{
    finalizer_t *f = 0;
    if (!target) {
        mm_rem_from_finlist(obj, &mm_finalizers);
        return;
    }
    *(volatile void **) target = *target;       /* check writeability */
    f = mm_alloc(&mm_fin_class, sizeof(*f));
    f->obj = obj;
    f->finalize = 0;
    f->clientData = target;
    pthread_mutex_lock(&mm_lock);
    if (4 == (4 & mm_mark_scan_access(obj, 0, 3))) {
        f->next = mm_weaklinks;
        mm_weaklinks = f;
    }
    pthread_mutex_unlock(&mm_lock);
}

static void mm_invoke_finalizers(void)
{
    finalizer_t *f = 0;
    finalizer_t *prev = 0;
    finalizer_t *next = 0;
    mm_mark_one((uintptr_t) mm_finalizers, 0);
    mm_mark_one((uintptr_t) mm_weaklinks, 0);
    mm_scan();
    /* first process the disappearing links */
    prev = 0;
    f = mm_weaklinks;
    while (f) {
        next = f->next;
        if (!mm_obj_is_marked(f->obj)) {
            ((void **) (f->clientData))[0] = 0;
            if (prev) {
                prev->next = next;
            }
            else {
                mm_weaklinks = next;
            }
        }
        else {
            prev = f;
        }
        f = next;
    }
    /* scan the finalizable objects and determine order */
    f = mm_finalizers;
    while (f) {
        if (!mm_obj_is_marked(f->obj)) {
            mm_scan_object(f->obj);
            mm_scan();
            if (mm_obj_is_marked(f->obj)) {
                /* finalization cycle - break it */
                mm_unmark_object(f->obj);
            }
        }
        f = f->next;
    }
    /* invoke the finalizers */
    prev = 0;
    f = mm_finalizers;
    while (f) {
        next = f->next;
        if (!mm_obj_is_marked(f->obj)) {
            mm_mark_object(f->obj);
            (*f->finalize) (f->obj, f->clientData);
            if (prev) {
                prev->next = next;
            }
            else {
                mm_finalizers = next;
            }
            mm_unmark_object(f);
        }
        else {
            prev = f;
        }
        f = next;
    }
    /* remove weak links with dead targets */
    prev = 0;
    f = mm_weaklinks;
    while (f) {
        next = f->next;
        if (mm_about_to_collect(f->clientData)) {
            if (prev) {
                prev->next = next;
            }
            else {
                mm_weaklinks = next;
            }
        }
        else {
            prev = f;
        }
        f = next;
    }
    next = 0;
}

mm_sys_blk_t *mm_freewatch = 0;
static mm_sys_blk_t *mm_debugfree_fn(mm_sys_blk_t * blk)
{
    return blk;
}

mm_sys_blk_t *(*mm_debugfree) (mm_sys_blk_t * blk) = &mm_debugfree_fn;

static void *mm_release_blk(mm_sys_blk_t * blk)
{
    assert(blk->signature == MM_SYS_BLK_SG);
    assert(blk->next == 0);
    assert(blk->left == 0);
    assert(blk->right == 0);
    if (blk == mm_freewatch) {
        blk = mm_debugfree(blk);
    }
    if (blk)
        (*(blk->ob_class->lba->free_fn)) (blk);
    return 0;
}

static void mm_sweep(void)
{
    mm_sys_blk_t *st = 0;
    mm_sys_blk_t *del = 0;
    unsigned n_avail = 0;
    mm_splay_tree = st = splay_extant(mm_splay_tree, 0);
    while (st) {
        mm_splay_tree = st = splay_extant(mm_splay_tree, ST_KEYVAL(st));
        if (del && del == st->left && !del->right) {
            st->left = del->left;
            del->left = 0;
            del = mm_release_blk(del);
        }
        n_avail = mm_link_blk_avail_list(st);
        if (st->avail) {
            if (st->n_ob == n_avail) {
                assert(st->signature == MM_SYS_BLK_SG && !(st->next));
                del = st;
            }
            else {              /* subtle, but we know st->n_ob > 1 here */
                unsigned qi;
                qi = mm_quantize(st->obj_sz);
                st->next = st->ob_class->quant[qi];
                assert((!(st->next)) || st->next->signature == MM_SYS_BLK_SG);
                st->ob_class->quant[qi] = st;
            }
        }
        st = st_right(st);
    }
    if (del && del == mm_splay_tree && !del->right) {
        mm_splay_tree = del->left;
        del->left = 0;
        del = mm_release_blk(del);
    }
}

static void mm_collect_internal(void)
{
    set_run_exclusive(1);
    mm_mark_setup();
    mm_mark_from_roots();
    mm_scan();
    mm_invoke_finalizers();
    mm_sweep();
    set_run_exclusive(0);
    mm_growth_since_collect = 0;
}

void mm_collect(void)
{
    pthread_mutex_lock(&mm_lock);
    mm_collect_internal();
    pthread_mutex_unlock(&mm_lock);
}

size_t mm_get_heap_growth(void)
{
    uintptr_t ans = 0;
    pthread_mutex_lock(&mm_lock);
    ans = mm_growth_since_collect;
    pthread_mutex_unlock(&mm_lock);
    return ans;
}

static void tr_none(mm_sys_blk_t * blk, uintptr_t * p)
{
    /* trace routine - ptrfree objects */
    (void) blk;
    (void) p;
}

static void tr_norm(mm_sys_blk_t * blk, uintptr_t * p)
{
    /* trace routine - trace all */
    unsigned n = 0;
    n = ((uintptr_t) ((blk->obj_sz) - (blk->ob_class->prefix_sz))) /
        sizeof(uintptr_t);
    while (n) {
        mm_mark_one(*p, MM_MARK_FLAG_INTERNAL);
        p++;
        n--;
    }
}

static struct mm_ob_class class_none = {MM_OB_CLASS_SC, &tr_none, 0, 0, 0, {0}};
static struct mm_ob_class class_norm = {MM_OB_CLASS_SC, &tr_norm, 0, 0, 0, {0}};
static struct mm_ob_class class_odda = {MM_OB_CLASS_SC, &tr_norm, 0, sizeof(uintptr_t), 0, {0}};
mm_kind_t mm_normal = &class_norm;
mm_kind_t mm_ptrfree = &class_none;
mm_kind_t mm_pairs = &class_odda;

static void mm_sys_free_blk(mm_sys_blk_t * blk)
{
    free(blk);
}

static struct mm_lba_class pfx8class = {&malloc, &mm_sys_free_blk, 8, 0, 0};
mm_lba_class_t *mm_default_lba = &pfx8class;


#if MM_TEST_MAIN
#include <stdio.h>
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
    a[1] = (int) c;
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
    return ((int) a + (int) b + (int) c + (int) list);
}
#endif
