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
#include <stdint.h>
#include "mm.h"

struct mm_sys_blk;
struct mm_lba_class;

typedef uintptr_t marknscan_t;    /* holds packed mark, scan bits */
#define MM_MARKS_PER_WORD (8*sizeof(marknscan_t)/2)
#define MM_HIGH_MARK (((uintptr_t)(1)) << (8*sizeof(marknscan_t)-1))
#define MM_HIGH_SCAN (((uintptr_t)(1)) << (8*sizeof(marknscan_t)-2))

#define MM_SYS_BLK_SG 0xABED
typedef struct mm_sys_blk {
   unsigned short      signature; /* signature for sanity check */
   unsigned short      n_ob;      /* number of objects in this blk */
   unsigned short      prefix0sz; /* bytes from block base to first prefix */
   unsigned short      reserved;  /* reserved for flags */
   struct mm_sys_blk * left;      /* binary tree left link */
   struct mm_sys_blk * right;     /* right link - address order */
   struct mm_sys_blk * next;      /* next one with same parameters */
   struct mm_ob_class *ob_class;  /* methods applicable to contained objects */
   size_t              blk_sz;    /* size of this block, including header */
   size_t              obj_sz;    /* uniform size of objects in this block */
   uintptr_t           avail;     /* avail list within this blk (offsets) */
   marknscan_t         mksc[1];   /* packed mark, scan bits (may be more than 1) */ 
} mm_sys_blk_t;

/* macros for computing offsets and sizes */
/* MM_BLK_HDR_SZ is the number of bytes needed up to the mark & scan bits */
#define MM_BLK_HDR_SZ (sizeof(mm_sys_blk_t)-sizeof(marknscan_t))
/* MM_MARK_SZ is the number of bytes needed for the mark & scan bits */
#define MM_MARK_SZ(n) (((2*(n)+8*sizeof(marknscan_t)-1)/(8*sizeof(marknscan_t)))*sizeof(marknscan_t))
#define S7 (2*sizeof(void*)-1)
/* MM_FIRST_OB finds the offset to the first object; a is alignment desired */
#define MM_FIRST_OB(pz,n,a) (((MM_BLK_HDR_SZ+MM_MARK_SZ(n)+pz+S7-a)&~S7)+a)
/* MM_TOTAL_BLK_SZ gives the total byte size for a blk: pz=prefix_sz, tsz=obj_sz (includes prefix) */
#define MM_TOTAL_BLK_SZ(pz,tsz,n,a) (MM_FIRST_OB(pz,n,a)-(pz)+(n)*(tsz))

/* The mm_ob_mark_ptrs_fn should call mm_mark_one for every possible 
// pointer in the object pointed to by p.                            */
typedef void (*mm_ob_mark_ptrs_fn)(
    mm_sys_blk_t *blk,  /* the blk containing the object             */
    uintptr_t    *p     /* points to allocated object (not prefix)   */
);

/* This next part is for registering marker procs for roots */
struct mm_sys_root_marker;
typedef void (*mm_sys_root_marker_fn)(struct mm_sys_root_marker * self);
typedef struct mm_sys_root_marker {
   mm_sys_root_marker_fn     root_marker;
   void                      *data;        /* client data */
   uintptr_t                  num;         /* also for client use */
   struct mm_sys_root_marker *next;
   struct mm_sys_root_marker *prev;
} mm_sys_root_marker_t;

/* mm_mark_one should only be called during the mark phase of a collection,
// normally from one of the two types of procedures above.                  */
extern void mm_mark_one(uintptr_t q, unsigned mark_flags);
#define MM_MARK_FLAG_INTERNAL 1   /* q may point to interior or prefix of object */

/* The next two are for registering or unregistering root-marker closures. */
/* The storage provided by the caller becomes part of the data structure.  */
extern void mm_register_root_marker(mm_sys_root_marker_t *);
extern void mm_unregister_root_marker(mm_sys_root_marker_t *);

/* For single-threaded applications on most platforms, the following should */
/* be called from one of the registered root-marker procedures.  It needs   */
/* an address near the cold end of the stack which should be saved during   */
/* initialization.                                                          */
extern void mm_generic_mark_my_stack(volatile void *coldpoint);

#define MM_MAX_QUANTA 20
#define MM_OB_CLASS_SC 0x7FF0BC15
typedef struct mm_ob_class {
   uintptr_t           signature; /* signature for sanity check */
   mm_ob_mark_ptrs_fn  marker;    /* records possible ptrs within object */
   size_t              prefix_sz; /* size of object prefixes */
   size_t              alignment; /* for controlling odd alignment needs */
   struct mm_lba_class*lba;       /* large block allocator (NULL=>default) */
   struct mm_sys_blk * quant[MM_MAX_QUANTA];
} mm_ob_class_t;

typedef struct mm_lba_class {
   void * (*alloc_fn)(size_t); /* allocate large block of given size */
   void (*free_fn)(mm_sys_blk_t *); /* free the same */
   size_t      granularity;    /* natural size unit of lba (e.g., pagesize) */
   size_t      overhead;       /* per large block */
   int         new_is_clear; /* true if newly-allocated memory is always all zero */
} mm_lba_class_t;

extern mm_lba_class_t *mm_default_lba;

#define set_run_exclusive (void)
