#ifndef MM_H
#define MM_H
#include <stddef.h>

typedef struct mm_ob_class *mm_kind_t;

extern mm_kind_t mm_normal;
extern mm_kind_t mm_ptrfree;
extern mm_kind_t mm_pairs; /* odd word alignment, no prefix */

void * mm_alloc(mm_kind_t, size_t);
int mm_alloc_several(mm_kind_t, size_t, int, void **);

void mm_collect(void);
size_t mm_get_heap_growth(void);

void mm_register_finalizer(
     void *obj,
     void (*finalize)(void *obj, void *clientData),
     void *clientData);

void mm_register_weak_link(void *obj, void **target);

#endif
