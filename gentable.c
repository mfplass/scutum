#include <stdio.h>
#include <stdlib.h>
#include <sys/utsname.h>
#include "mm_sys.h"

static int expand_to_fit = 1;

int main (int argc, char **argv) {
   int i = 0;
   int j = 0;
   char *dlm = 0;
   int q[MM_MAX_QUANTA+5];
   struct utsname u;
   size_t words = sizeof(void *);
   uname(&u);
   printf("/* mm_static_tables.h is generated */\n");
   printf("/* do not hand edit */\n");
   printf("/* generated for %s on %s */\n",
          u.sysname,
          u.machine);
   i = 4;
   q[0] = sizeof(void *);
   q[1] = 2*q[0];
   q[2] = 4*q[0];
   q[3] = 6*q[0];
   while (i < MM_MAX_QUANTA) {
      q[i] = (q[i-1]+q[i-2]+2*q[i-3]+31)/32*16;
      i+=1;
   }
   /* the following expands the sizes to minimize waste assuming the
   // large blocks are allocated in multiples of 2K words.           */
   i = 0;
   while (expand_to_fit && i < MM_MAX_QUANTA) {
      unsigned sz = 0;
      unsigned prefix_sz = 2*words;
      unsigned m = 2*words*1024-1;
      sz = q[i];
      while (1) {
         unsigned n = 1;
         uintptr_t k = m;
         /* c.f. mm_choose_blk_n */
         while (k-sizeof(mm_sys_blk_t)-2*words <= 2*sz) k = 2*k+1;
         k += 1;
         n = k / (sz+1);
         while (MM_TOTAL_BLK_SZ(prefix_sz,sz,n+1,0) < k) n++;
         k = MM_TOTAL_BLK_SZ(prefix_sz,sz,n,0);
         if (((k+m)&~m) < MM_TOTAL_BLK_SZ(prefix_sz,sz+2*words,n,0)) break;
         sz = sz + 2*words;
      }
      q[i] = sz;
      i+=1;
   }
   i = 0;
   printf("static const size_t mm_quantum_sz[MM_MAX_QUANTA] = {");
   while (i < MM_MAX_QUANTA-1) {
      if (i % 5 == 0) {
          printf("\n     ");
      }
      printf(" %5d,", q[i]);
      i++;
   }
   printf(" %5d};\n\n", q[MM_MAX_QUANTA-1]);
   printf("static const uint8_t mm_q_from_wsz[] = {");
   i=0;
   j=0;
   dlm="";
   while (j < MM_MAX_QUANTA-2) {
      while (i*sizeof(void *) > q[j]) j++;
      if (i % 10 == 0) {
          printf("%s\n /* %5d */ ", dlm, i);
          dlm="";
      };
      printf("%s%3d", dlm, j);
      dlm=",";
      i++;
   }
   printf(" };\n\n");
   exit(0);
}

