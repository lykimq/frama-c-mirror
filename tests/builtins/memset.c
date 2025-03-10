/* run.config*
   STDOPT: #"-calldeps -eva-msg-key imprecision -eva-plevel 500 -absolute-valid-range 0x10-0xf0" +"-inout -no-deps"
*/

#include "string.h"
#include "stdlib.h"

int t1[100];
int t2[100];
int t3[100];
int t4[100];
int t5[100];
int t6[100];
int t7[100];
int t8[100];
int t9[100];
int t10[100];
int t11[100];
int t12[100];

struct s {
  char f1;
  short f2;
  int f3;
  int f4[3];
};

struct s ts[5];
extern struct incomplete *incomplete_type;
volatile int vol;

void test() {
  void * dst = memset(t1, 0x11, sizeof(t1)); // basic
  memset(t2+(int)t2, 0x12, sizeof(t2)); // garbled dest
  memset(t3+10, 0x11, (unsigned long)t1); // garbled size

  if (vol) {
    memset(t4+1, 1, sizeof(t4)); // out of bounds
  }

  memset(t5, (int)t1, sizeof(t4)); // garbled char

  int *p = vol ? t6+10 : t7;
  memset(p, 0x22, 16); // multiple dest

  p = vol ? (char*) 0 : t8;
  memset(p, 0x22, 16); // one valid dest;

  p = t9+20;
  while (1) {
    if (vol) break;
    p++;
  }
  memset(p, 0x8FE, 4); // imprecise dest

  unsigned long s = 12;
  if (vol) s += 24;
  memset(t10+4, 0x88, s); // imprecise size

  unsigned long s1 = 8;
  if (vol) s1 += 8;
  p = t11 + 2;
  if (vol)
    p++;
  memset(p, 0x99, s1); // imprecise dest+size with juxtaposition

  if (vol)
    memset(ts, 254, sizeof(ts));

  unsigned k = vol;
  //@ assert Assume: k <= 12;
  memset(t12+k*8, 1, 4); // Imprecise, because of double congruences

  memset(t1, 1, 0); // size is negative or null
  memset(incomplete_type, 'A', 1); // destination type and size differ
  char *absolute_valid_range = (char*)0x10;
  memset(absolute_valid_range, 'B', 2); // destination has an unknown form
}

/* Should not crash and emit uninitialization alarms.
   See gitlab public issue pub/frama-c#2576. */
void uninit() {
  void *x;
  if (vol)
    memset (x, 0, 2 * 4);
  int a;
  if (vol)
    x = &a;
  memset(x, 0, 4);
}

typedef struct S {
  int i;
  char c;
  int *ptr;
} st;

void memset_weak_base () {
  int x;
  /*@ eva_allocate fresh_weak; */
  st *s = malloc(sizeof(st));
  if (s == NULL) return;
  s->i = 421;
  s->c = 1;
  s->ptr = &x;
  memset(s, 0, sizeof(st));
  int *q = s->ptr; /* As the base is weak, s->ptr must be 0 or &x.
                      This should not be a garbled mix. */
  if (q != NULL) *q = 42; // This write should be valid.
}

void main() {
  test();
  uninit();
  memset_weak_base();
}
