/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

int A = 1;
int B = 2;
int C = 3;
void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __e_acsl_store_block((void *)(& B),4UL);
    __e_acsl_full_init((void *)(& B));
  }
  return;
}

void __e_acsl_globals_clean(void)
{
  __e_acsl_delete_block((void *)(& B));
  return;
}

int main(int argc, char **argv)
{
  int __retres;
  __e_acsl_memory_init(& argc,& argv,8UL);
  __e_acsl_globals_init();
  int *p = (int *)0;
  __e_acsl_store_block((void *)(& p),8UL);
  __e_acsl_full_init((void *)(& p));
  int *q = (int *)0;
  int a = 1;
  int b = 2;
  __e_acsl_store_block((void *)(& b),4UL);
  __e_acsl_full_init((void *)(& b));
  int c = 3;
  __e_acsl_full_init((void *)(& p));
  p = & b;
  {
    int __gen_e_acsl_initialized;
    int __gen_e_acsl_and;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_initialized = __e_acsl_initialized((void *)(& p),
                                                    sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"&p",
                                 (void *)(& p));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                   "sizeof(int *)",0,sizeof(int *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                 "\\initialized(&p)",0,
                                 __gen_e_acsl_initialized);
    if (__gen_e_acsl_initialized) {
      int __gen_e_acsl_valid;
      __gen_e_acsl_valid = __e_acsl_valid((void *)p,sizeof(int),(void *)p,
                                          (void *)(& p));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"p",(void *)p);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"\\valid(p)",0,
                                   __gen_e_acsl_valid);
      __gen_e_acsl_and = __gen_e_acsl_valid;
    }
    else __gen_e_acsl_and = 0;
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "\\valid(p)";
    __gen_e_acsl_assert_data.file = "block_valid.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 19;
    __e_acsl_assert(__gen_e_acsl_and,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert \valid(p); */ ;
  {
    int __gen_e_acsl_valid_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_valid_2 = __e_acsl_valid((void *)(p + 1),sizeof(int),
                                          (void *)p,(void *)(& p));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"p",(void *)p);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                   "sizeof(int)",0,sizeof(int));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                 "\\valid(p + 1)",0,__gen_e_acsl_valid_2);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "!\\valid(p + 1)";
    __gen_e_acsl_assert_data_2.file = "block_valid.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 21;
    __e_acsl_assert(! __gen_e_acsl_valid_2,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert !\valid(p + 1); */ ;
  __e_acsl_full_init((void *)(& p));
  p = & B;
  {
    int __gen_e_acsl_initialized_2;
    int __gen_e_acsl_and_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_2 = __e_acsl_initialized((void *)(& p),
                                                      sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"&p",
                                 (void *)(& p));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                   "sizeof(int *)",0,sizeof(int *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                 "\\initialized(&p)",0,
                                 __gen_e_acsl_initialized_2);
    if (__gen_e_acsl_initialized_2) {
      int __gen_e_acsl_valid_3;
      __gen_e_acsl_valid_3 = __e_acsl_valid((void *)p,sizeof(int),(void *)p,
                                            (void *)(& p));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_3,"p",
                                   (void *)p);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_3,
                                     "sizeof(int)",0,sizeof(int));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"\\valid(p)",
                                   0,__gen_e_acsl_valid_3);
      __gen_e_acsl_and_2 = __gen_e_acsl_valid_3;
    }
    else __gen_e_acsl_and_2 = 0;
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Assertion";
    __gen_e_acsl_assert_data_3.pred_txt = "\\valid(p)";
    __gen_e_acsl_assert_data_3.file = "block_valid.c";
    __gen_e_acsl_assert_data_3.fct = "main";
    __gen_e_acsl_assert_data_3.line = 24;
    __e_acsl_assert(__gen_e_acsl_and_2,& __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
  }
  /*@ assert \valid(p); */ ;
  {
    int __gen_e_acsl_valid_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gen_e_acsl_valid_4 = __e_acsl_valid((void *)(p + 1),sizeof(int),
                                          (void *)p,(void *)(& p));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"p",(void *)p);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                   "sizeof(int)",0,sizeof(int));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                 "\\valid(p + 1)",0,__gen_e_acsl_valid_4);
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Assertion";
    __gen_e_acsl_assert_data_4.pred_txt = "!\\valid(p + 1)";
    __gen_e_acsl_assert_data_4.file = "block_valid.c";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 26;
    __e_acsl_assert(! __gen_e_acsl_valid_4,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
  }
  /*@ assert !\valid(p + 1); */ ;
  char *pmin = malloc(sizeof(int));
  __e_acsl_store_block((void *)(& pmin),8UL);
  __e_acsl_full_init((void *)(& pmin));
  char *pmax = malloc(sizeof(int));
  __e_acsl_store_block((void *)(& pmax),8UL);
  __e_acsl_full_init((void *)(& pmax));
  if ((uintptr_t)pmin > (uintptr_t)pmax) {
    char *t = pmin;
    __e_acsl_store_block((void *)(& t),8UL);
    __e_acsl_full_init((void *)(& t));
    __e_acsl_full_init((void *)(& pmin));
    pmin = pmax;
    __e_acsl_full_init((void *)(& pmax));
    pmax = t;
    __e_acsl_delete_block((void *)(& t));
  }
  __e_acsl_initialize((void *)pmin,sizeof(char));
  *pmin = (char)'P';
  __e_acsl_initialize((void *)pmax,sizeof(char));
  *pmax = (char)'L';
  int diff = (int)((uintptr_t)pmax - (uintptr_t)pmin);
  {
    int __gen_e_acsl_initialized_3;
    int __gen_e_acsl_and_3;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_3 = __e_acsl_initialized((void *)(& pmin),
                                                      sizeof(char *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"&pmin",
                                 (void *)(& pmin));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                   "sizeof(char *)",0,sizeof(char *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                 "\\initialized(&pmin)",0,
                                 __gen_e_acsl_initialized_3);
    if (__gen_e_acsl_initialized_3) {
      int __gen_e_acsl_valid_5;
      __gen_e_acsl_valid_5 = __e_acsl_valid((void *)pmin,sizeof(char),
                                            (void *)pmin,(void *)(& pmin));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"pmin",
                                   (void *)pmin);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                     "sizeof(char)",0,sizeof(char));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                   "\\valid(pmin)",0,__gen_e_acsl_valid_5);
      __gen_e_acsl_and_3 = __gen_e_acsl_valid_5;
    }
    else __gen_e_acsl_and_3 = 0;
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Assertion";
    __gen_e_acsl_assert_data_5.pred_txt = "\\valid(pmin)";
    __gen_e_acsl_assert_data_5.file = "block_valid.c";
    __gen_e_acsl_assert_data_5.fct = "main";
    __gen_e_acsl_assert_data_5.line = 44;
    __e_acsl_assert(__gen_e_acsl_and_3,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
  }
  /*@ assert \valid(pmin); */ ;
  {
    int __gen_e_acsl_initialized_4;
    int __gen_e_acsl_and_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
      {.values = (void *)0};
    __gen_e_acsl_initialized_4 = __e_acsl_initialized((void *)(& pmax),
                                                      sizeof(char *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_6,"&pmax",
                                 (void *)(& pmax));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_6,
                                   "sizeof(char *)",0,sizeof(char *));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,
                                 "\\initialized(&pmax)",0,
                                 __gen_e_acsl_initialized_4);
    if (__gen_e_acsl_initialized_4) {
      int __gen_e_acsl_valid_6;
      __gen_e_acsl_valid_6 = __e_acsl_valid((void *)pmax,sizeof(char),
                                            (void *)pmax,(void *)(& pmax));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_6,"pmax",
                                   (void *)pmax);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_6,
                                     "sizeof(char)",0,sizeof(char));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,
                                   "\\valid(pmax)",0,__gen_e_acsl_valid_6);
      __gen_e_acsl_and_4 = __gen_e_acsl_valid_6;
    }
    else __gen_e_acsl_and_4 = 0;
    __gen_e_acsl_assert_data_6.blocking = 1;
    __gen_e_acsl_assert_data_6.kind = "Assertion";
    __gen_e_acsl_assert_data_6.pred_txt = "\\valid(pmax)";
    __gen_e_acsl_assert_data_6.file = "block_valid.c";
    __gen_e_acsl_assert_data_6.fct = "main";
    __gen_e_acsl_assert_data_6.line = 45;
    __e_acsl_assert(__gen_e_acsl_and_4,& __gen_e_acsl_assert_data_6);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
  }
  /*@ assert \valid(pmax); */ ;
  {
    int __gen_e_acsl_valid_7;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
      {.values = (void *)0};
    __gen_e_acsl_valid_7 = __e_acsl_valid((void *)(pmin + diff),sizeof(char),
                                          (void *)pmin,(void *)(& pmin));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_7,"pmin",
                                 (void *)pmin);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,"diff",0,diff);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_7,
                                   "sizeof(char)",0,sizeof(char));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,
                                 "\\valid(pmin + diff)",0,
                                 __gen_e_acsl_valid_7);
    __gen_e_acsl_assert_data_7.blocking = 1;
    __gen_e_acsl_assert_data_7.kind = "Assertion";
    __gen_e_acsl_assert_data_7.pred_txt = "!\\valid(pmin + diff)";
    __gen_e_acsl_assert_data_7.file = "block_valid.c";
    __gen_e_acsl_assert_data_7.fct = "main";
    __gen_e_acsl_assert_data_7.line = 47;
    __e_acsl_assert(! __gen_e_acsl_valid_7,& __gen_e_acsl_assert_data_7);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
  }
  /*@ assert !\valid(pmin + diff); */ ;
  {
    int __gen_e_acsl_valid_8;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_8 =
      {.values = (void *)0};
    __gen_e_acsl_valid_8 = __e_acsl_valid((void *)(pmax - diff),sizeof(char),
                                          (void *)pmax,(void *)(& pmax));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_8,"pmax",
                                 (void *)pmax);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,"diff",0,diff);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_8,
                                   "sizeof(char)",0,sizeof(char));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,
                                 "\\valid(pmax - diff)",0,
                                 __gen_e_acsl_valid_8);
    __gen_e_acsl_assert_data_8.blocking = 1;
    __gen_e_acsl_assert_data_8.kind = "Assertion";
    __gen_e_acsl_assert_data_8.pred_txt = "!\\valid(pmax - diff)";
    __gen_e_acsl_assert_data_8.file = "block_valid.c";
    __gen_e_acsl_assert_data_8.fct = "main";
    __gen_e_acsl_assert_data_8.line = 49;
    __e_acsl_assert(! __gen_e_acsl_valid_8,& __gen_e_acsl_assert_data_8);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_8);
  }
  /*@ assert !\valid(pmax - diff); */ ;
  __retres = 0;
  __e_acsl_delete_block((void *)(& pmax));
  __e_acsl_delete_block((void *)(& pmin));
  __e_acsl_delete_block((void *)(& b));
  __e_acsl_delete_block((void *)(& p));
  __e_acsl_globals_clean();
  __e_acsl_memory_clean();
  return __retres;
}


