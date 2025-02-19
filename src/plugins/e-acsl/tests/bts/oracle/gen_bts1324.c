/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

/*@ behavior yes:
      assumes \forall int i; 0 < i < n ==> *(t + (i - 1)) <= *(t + i);
      ensures \result == 1;
 */
int __gen_e_acsl_sorted(int *t, int n);

int sorted(int *t, int n)
{
  int __retres;
  int b = 1;
  if (n <= 1) {
    __retres = 1;
    goto return_label;
  }
  b = 1;
  while (b < n) {
    if (*(t + (b - 1)) > *(t + b)) {
      __retres = 0;
      goto return_label;
    }
    b ++;
  }
  __retres = 1;
  return_label: return __retres;
}

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  int t[7] = {1, 4, 4, 5, 5, 5, 7};
  __e_acsl_store_block((void *)(t),28UL);
  __e_acsl_full_init((void *)(& t));
  int n = __gen_e_acsl_sorted(t,7);
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"n",0,n);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "n == 1";
    __gen_e_acsl_assert_data.file = "bts1324.i";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 23;
    __e_acsl_assert(n == 1,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert n == 1; */ ;
  __retres = 0;
  __e_acsl_delete_block((void *)(t));
  __e_acsl_memory_clean();
  return __retres;
}

/*@ behavior yes:
      assumes \forall int i; 0 < i < n ==> *(t + (i - 1)) <= *(t + i);
      ensures \result == 1;
 */
int __gen_e_acsl_sorted(int *t, int n)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  int __retres;
  {
    int __gen_e_acsl_forall;
    int __gen_e_acsl_i;
    __e_acsl_store_block((void *)(& t),8UL);
    __gen_e_acsl_contract = __e_acsl_contract_init(1UL);
    __gen_e_acsl_forall = 1;
    __gen_e_acsl_i = 0 + 1;
    while (1) {
      if (__gen_e_acsl_i < n) ; else break;
      {
        int __gen_e_acsl_valid_read;
        int __gen_e_acsl_valid_read_2;
        __e_acsl_assert_data_t __gen_e_acsl_assert_data =
          {.values = (void *)0};
        __gen_e_acsl_valid_read = __e_acsl_valid_read((void *)(t + __gen_e_acsl_i),
                                                      sizeof(int),(void *)t,
                                                      (void *)(& t));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"t",
                                     (void *)t);
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                     "__gen_e_acsl_i",0,__gen_e_acsl_i);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                       "sizeof(int)",0,sizeof(int));
        __gen_e_acsl_assert_data.blocking = 1;
        __gen_e_acsl_assert_data.kind = "RTE";
        __gen_e_acsl_assert_data.pred_txt = "\\valid_read(t + __gen_e_acsl_i)";
        __gen_e_acsl_assert_data.file = "bts1324.i";
        __gen_e_acsl_assert_data.fct = "sorted";
        __gen_e_acsl_assert_data.line = 6;
        __gen_e_acsl_assert_data.name = "mem_access";
        __e_acsl_assert(__gen_e_acsl_valid_read,& __gen_e_acsl_assert_data);
        __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
        __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
          {.values = (void *)0};
        __gen_e_acsl_valid_read_2 = __e_acsl_valid_read((void *)(t + (
                                                                 __gen_e_acsl_i - 1L)),
                                                        sizeof(int),
                                                        (void *)t,
                                                        (void *)(& t));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"t",
                                     (void *)t);
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                     "__gen_e_acsl_i",0,__gen_e_acsl_i);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                       "sizeof(int)",0,sizeof(int));
        __gen_e_acsl_assert_data_2.blocking = 1;
        __gen_e_acsl_assert_data_2.kind = "RTE";
        __gen_e_acsl_assert_data_2.pred_txt = "\\valid_read(t + (long)(__gen_e_acsl_i - 1))";
        __gen_e_acsl_assert_data_2.file = "bts1324.i";
        __gen_e_acsl_assert_data_2.fct = "sorted";
        __gen_e_acsl_assert_data_2.line = 6;
        __gen_e_acsl_assert_data_2.name = "mem_access";
        __e_acsl_assert(__gen_e_acsl_valid_read_2,
                        & __gen_e_acsl_assert_data_2);
        __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
        if (*(t + (__gen_e_acsl_i - 1L)) <= *(t + __gen_e_acsl_i)) ;
        else {
          __gen_e_acsl_forall = 0;
          goto e_acsl_end_loop1;
        }
      }
      __gen_e_acsl_i ++;
    }
    e_acsl_end_loop1: ;
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,
                                           __gen_e_acsl_forall);
  }
  __retres = sorted(t,n);
  {
    int __gen_e_acsl_assumes_value;
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"\\result",0,
                                   __retres);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Postcondition";
      __gen_e_acsl_assert_data_3.pred_txt = "\\result == 1";
      __gen_e_acsl_assert_data_3.file = "bts1324.i";
      __gen_e_acsl_assert_data_3.fct = "sorted";
      __gen_e_acsl_assert_data_3.line = 7;
      __gen_e_acsl_assert_data_3.name = "yes";
      __e_acsl_assert(__retres == 1,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    __e_acsl_delete_block((void *)(& t));
    return __retres;
  }
}


