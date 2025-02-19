/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

char c;
float f;
void main(void)
{
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  {
    __e_acsl_mpq_t __gen_e_acsl_;
    __e_acsl_mpq_t __gen_e_acsl__2;
    __e_acsl_mpq_t __gen_e_acsl__3;
    __e_acsl_mpq_t __gen_e_acsl_add;
    int __gen_e_acsl_lt;
    __e_acsl_mpq_t __gen_e_acsl__4;
    int __gen_e_acsl_lt_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gmpq_init(__gen_e_acsl_);
    __gmpq_set_str(__gen_e_acsl_,"-129",10);
    __gmpq_init(__gen_e_acsl__2);
    __gmpq_set_d(__gen_e_acsl__2,(double)f);
    __gmpq_init(__gen_e_acsl__3);
    __gmpq_set_d(__gen_e_acsl__3,0.5);
    __gmpq_init(__gen_e_acsl_add);
    __gmpq_add(__gen_e_acsl_add,
               (__e_acsl_mpq_struct const *)(__gen_e_acsl__2),
               (__e_acsl_mpq_struct const *)(__gen_e_acsl__3));
    __gen_e_acsl_lt = __gmpq_cmp((__e_acsl_mpq_struct const *)(__gen_e_acsl_),
                                 (__e_acsl_mpq_struct const *)(__gen_e_acsl_add));
    __e_acsl_assert_register_float(& __gen_e_acsl_assert_data,"f",f);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "-129 < (double)f + 0.5";
    __gen_e_acsl_assert_data.file = "neg_rte_int.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 10;
    __gen_e_acsl_assert_data.name = "rte/float_to_int";
    __e_acsl_assert(__gen_e_acsl_lt < 0,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gmpq_init(__gen_e_acsl__4);
    __gmpq_set_str(__gen_e_acsl__4,"128",10);
    __gen_e_acsl_lt_2 = __gmpq_cmp((__e_acsl_mpq_struct const *)(__gen_e_acsl_add),
                                   (__e_acsl_mpq_struct const *)(__gen_e_acsl__4));
    __e_acsl_assert_register_float(& __gen_e_acsl_assert_data_2,"f",f);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "(double)f + 0.5 < 128";
    __gen_e_acsl_assert_data_2.file = "neg_rte_int.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 10;
    __gen_e_acsl_assert_data_2.name = "rte/float_to_int";
    __e_acsl_assert(__gen_e_acsl_lt_2 < 0,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    __gmpq_clear(__gen_e_acsl_);
    __gmpq_clear(__gen_e_acsl__2);
    __gmpq_clear(__gen_e_acsl__3);
    __gmpq_clear(__gen_e_acsl_add);
    __gmpq_clear(__gen_e_acsl__4);
  }
  /*@ assert
      rte: is_nan_or_infinite:
        \is_finite(\add_double((double)f, (double)0.5));
  */
  /*@ assert rte: float_to_int: (double)f + 0.5 < 128; */
  /*@ assert rte: float_to_int: -129 < (double)f + 0.5; */
  c = (char)((double)f + 0.5);
  __e_acsl_memory_clean();
  return;
}


