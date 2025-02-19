/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

/*@
logic integer f(integer x) = x == 0? 0: (\let v = f(0); 0 == v? x - 1: 0);

*/
long __gen_e_acsl_f_2(int x);

int __gen_e_acsl_f(int x);

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  {
    int __gen_e_acsl_f_6;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_f_6 = __gen_e_acsl_f(1);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"f(1)",0,
                                 __gen_e_acsl_f_6);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "f(1) == 0";
    __gen_e_acsl_assert_data.file = "let.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 17;
    __e_acsl_assert(__gen_e_acsl_f_6 == 0,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert f(1) == 0; */ ;
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}

/*@ assigns \result;
    assigns \result \from x; */
int __gen_e_acsl_f(int x)
{
  long __gen_e_acsl_if_4;
  if (x == 0) __gen_e_acsl_if_4 = 0L;
  else {
    long __gen_e_acsl_v;
    long __gen_e_acsl_f_5;
    long __gen_e_acsl_if_3;
    __gen_e_acsl_f_5 = __gen_e_acsl_f_2(0);
    __gen_e_acsl_v = __gen_e_acsl_f_5;
    if (0L == __gen_e_acsl_v) __gen_e_acsl_if_3 = x - 1L;
    else __gen_e_acsl_if_3 = 0L;
    __gen_e_acsl_if_4 = __gen_e_acsl_if_3;
  }
  int __retres = (int)__gen_e_acsl_if_4;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from x; */
long __gen_e_acsl_f_2(int x)
{
  long __gen_e_acsl_if_2;
  if (x == 0) __gen_e_acsl_if_2 = 0L;
  else {
    long __gen_e_acsl_v_2;
    long __gen_e_acsl_f_4;
    long __gen_e_acsl_if;
    __gen_e_acsl_f_4 = __gen_e_acsl_f_2(0);
    __gen_e_acsl_v_2 = __gen_e_acsl_f_4;
    if (0L == __gen_e_acsl_v_2) __gen_e_acsl_if = x - 1L;
    else __gen_e_acsl_if = 0L;
    __gen_e_acsl_if_2 = __gen_e_acsl_if;
  }
  long __retres = (int)__gen_e_acsl_if_2;
  return __retres;
}


