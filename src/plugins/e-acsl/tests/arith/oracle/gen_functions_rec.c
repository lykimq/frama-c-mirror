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
predicate even(integer n) =
  n == 0? \true: (n > 0? !even(n - 1): !even(n + 1));

*/
int __gen_e_acsl_even_7(int n);

int __gen_e_acsl_even(int n);

int __gen_e_acsl_even_13(int n);

/*@
predicate even_and_not_negative(integer n) =
  n == 0? \true: (n > 0? !even(n - 1): \false);
 */
int __gen_e_acsl_even_and_not_negative(int n);

/*@ logic integer f1(integer n) = n <= 0? 0: f1(n - 1) + n;

*/
void __gen_e_acsl_f1_9(__e_acsl_mpz_t *__retres_arg, int n);

void __gen_e_acsl_f1_5(__e_acsl_mpz_t *__retres_arg, int n);

int __gen_e_acsl_f1(int n);

/*@
logic integer f2(integer n) = n < 0? 1: (f2(n - 1) * f2(n - 2)) / f2(n - 3);
 */
int __gen_e_acsl_f2(int n);

/*@ logic integer g(integer n) = 0;
 */
int __gen_e_acsl_g_3(int n);

int __gen_e_acsl_g(int n);

/*@ logic integer f3(integer n) = n > 0? g(n) * f3(n - 1) - 5: g(n + 1);
 */
int __gen_e_acsl_f3(int n);

/*@
logic integer f4(integer n) =
  n < 100? f4(n + 1): (n < 0x7fffffffffffffffL? 0x7fffffffffffffffL: 6);
 */
unsigned long __gen_e_acsl_f4(unsigned int n);

/*@ logic integer f5(integer n) = n >= 0? 0: f5(n + 1) + n;

*/
int __gen_e_acsl_f5(unsigned int n);

void __gen_e_acsl_f5_5(__e_acsl_mpz_t *__retres_arg, long n);

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  {
    int __gen_e_acsl_even_6;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_even_6 = __gen_e_acsl_even(10);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"even(10)",0,
                                 __gen_e_acsl_even_6);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "even(10)";
    __gen_e_acsl_assert_data.file = "functions_rec.c";
    __gen_e_acsl_assert_data.fct = "main";
    __gen_e_acsl_assert_data.line = 40;
    __e_acsl_assert(__gen_e_acsl_even_6,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert even(10); */ ;
  {
    int __gen_e_acsl_even_12;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_even_12 = __gen_e_acsl_even_7(-6);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"even(-6)",0,
                                 __gen_e_acsl_even_12);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Assertion";
    __gen_e_acsl_assert_data_2.pred_txt = "even(-6)";
    __gen_e_acsl_assert_data_2.file = "functions_rec.c";
    __gen_e_acsl_assert_data_2.fct = "main";
    __gen_e_acsl_assert_data_2.line = 41;
    __e_acsl_assert(__gen_e_acsl_even_12,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  /*@ assert even(-6); */ ;
  {
    int __gen_e_acsl_even_and_not_negative_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
      {.values = (void *)0};
    __gen_e_acsl_even_and_not_negative_2 = __gen_e_acsl_even_and_not_negative
    (10);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                 "even_and_not_negative(10)",0,
                                 __gen_e_acsl_even_and_not_negative_2);
    __gen_e_acsl_assert_data_3.blocking = 1;
    __gen_e_acsl_assert_data_3.kind = "Assertion";
    __gen_e_acsl_assert_data_3.pred_txt = "even_and_not_negative(10)";
    __gen_e_acsl_assert_data_3.file = "functions_rec.c";
    __gen_e_acsl_assert_data_3.fct = "main";
    __gen_e_acsl_assert_data_3.line = 42;
    __e_acsl_assert(__gen_e_acsl_even_and_not_negative_2,
                    & __gen_e_acsl_assert_data_3);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
  }
  /*@ assert even_and_not_negative(10); */ ;
  {
    int __gen_e_acsl_f1_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __gen_e_acsl_f1_4 = __gen_e_acsl_f1(0);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"f1(0)",0,
                                 __gen_e_acsl_f1_4);
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Assertion";
    __gen_e_acsl_assert_data_4.pred_txt = "f1(0) == 0";
    __gen_e_acsl_assert_data_4.file = "functions_rec.c";
    __gen_e_acsl_assert_data_4.fct = "main";
    __gen_e_acsl_assert_data_4.line = 43;
    __e_acsl_assert(__gen_e_acsl_f1_4 == 0,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
  }
  /*@ assert f1(0) == 0; */ ;
  {
    __e_acsl_mpz_t __gen_e_acsl_f1_8;
    __e_acsl_mpz_t __gen_e_acsl__2;
    int __gen_e_acsl_eq;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __gen_e_acsl_f1_5(& __gen_e_acsl_f1_8,1);
    __gmpz_init_set_si(__gen_e_acsl__2,1L);
    __gen_e_acsl_eq = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_8),
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl__2));
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data_5,"f1(1)",0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_8));
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Assertion";
    __gen_e_acsl_assert_data_5.pred_txt = "f1(1) == 1";
    __gen_e_acsl_assert_data_5.file = "functions_rec.c";
    __gen_e_acsl_assert_data_5.fct = "main";
    __gen_e_acsl_assert_data_5.line = 44;
    __e_acsl_assert(__gen_e_acsl_eq == 0,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    __gmpz_clear(__gen_e_acsl_f1_8);
    __gmpz_clear(__gen_e_acsl__2);
  }
  /*@ assert f1(1) == 1; */ ;
  {
    __e_acsl_mpz_t __gen_e_acsl_f1_12;
    __e_acsl_mpz_t __gen_e_acsl__4;
    int __gen_e_acsl_eq_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
      {.values = (void *)0};
    __gen_e_acsl_f1_9(& __gen_e_acsl_f1_12,10);
    __gmpz_init_set_si(__gen_e_acsl__4,55L);
    __gen_e_acsl_eq_2 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_12),
                                   (__e_acsl_mpz_struct const *)(__gen_e_acsl__4));
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data_6,"f1(10)",0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_12));
    __gen_e_acsl_assert_data_6.blocking = 1;
    __gen_e_acsl_assert_data_6.kind = "Assertion";
    __gen_e_acsl_assert_data_6.pred_txt = "f1(10) == 55";
    __gen_e_acsl_assert_data_6.file = "functions_rec.c";
    __gen_e_acsl_assert_data_6.fct = "main";
    __gen_e_acsl_assert_data_6.line = 45;
    __e_acsl_assert(__gen_e_acsl_eq_2 == 0,& __gen_e_acsl_assert_data_6);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
    __gmpz_clear(__gen_e_acsl_f1_12);
    __gmpz_clear(__gen_e_acsl__4);
  }
  /*@ assert f1(10) == 55; */ ;
  {
    int __gen_e_acsl_f2_8;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
      {.values = (void *)0};
    __gen_e_acsl_f2_8 = __gen_e_acsl_f2(7);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_7,"f2(7)",0,
                                 __gen_e_acsl_f2_8);
    __gen_e_acsl_assert_data_7.blocking = 1;
    __gen_e_acsl_assert_data_7.kind = "Assertion";
    __gen_e_acsl_assert_data_7.pred_txt = "f2(7) == 1";
    __gen_e_acsl_assert_data_7.file = "functions_rec.c";
    __gen_e_acsl_assert_data_7.fct = "main";
    __gen_e_acsl_assert_data_7.line = 47;
    __e_acsl_assert(__gen_e_acsl_f2_8 == 1,& __gen_e_acsl_assert_data_7);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
  }
  /*@ assert f2(7) == 1; */ ;
  {
    int __gen_e_acsl_f3_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_9 =
      {.values = (void *)0};
    __gen_e_acsl_f3_4 = __gen_e_acsl_f3(6);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_9,"f3(6)",0,
                                 __gen_e_acsl_f3_4);
    __gen_e_acsl_assert_data_9.blocking = 1;
    __gen_e_acsl_assert_data_9.kind = "Assertion";
    __gen_e_acsl_assert_data_9.pred_txt = "f3(6) == -5";
    __gen_e_acsl_assert_data_9.file = "functions_rec.c";
    __gen_e_acsl_assert_data_9.fct = "main";
    __gen_e_acsl_assert_data_9.line = 49;
    __e_acsl_assert(__gen_e_acsl_f3_4 == -5,& __gen_e_acsl_assert_data_9);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_9);
  }
  /*@ assert f3(6) == -5; */ ;
  {
    unsigned long __gen_e_acsl_f4_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_10 =
      {.values = (void *)0};
    __gen_e_acsl_f4_4 = __gen_e_acsl_f4(9U);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_10,"f4(9)",0,
                                   __gen_e_acsl_f4_4);
    __gen_e_acsl_assert_data_10.blocking = 1;
    __gen_e_acsl_assert_data_10.kind = "Assertion";
    __gen_e_acsl_assert_data_10.pred_txt = "f4(9) > 0";
    __gen_e_acsl_assert_data_10.file = "functions_rec.c";
    __gen_e_acsl_assert_data_10.fct = "main";
    __gen_e_acsl_assert_data_10.line = 51;
    __e_acsl_assert(__gen_e_acsl_f4_4 > 0UL,& __gen_e_acsl_assert_data_10);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_10);
  }
  /*@ assert f4(9) > 0; */ ;
  {
    int __gen_e_acsl_f5_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_11 =
      {.values = (void *)0};
    __gen_e_acsl_f5_4 = __gen_e_acsl_f5(0U);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_11,"f5(0)",0,
                                 __gen_e_acsl_f5_4);
    __gen_e_acsl_assert_data_11.blocking = 1;
    __gen_e_acsl_assert_data_11.kind = "Assertion";
    __gen_e_acsl_assert_data_11.pred_txt = "f5(0) == 0";
    __gen_e_acsl_assert_data_11.file = "functions_rec.c";
    __gen_e_acsl_assert_data_11.fct = "main";
    __gen_e_acsl_assert_data_11.line = 53;
    __e_acsl_assert(__gen_e_acsl_f5_4 == 0,& __gen_e_acsl_assert_data_11);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_11);
  }
  /*@ assert f5(0) == 0; */ ;
  {
    long __gen_e_acsl_n_3;
    __e_acsl_mpz_t __gen_e_acsl_f5_8;
    __e_acsl_mpz_t __gen_e_acsl__6;
    int __gen_e_acsl_eq_3;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_12 =
      {.values = (void *)0};
    __gen_e_acsl_n_3 = 9223372036854775807L;
    __gen_e_acsl_f5_5(& __gen_e_acsl_f5_8,__gen_e_acsl_n_3);
    __gmpz_init_set_si(__gen_e_acsl__6,0L);
    __gen_e_acsl_eq_3 = __gmpz_cmp((__e_acsl_mpz_struct const *)(__gen_e_acsl_f5_8),
                                   (__e_acsl_mpz_struct const *)(__gen_e_acsl__6));
    __e_acsl_assert_register_long(& __gen_e_acsl_assert_data_12,"n",0,
                                  __gen_e_acsl_n_3);
    __e_acsl_assert_register_mpz(& __gen_e_acsl_assert_data_12,"f5(n)",0,
                                 (__e_acsl_mpz_struct const *)(__gen_e_acsl_f5_8));
    __gen_e_acsl_assert_data_12.blocking = 1;
    __gen_e_acsl_assert_data_12.kind = "Assertion";
    __gen_e_acsl_assert_data_12.pred_txt = "\\let n = 0 == 0? 0x7fffffffffffffffL: -1; f5(n) == 0";
    __gen_e_acsl_assert_data_12.file = "functions_rec.c";
    __gen_e_acsl_assert_data_12.fct = "main";
    __gen_e_acsl_assert_data_12.line = 55;
    __e_acsl_assert(__gen_e_acsl_eq_3 == 0,& __gen_e_acsl_assert_data_12);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_12);
    __gmpz_clear(__gen_e_acsl_f5_8);
    __gmpz_clear(__gen_e_acsl__6);
  }
  /*@ assert \let n = 0 == 0? 0x7fffffffffffffffL: -1; f5(n) == 0; */ ;
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_even_13(int n)
{
  int __gen_e_acsl_if_6;
  if (n == 0) __gen_e_acsl_if_6 = 1;
  else {
    int __gen_e_acsl_if_5;
    if (n > 0) {
      int __gen_e_acsl_even_15;
      __gen_e_acsl_even_15 = __gen_e_acsl_even_13(n - 1);
      __gen_e_acsl_if_5 = ! __gen_e_acsl_even_15;
    }
    else {
      int __gen_e_acsl_even_17;
      __gen_e_acsl_even_17 = __gen_e_acsl_even_13(n + 1);
      __gen_e_acsl_if_5 = ! __gen_e_acsl_even_17;
    }
    __gen_e_acsl_if_6 = __gen_e_acsl_if_5;
  }
  return __gen_e_acsl_if_6;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_even(int n)
{
  int __gen_e_acsl_if_2;
  if (n == 0) __gen_e_acsl_if_2 = 1;
  else {
    int __gen_e_acsl_if;
    if (n > 0) {
      int __gen_e_acsl_even_3;
      __gen_e_acsl_even_3 = __gen_e_acsl_even(n - 1);
      __gen_e_acsl_if = ! __gen_e_acsl_even_3;
    }
    else {
      int __gen_e_acsl_even_5;
      __gen_e_acsl_even_5 = __gen_e_acsl_even(n + 1);
      __gen_e_acsl_if = ! __gen_e_acsl_even_5;
    }
    __gen_e_acsl_if_2 = __gen_e_acsl_if;
  }
  return __gen_e_acsl_if_2;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_even_7(int n)
{
  int __gen_e_acsl_if_4;
  if (n == 0) __gen_e_acsl_if_4 = 1;
  else {
    int __gen_e_acsl_if_3;
    if (n > 0) {
      int __gen_e_acsl_even_9;
      __gen_e_acsl_even_9 = __gen_e_acsl_even_7(n - 1);
      __gen_e_acsl_if_3 = ! __gen_e_acsl_even_9;
    }
    else {
      int __gen_e_acsl_even_11;
      __gen_e_acsl_even_11 = __gen_e_acsl_even_7(n + 1);
      __gen_e_acsl_if_3 = ! __gen_e_acsl_even_11;
    }
    __gen_e_acsl_if_4 = __gen_e_acsl_if_3;
  }
  return __gen_e_acsl_if_4;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_even_and_not_negative(int n)
{
  int __gen_e_acsl_if_8;
  if (n == 0) __gen_e_acsl_if_8 = 1;
  else {
    int __gen_e_acsl_if_7;
    if (n > 0) {
      int __gen_e_acsl_even_18;
      __gen_e_acsl_even_18 = __gen_e_acsl_even_13(n - 1);
      __gen_e_acsl_if_7 = ! __gen_e_acsl_even_18;
    }
    else __gen_e_acsl_if_7 = 0;
    __gen_e_acsl_if_8 = __gen_e_acsl_if_7;
  }
  return __gen_e_acsl_if_8;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_f1(int n)
{
  int __gen_e_acsl_if_9;
  if (n <= 0) __gen_e_acsl_if_9 = 0;
  else {
    int __gen_e_acsl_f1_3;
    __gen_e_acsl_f1_3 = __gen_e_acsl_f1(n - 1);
    __gen_e_acsl_if_9 = __gen_e_acsl_f1_3 + n;
  }
  return __gen_e_acsl_if_9;
}

/*@ assigns (*__retres_arg)[0];
    assigns (*__retres_arg)[0] \from n; */
void __gen_e_acsl_f1_5(__e_acsl_mpz_t *__retres_arg, int n)
{
  __e_acsl_mpz_t __gen_e_acsl_if_10;
  if (n <= 0) {
    __e_acsl_mpz_t __gen_e_acsl_;
    __gmpz_init_set_si(__gen_e_acsl_,0L);
    __gmpz_init_set(__gen_e_acsl_if_10,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl_));
    __gmpz_clear(__gen_e_acsl_);
  }
  else {
    __e_acsl_mpz_t __gen_e_acsl_f1_7;
    __e_acsl_mpz_t __gen_e_acsl_n;
    __e_acsl_mpz_t __gen_e_acsl_add;
    __gen_e_acsl_f1_5(& __gen_e_acsl_f1_7,n - 1);
    __gmpz_init_set_si(__gen_e_acsl_n,(long)n);
    __gmpz_init(__gen_e_acsl_add);
    __gmpz_add(__gen_e_acsl_add,
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_7),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_n));
    __gmpz_init_set(__gen_e_acsl_if_10,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl_add));
    __gmpz_clear(__gen_e_acsl_f1_7);
    __gmpz_clear(__gen_e_acsl_n);
    __gmpz_clear(__gen_e_acsl_add);
  }
  __gmpz_init_set(*__retres_arg,
                  (__e_acsl_mpz_struct const *)(__gen_e_acsl_if_10));
  __gmpz_clear(__gen_e_acsl_if_10);
  return;
}

/*@ assigns (*__retres_arg)[0];
    assigns (*__retres_arg)[0] \from n; */
void __gen_e_acsl_f1_9(__e_acsl_mpz_t *__retres_arg, int n)
{
  __e_acsl_mpz_t __gen_e_acsl_if_11;
  if (n <= 0) {
    __e_acsl_mpz_t __gen_e_acsl__3;
    __gmpz_init_set_si(__gen_e_acsl__3,0L);
    __gmpz_init_set(__gen_e_acsl_if_11,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl__3));
    __gmpz_clear(__gen_e_acsl__3);
  }
  else {
    __e_acsl_mpz_t __gen_e_acsl_f1_11;
    __e_acsl_mpz_t __gen_e_acsl_n_2;
    __e_acsl_mpz_t __gen_e_acsl_add_2;
    __gen_e_acsl_f1_9(& __gen_e_acsl_f1_11,n - 1);
    __gmpz_init_set_si(__gen_e_acsl_n_2,(long)n);
    __gmpz_init(__gen_e_acsl_add_2);
    __gmpz_add(__gen_e_acsl_add_2,
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_f1_11),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_n_2));
    __gmpz_init_set(__gen_e_acsl_if_11,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl_add_2));
    __gmpz_clear(__gen_e_acsl_f1_11);
    __gmpz_clear(__gen_e_acsl_n_2);
    __gmpz_clear(__gen_e_acsl_add_2);
  }
  __gmpz_init_set(*__retres_arg,
                  (__e_acsl_mpz_struct const *)(__gen_e_acsl_if_11));
  __gmpz_clear(__gen_e_acsl_if_11);
  return;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_f2(int n)
{
  int __gen_e_acsl_if_12;
  if (n < 0) __gen_e_acsl_if_12 = 1;
  else {
    int __gen_e_acsl_f2_3;
    int __gen_e_acsl_f2_5;
    int __gen_e_acsl_f2_7;
    __gen_e_acsl_f2_3 = __gen_e_acsl_f2(n - 1);
    __gen_e_acsl_f2_5 = __gen_e_acsl_f2(n - 2);
    __gen_e_acsl_f2_7 = __gen_e_acsl_f2(n - 3);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_8 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_8,
                                 "__gen_e_acsl_f2_7",0,__gen_e_acsl_f2_7);
    __gen_e_acsl_assert_data_8.blocking = 1;
    __gen_e_acsl_assert_data_8.kind = "RTE";
    __gen_e_acsl_assert_data_8.pred_txt = "__gen_e_acsl_f2_7 != 0";
    __gen_e_acsl_assert_data_8.file = "functions_rec.c";
    __gen_e_acsl_assert_data_8.fct = "f2";
    __gen_e_acsl_assert_data_8.line = 25;
    __gen_e_acsl_assert_data_8.name = "division_by_zero";
    __e_acsl_assert(__gen_e_acsl_f2_7 != 0,& __gen_e_acsl_assert_data_8);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_8);
    /*@ assert Eva: division_by_zero: __gen_e_acsl_f2_7 != 0; */
    /*@ assert
        Eva: signed_overflow:
          -2147483648 <= __gen_e_acsl_f2_3 * __gen_e_acsl_f2_5;
    */
    /*@ assert
        Eva: signed_overflow:
          __gen_e_acsl_f2_3 * __gen_e_acsl_f2_5 <= 2147483647;
    */
    /*@ assert
        Eva: signed_overflow:
          (int)(__gen_e_acsl_f2_3 * __gen_e_acsl_f2_5) / __gen_e_acsl_f2_7 <=
          2147483647;
    */
    __gen_e_acsl_if_12 = (__gen_e_acsl_f2_3 * __gen_e_acsl_f2_5) / __gen_e_acsl_f2_7;
  }
  return __gen_e_acsl_if_12;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_g(int n)
{
  int __retres = 0;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_g_3(int n)
{
  int __retres = 0;
  return __retres;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_f3(int n)
{
  int __gen_e_acsl_if_13;
  if (n > 0) {
    int __gen_e_acsl_g_2;
    int __gen_e_acsl_f3_3;
    __gen_e_acsl_g_2 = __gen_e_acsl_g(n);
    __gen_e_acsl_f3_3 = __gen_e_acsl_f3(n - 1);
    __gen_e_acsl_if_13 = __gen_e_acsl_g_2 * __gen_e_acsl_f3_3 - 5;
  }
  else {
    int __gen_e_acsl_g_4;
    __gen_e_acsl_g_4 = __gen_e_acsl_g_3(n + 1);
    __gen_e_acsl_if_13 = __gen_e_acsl_g_4;
  }
  return __gen_e_acsl_if_13;
}

/*@ assigns \result;
    assigns \result \from n; */
unsigned long __gen_e_acsl_f4(unsigned int n)
{
  unsigned long __gen_e_acsl_if_15;
  if (n < 100U) {
    unsigned long __gen_e_acsl_f4_3;
    __gen_e_acsl_f4_3 = __gen_e_acsl_f4(n + 1U);
    __gen_e_acsl_if_15 = __gen_e_acsl_f4_3;
  }
  else {
    unsigned long __gen_e_acsl_if_14;
    if ((unsigned long)n < 9223372036854775807UL) __gen_e_acsl_if_14 = 9223372036854775807UL;
    else __gen_e_acsl_if_14 = 6UL;
    __gen_e_acsl_if_15 = __gen_e_acsl_if_14;
  }
  return __gen_e_acsl_if_15;
}

/*@ assigns (*__retres_arg)[0];
    assigns (*__retres_arg)[0] \from n; */
void __gen_e_acsl_f5_5(__e_acsl_mpz_t *__retres_arg, long n)
{
  __e_acsl_mpz_t __gen_e_acsl_if_17;
  if (n >= 0L) {
    __e_acsl_mpz_t __gen_e_acsl__5;
    __gmpz_init_set_si(__gen_e_acsl__5,0L);
    __gmpz_init_set(__gen_e_acsl_if_17,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl__5));
    __gmpz_clear(__gen_e_acsl__5);
  }
  else {
    __e_acsl_mpz_t __gen_e_acsl_f5_7;
    __e_acsl_mpz_t __gen_e_acsl_n_4;
    __e_acsl_mpz_t __gen_e_acsl_add_3;
    __gen_e_acsl_f5_5(& __gen_e_acsl_f5_7,n + 1L);
    __gmpz_init_set_si(__gen_e_acsl_n_4,n);
    __gmpz_init(__gen_e_acsl_add_3);
    __gmpz_add(__gen_e_acsl_add_3,
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_f5_7),
               (__e_acsl_mpz_struct const *)(__gen_e_acsl_n_4));
    __gmpz_init_set(__gen_e_acsl_if_17,
                    (__e_acsl_mpz_struct const *)(__gen_e_acsl_add_3));
    __gmpz_clear(__gen_e_acsl_f5_7);
    __gmpz_clear(__gen_e_acsl_n_4);
    __gmpz_clear(__gen_e_acsl_add_3);
  }
  __gmpz_init_set(*__retres_arg,
                  (__e_acsl_mpz_struct const *)(__gen_e_acsl_if_17));
  __gmpz_clear(__gen_e_acsl_if_17);
  return;
}

/*@ assigns \result;
    assigns \result \from n; */
int __gen_e_acsl_f5(unsigned int n)
{
  int __gen_e_acsl_if_16;
  if (n >= 0U) __gen_e_acsl_if_16 = 0;
  else {
    int __gen_e_acsl_f5_3;
    __gen_e_acsl_f5_3 = __gen_e_acsl_f5(n + 1U);
    __gen_e_acsl_if_16 = __gen_e_acsl_f5_3 + n;
  }
  return __gen_e_acsl_if_16;
}


