/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

int X = 0;
int Y = 2;
/*@ ensures X == 1; */
void __gen_e_acsl_f(void);

void f(void)
{
  X = 1;
  return;
}

/*@ ensures X == 2;
    ensures Y == 2; */
void __gen_e_acsl_g(void);

void g(void)
{
  X = 2;
  return;
}

/*@ requires X == 2; */
void __gen_e_acsl_h(void);

void h(void)
{
  X ++;
  return;
}

/*@ requires X == 3;
    requires Y == 2; */
void __gen_e_acsl_i(void);

void i(void)
{
  X += Y;
  return;
}

/*@ behavior b1:
      requires X == 5;
      ensures X == 3;
    
    behavior b2:
      requires X == 3 + Y;
      requires Y == 2;
      ensures X == Y + 1;
 */
void __gen_e_acsl_j(void);

void j(void)
{
  X = 3;
  return;
}

/*@ behavior b1:
      assumes X == 1;
      requires X == 0;
    
    behavior b2:
      assumes X == 3;
      assumes Y == 2;
      requires X == 3;
      requires X + Y == 5;
 */
void __gen_e_acsl_k(void);

void k(void)
{
  X += Y;
  return;
}

/*@ ensures X == 5; */
int __gen_e_acsl_l(void);

int l(void)
{
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"Y",0,Y);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "Y == 2";
    __gen_e_acsl_assert_data.file = "function_contract.i";
    __gen_e_acsl_assert_data.fct = "l";
    __gen_e_acsl_assert_data.line = 61;
    __e_acsl_assert(Y == 2,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert Y == 2; */ ;
  return X;
}

/*@ behavior b1:
      assumes X == 7;
      ensures X == 95;
    
    behavior b2:
      assumes X == 5;
      assumes Y == 2;
      ensures X == 7;
      ensures X == \old(X) + Y;
 */
void __gen_e_acsl_m(void);

void m(void)
{
  X += Y;
  return;
}

/*@ requires X > 0;
    requires X < 10;
    
    behavior b1:
      assumes X == 7;
      ensures X == 8;
    
    behavior b2:
      assumes X == 5;
      ensures X == 98;
 */
void __gen_e_acsl_n(void);

void n(void)
{
  X ++;
  return;
}

/*@ requires X > -1000;
    ensures X == \old(Y);
    
    behavior neg:
      assumes Y < 0;
      requires Y < 1;
      ensures X == \old(Y);
    
    behavior pos:
      assumes Y >= 0;
      requires Y > -1;
      ensures X == \old(Y);
    
    behavior odd:
      assumes Y % 2 == 1;
      requires Y % 2 - 1 == 0;
      ensures X == \old(Y);
    
    behavior even:
      assumes Y % 2 == 0;
      requires Y % 2 + 1 == 1;
      ensures X == \old(Y);
    
    complete behaviors even, odd, pos, neg;
    complete behaviors odd, even;
    complete behaviors neg, pos;
    disjoint behaviors odd, even;
    disjoint behaviors neg, pos;
 */
void __gen_e_acsl_o(void);

void o(void)
{
  X = Y;
  return;
}

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __gen_e_acsl_f();
  __gen_e_acsl_g();
  __gen_e_acsl_h();
  __gen_e_acsl_i();
  __gen_e_acsl_j();
  __gen_e_acsl_k();
  __gen_e_acsl_l();
  __gen_e_acsl_m();
  __gen_e_acsl_n();
  __gen_e_acsl_o();
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}

/*@ requires X > -1000;
    ensures X == \old(Y);
    
    behavior neg:
      assumes Y < 0;
      requires Y < 1;
      ensures X == \old(Y);
    
    behavior pos:
      assumes Y >= 0;
      requires Y > -1;
      ensures X == \old(Y);
    
    behavior odd:
      assumes Y % 2 == 1;
      requires Y % 2 - 1 == 0;
      ensures X == \old(Y);
    
    behavior even:
      assumes Y % 2 == 0;
      requires Y % 2 + 1 == 1;
      ensures X == \old(Y);
    
    complete behaviors even, odd, pos, neg;
    complete behaviors odd, even;
    complete behaviors neg, pos;
    disjoint behaviors odd, even;
    disjoint behaviors neg, pos;
 */
void __gen_e_acsl_o(void)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  int __gen_e_acsl_at_5;
  int __gen_e_acsl_at_4;
  int __gen_e_acsl_at_3;
  int __gen_e_acsl_at_2;
  int __gen_e_acsl_at;
  {
    int __gen_e_acsl_assumes_value;
    int __gen_e_acsl_active_bhvrs;
    __gen_e_acsl_at = Y;
    __gen_e_acsl_at_2 = Y;
    __gen_e_acsl_at_3 = Y;
    __gen_e_acsl_at_4 = Y;
    __gen_e_acsl_at_5 = Y;
    __gen_e_acsl_contract = __e_acsl_contract_init(4UL);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Precondition";
    __gen_e_acsl_assert_data.pred_txt = "X > -1000";
    __gen_e_acsl_assert_data.file = "function_contract.i";
    __gen_e_acsl_assert_data.fct = "o";
    __gen_e_acsl_assert_data.line = 93;
    __e_acsl_assert(X > -1000,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,Y < 0);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,1UL,Y >= 0);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,2UL,
                                           Y % 2 == 1);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,3UL,
                                           Y % 2 == 0);
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"Y",0,Y);
      __gen_e_acsl_assert_data_2.blocking = 1;
      __gen_e_acsl_assert_data_2.kind = "Precondition";
      __gen_e_acsl_assert_data_2.pred_txt = "Y < 1";
      __gen_e_acsl_assert_data_2.file = "function_contract.i";
      __gen_e_acsl_assert_data_2.fct = "o";
      __gen_e_acsl_assert_data_2.line = 98;
      __gen_e_acsl_assert_data_2.name = "neg";
      __e_acsl_assert(Y < 1,& __gen_e_acsl_assert_data_2);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"Y",0,Y);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Precondition";
      __gen_e_acsl_assert_data_3.pred_txt = "Y > -1";
      __gen_e_acsl_assert_data_3.file = "function_contract.i";
      __gen_e_acsl_assert_data_3.fct = "o";
      __gen_e_acsl_assert_data_3.line = 103;
      __gen_e_acsl_assert_data_3.name = "pos";
      __e_acsl_assert(Y > -1,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,2UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"Y",0,Y);
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Precondition";
      __gen_e_acsl_assert_data_4.pred_txt = "Y % 2 - 1 == 0";
      __gen_e_acsl_assert_data_4.file = "function_contract.i";
      __gen_e_acsl_assert_data_4.fct = "o";
      __gen_e_acsl_assert_data_4.line = 108;
      __gen_e_acsl_assert_data_4.name = "odd";
      __e_acsl_assert(Y % 2 - 1 == 0,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,3UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"Y",0,Y);
      __gen_e_acsl_assert_data_5.blocking = 1;
      __gen_e_acsl_assert_data_5.kind = "Precondition";
      __gen_e_acsl_assert_data_5.pred_txt = "Y % 2 + 1 == 1";
      __gen_e_acsl_assert_data_5.file = "function_contract.i";
      __gen_e_acsl_assert_data_5.fct = "o";
      __gen_e_acsl_assert_data_5.line = 113;
      __gen_e_acsl_assert_data_5.name = "even";
      __e_acsl_assert(Y % 2 + 1 == 1,& __gen_e_acsl_assert_data_5);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    }
    __gen_e_acsl_active_bhvrs = __e_acsl_contract_partial_count_behaviors
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,2UL,1,0);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
      {.values = (void *)0};
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_7 =
      {.values = (void *)0};
    if (__gen_e_acsl_active_bhvrs != 1) {
      __gen_e_acsl_assert_data_6.blocking = 1;
      __gen_e_acsl_assert_data_6.kind = "Precondition";
      __gen_e_acsl_assert_data_6.pred_txt = "complete behaviors pos, neg";
      __gen_e_acsl_assert_data_6.file = "function_contract.i";
      __gen_e_acsl_assert_data_6.fct = "o";
      __gen_e_acsl_assert_data_6.line = 123;
      __e_acsl_assert(__gen_e_acsl_active_bhvrs >= 1,
                      & __gen_e_acsl_assert_data_6);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
      __gen_e_acsl_assert_data_7.blocking = 1;
      __gen_e_acsl_assert_data_7.kind = "Precondition";
      __gen_e_acsl_assert_data_7.pred_txt = "disjoint behaviors pos, neg";
      __gen_e_acsl_assert_data_7.file = "function_contract.i";
      __gen_e_acsl_assert_data_7.fct = "o";
      __gen_e_acsl_assert_data_7.line = 123;
      __e_acsl_assert(__gen_e_acsl_active_bhvrs <= 1,
                      & __gen_e_acsl_assert_data_7);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_7);
    }
    __gen_e_acsl_active_bhvrs = __e_acsl_contract_partial_count_behaviors
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,2UL,2,3);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_8 =
      {.values = (void *)0};
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_9 =
      {.values = (void *)0};
    if (__gen_e_acsl_active_bhvrs != 1) {
      __gen_e_acsl_assert_data_8.blocking = 1;
      __gen_e_acsl_assert_data_8.kind = "Precondition";
      __gen_e_acsl_assert_data_8.pred_txt = "complete behaviors odd, even";
      __gen_e_acsl_assert_data_8.file = "function_contract.i";
      __gen_e_acsl_assert_data_8.fct = "o";
      __gen_e_acsl_assert_data_8.line = 123;
      __e_acsl_assert(__gen_e_acsl_active_bhvrs >= 1,
                      & __gen_e_acsl_assert_data_8);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_8);
      __gen_e_acsl_assert_data_9.blocking = 1;
      __gen_e_acsl_assert_data_9.kind = "Precondition";
      __gen_e_acsl_assert_data_9.pred_txt = "disjoint behaviors odd, even";
      __gen_e_acsl_assert_data_9.file = "function_contract.i";
      __gen_e_acsl_assert_data_9.fct = "o";
      __gen_e_acsl_assert_data_9.line = 123;
      __e_acsl_assert(__gen_e_acsl_active_bhvrs <= 1,
                      & __gen_e_acsl_assert_data_9);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_9);
    }
    __gen_e_acsl_active_bhvrs = __e_acsl_contract_partial_count_all_behaviors
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_10 =
      {.values = (void *)0};
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_11 =
      {.values = (void *)0};
    __gen_e_acsl_assert_data_10.blocking = 1;
    __gen_e_acsl_assert_data_10.kind = "Precondition";
    __gen_e_acsl_assert_data_10.pred_txt = "all behaviors complete";
    __gen_e_acsl_assert_data_10.file = "function_contract.i";
    __gen_e_acsl_assert_data_10.fct = "o";
    __gen_e_acsl_assert_data_10.line = 123;
    __e_acsl_assert(__gen_e_acsl_active_bhvrs >= 1,
                    & __gen_e_acsl_assert_data_10);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_10);
  }
  o();
  {
    int __gen_e_acsl_assumes_value_2;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_12 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_12,"X",0,X);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_12,"\\old(Y)",0,
                                 __gen_e_acsl_at_5);
    __gen_e_acsl_assert_data_12.blocking = 1;
    __gen_e_acsl_assert_data_12.kind = "Postcondition";
    __gen_e_acsl_assert_data_12.pred_txt = "X == \\old(Y)";
    __gen_e_acsl_assert_data_12.file = "function_contract.i";
    __gen_e_acsl_assert_data_12.fct = "o";
    __gen_e_acsl_assert_data_12.line = 94;
    __e_acsl_assert(X == __gen_e_acsl_at_5,& __gen_e_acsl_assert_data_12);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_12);
    __gen_e_acsl_assumes_value_2 = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value_2) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_13 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_13,"X",0,X);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_13,"\\old(Y)",
                                   0,__gen_e_acsl_at_4);
      __gen_e_acsl_assert_data_13.blocking = 1;
      __gen_e_acsl_assert_data_13.kind = "Postcondition";
      __gen_e_acsl_assert_data_13.pred_txt = "X == \\old(Y)";
      __gen_e_acsl_assert_data_13.file = "function_contract.i";
      __gen_e_acsl_assert_data_13.fct = "o";
      __gen_e_acsl_assert_data_13.line = 99;
      __gen_e_acsl_assert_data_13.name = "neg";
      __e_acsl_assert(X == __gen_e_acsl_at_4,& __gen_e_acsl_assert_data_13);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_13);
    }
    __gen_e_acsl_assumes_value_2 = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value_2) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_14 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_14,"X",0,X);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_14,"\\old(Y)",
                                   0,__gen_e_acsl_at_3);
      __gen_e_acsl_assert_data_14.blocking = 1;
      __gen_e_acsl_assert_data_14.kind = "Postcondition";
      __gen_e_acsl_assert_data_14.pred_txt = "X == \\old(Y)";
      __gen_e_acsl_assert_data_14.file = "function_contract.i";
      __gen_e_acsl_assert_data_14.fct = "o";
      __gen_e_acsl_assert_data_14.line = 104;
      __gen_e_acsl_assert_data_14.name = "pos";
      __e_acsl_assert(X == __gen_e_acsl_at_3,& __gen_e_acsl_assert_data_14);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_14);
    }
    __gen_e_acsl_assumes_value_2 = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,2UL);
    if (__gen_e_acsl_assumes_value_2) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_15 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_15,"X",0,X);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_15,"\\old(Y)",
                                   0,__gen_e_acsl_at_2);
      __gen_e_acsl_assert_data_15.blocking = 1;
      __gen_e_acsl_assert_data_15.kind = "Postcondition";
      __gen_e_acsl_assert_data_15.pred_txt = "X == \\old(Y)";
      __gen_e_acsl_assert_data_15.file = "function_contract.i";
      __gen_e_acsl_assert_data_15.fct = "o";
      __gen_e_acsl_assert_data_15.line = 109;
      __gen_e_acsl_assert_data_15.name = "odd";
      __e_acsl_assert(X == __gen_e_acsl_at_2,& __gen_e_acsl_assert_data_15);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_15);
    }
    __gen_e_acsl_assumes_value_2 = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,3UL);
    if (__gen_e_acsl_assumes_value_2) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_16 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_16,"X",0,X);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_16,"\\old(Y)",
                                   0,__gen_e_acsl_at);
      __gen_e_acsl_assert_data_16.blocking = 1;
      __gen_e_acsl_assert_data_16.kind = "Postcondition";
      __gen_e_acsl_assert_data_16.pred_txt = "X == \\old(Y)";
      __gen_e_acsl_assert_data_16.file = "function_contract.i";
      __gen_e_acsl_assert_data_16.fct = "o";
      __gen_e_acsl_assert_data_16.line = 114;
      __gen_e_acsl_assert_data_16.name = "even";
      __e_acsl_assert(X == __gen_e_acsl_at,& __gen_e_acsl_assert_data_16);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_16);
    }
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    return;
  }
}

/*@ requires X > 0;
    requires X < 10;
    
    behavior b1:
      assumes X == 7;
      ensures X == 8;
    
    behavior b2:
      assumes X == 5;
      ensures X == 98;
 */
void __gen_e_acsl_n(void)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  {
    __gen_e_acsl_contract = __e_acsl_contract_init(2UL);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Precondition";
    __gen_e_acsl_assert_data.pred_txt = "X > 0";
    __gen_e_acsl_assert_data.file = "function_contract.i";
    __gen_e_acsl_assert_data.fct = "n";
    __gen_e_acsl_assert_data.line = 79;
    __e_acsl_assert(X > 0,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"X",0,X);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Precondition";
    __gen_e_acsl_assert_data_2.pred_txt = "X < 10";
    __gen_e_acsl_assert_data_2.file = "function_contract.i";
    __gen_e_acsl_assert_data_2.fct = "n";
    __gen_e_acsl_assert_data_2.line = 80;
    __e_acsl_assert(X < 10,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,X == 7);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,1UL,X == 5);
  }
  n();
  {
    int __gen_e_acsl_assumes_value;
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"X",0,X);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Postcondition";
      __gen_e_acsl_assert_data_3.pred_txt = "X == 8";
      __gen_e_acsl_assert_data_3.file = "function_contract.i";
      __gen_e_acsl_assert_data_3.fct = "n";
      __gen_e_acsl_assert_data_3.line = 83;
      __gen_e_acsl_assert_data_3.name = "b1";
      __e_acsl_assert(X == 8,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"X",0,X);
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Postcondition";
      __gen_e_acsl_assert_data_4.pred_txt = "X == 98";
      __gen_e_acsl_assert_data_4.file = "function_contract.i";
      __gen_e_acsl_assert_data_4.fct = "n";
      __gen_e_acsl_assert_data_4.line = 86;
      __gen_e_acsl_assert_data_4.name = "b2";
      __e_acsl_assert(X == 98,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    }
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    return;
  }
}

/*@ behavior b1:
      assumes X == 7;
      ensures X == 95;
    
    behavior b2:
      assumes X == 5;
      assumes Y == 2;
      ensures X == 7;
      ensures X == \old(X) + Y;
 */
void __gen_e_acsl_m(void)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  long __gen_e_acsl_at;
  {
    int __gen_e_acsl_and;
    __gen_e_acsl_at = (long)X;
    __gen_e_acsl_contract = __e_acsl_contract_init(2UL);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,X == 7);
    if (X == 5) __gen_e_acsl_and = Y == 2; else __gen_e_acsl_and = 0;
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,1UL,
                                           __gen_e_acsl_and);
  }
  m();
  {
    int __gen_e_acsl_assumes_value;
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
      __gen_e_acsl_assert_data.blocking = 1;
      __gen_e_acsl_assert_data.kind = "Postcondition";
      __gen_e_acsl_assert_data.pred_txt = "X == 95";
      __gen_e_acsl_assert_data.file = "function_contract.i";
      __gen_e_acsl_assert_data.fct = "m";
      __gen_e_acsl_assert_data.line = 68;
      __gen_e_acsl_assert_data.name = "b1";
      __e_acsl_assert(X == 95,& __gen_e_acsl_assert_data);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"X",0,X);
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"X",0,X);
      __e_acsl_assert_register_long(& __gen_e_acsl_assert_data_3,"\\old(X)",
                                    0,__gen_e_acsl_at);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"Y",0,Y);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Postcondition";
      __gen_e_acsl_assert_data_3.pred_txt = "X == \\old(X) + Y";
      __gen_e_acsl_assert_data_3.file = "function_contract.i";
      __gen_e_acsl_assert_data_3.fct = "m";
      __gen_e_acsl_assert_data_3.line = 73;
      __gen_e_acsl_assert_data_3.name = "b2";
      __e_acsl_assert((long)X == __gen_e_acsl_at + Y,
                      & __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
      __gen_e_acsl_assert_data_2.blocking = 1;
      __gen_e_acsl_assert_data_2.kind = "Postcondition";
      __gen_e_acsl_assert_data_2.pred_txt = "X == 7";
      __gen_e_acsl_assert_data_2.file = "function_contract.i";
      __gen_e_acsl_assert_data_2.fct = "m";
      __gen_e_acsl_assert_data_2.line = 72;
      __gen_e_acsl_assert_data_2.name = "b2";
      __e_acsl_assert(X == 7,& __gen_e_acsl_assert_data_2);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    }
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    return;
  }
}

/*@ ensures X == 5; */
int __gen_e_acsl_l(void)
{
  int __retres;
  __retres = l();
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Postcondition";
    __gen_e_acsl_assert_data.pred_txt = "X == 5";
    __gen_e_acsl_assert_data.file = "function_contract.i";
    __gen_e_acsl_assert_data.fct = "l";
    __gen_e_acsl_assert_data.line = 59;
    __e_acsl_assert(X == 5,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    return __retres;
  }
}

/*@ behavior b1:
      assumes X == 1;
      requires X == 0;
    
    behavior b2:
      assumes X == 3;
      assumes Y == 2;
      requires X == 3;
      requires X + Y == 5;
 */
void __gen_e_acsl_k(void)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  {
    int __gen_e_acsl_and;
    int __gen_e_acsl_assumes_value;
    __gen_e_acsl_contract = __e_acsl_contract_init(2UL);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,X == 1);
    if (X == 3) __gen_e_acsl_and = Y == 2; else __gen_e_acsl_and = 0;
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,1UL,
                                           __gen_e_acsl_and);
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
      __gen_e_acsl_assert_data.blocking = 1;
      __gen_e_acsl_assert_data.kind = "Precondition";
      __gen_e_acsl_assert_data.pred_txt = "X == 0";
      __gen_e_acsl_assert_data.file = "function_contract.i";
      __gen_e_acsl_assert_data.fct = "k";
      __gen_e_acsl_assert_data.line = 48;
      __gen_e_acsl_assert_data.name = "b1";
      __e_acsl_assert(X == 0,& __gen_e_acsl_assert_data);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"X",0,X);
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"X",0,X);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"Y",0,Y);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Precondition";
      __gen_e_acsl_assert_data_3.pred_txt = "X + Y == 5";
      __gen_e_acsl_assert_data_3.file = "function_contract.i";
      __gen_e_acsl_assert_data_3.fct = "k";
      __gen_e_acsl_assert_data_3.line = 53;
      __gen_e_acsl_assert_data_3.name = "b2";
      __e_acsl_assert(X + (long)Y == 5L,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
      __gen_e_acsl_assert_data_2.blocking = 1;
      __gen_e_acsl_assert_data_2.kind = "Precondition";
      __gen_e_acsl_assert_data_2.pred_txt = "X == 3";
      __gen_e_acsl_assert_data_2.file = "function_contract.i";
      __gen_e_acsl_assert_data_2.fct = "k";
      __gen_e_acsl_assert_data_2.line = 52;
      __gen_e_acsl_assert_data_2.name = "b2";
      __e_acsl_assert(X == 3,& __gen_e_acsl_assert_data_2);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    }
  }
  k();
  __e_acsl_contract_clean(__gen_e_acsl_contract);
  return;
}

/*@ behavior b1:
      requires X == 5;
      ensures X == 3;
    
    behavior b2:
      requires X == 3 + Y;
      requires Y == 2;
      ensures X == Y + 1;
 */
void __gen_e_acsl_j(void)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  {
    int __gen_e_acsl_assumes_value;
    __gen_e_acsl_contract = __e_acsl_contract_init(2UL);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,1);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,1UL,1);
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
      __gen_e_acsl_assert_data.blocking = 1;
      __gen_e_acsl_assert_data.kind = "Precondition";
      __gen_e_acsl_assert_data.pred_txt = "X == 5";
      __gen_e_acsl_assert_data.file = "function_contract.i";
      __gen_e_acsl_assert_data.fct = "j";
      __gen_e_acsl_assert_data.line = 35;
      __gen_e_acsl_assert_data.name = "b1";
      __e_acsl_assert(X == 5,& __gen_e_acsl_assert_data);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"X",0,X);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"Y",0,Y);
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"Y",0,Y);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Precondition";
      __gen_e_acsl_assert_data_3.pred_txt = "Y == 2";
      __gen_e_acsl_assert_data_3.file = "function_contract.i";
      __gen_e_acsl_assert_data_3.fct = "j";
      __gen_e_acsl_assert_data_3.line = 39;
      __gen_e_acsl_assert_data_3.name = "b2";
      __e_acsl_assert(Y == 2,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
      __gen_e_acsl_assert_data_2.blocking = 1;
      __gen_e_acsl_assert_data_2.kind = "Precondition";
      __gen_e_acsl_assert_data_2.pred_txt = "X == 3 + Y";
      __gen_e_acsl_assert_data_2.file = "function_contract.i";
      __gen_e_acsl_assert_data_2.fct = "j";
      __gen_e_acsl_assert_data_2.line = 38;
      __gen_e_acsl_assert_data_2.name = "b2";
      __e_acsl_assert((long)X == 3L + Y,& __gen_e_acsl_assert_data_2);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    }
  }
  j();
  {
    int __gen_e_acsl_assumes_value_2;
    __gen_e_acsl_assumes_value_2 = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value_2) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"X",0,X);
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "Postcondition";
      __gen_e_acsl_assert_data_4.pred_txt = "X == 3";
      __gen_e_acsl_assert_data_4.file = "function_contract.i";
      __gen_e_acsl_assert_data_4.fct = "j";
      __gen_e_acsl_assert_data_4.line = 36;
      __gen_e_acsl_assert_data_4.name = "b1";
      __e_acsl_assert(X == 3,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
    }
    __gen_e_acsl_assumes_value_2 = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value_2) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"X",0,X);
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"Y",0,Y);
      __gen_e_acsl_assert_data_5.blocking = 1;
      __gen_e_acsl_assert_data_5.kind = "Postcondition";
      __gen_e_acsl_assert_data_5.pred_txt = "X == Y + 1";
      __gen_e_acsl_assert_data_5.file = "function_contract.i";
      __gen_e_acsl_assert_data_5.fct = "j";
      __gen_e_acsl_assert_data_5.line = 40;
      __gen_e_acsl_assert_data_5.name = "b2";
      __e_acsl_assert((long)X == Y + 1L,& __gen_e_acsl_assert_data_5);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    }
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    return;
  }
}

/*@ requires X == 3;
    requires Y == 2; */
void __gen_e_acsl_i(void)
{
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Precondition";
    __gen_e_acsl_assert_data.pred_txt = "X == 3";
    __gen_e_acsl_assert_data.file = "function_contract.i";
    __gen_e_acsl_assert_data.fct = "i";
    __gen_e_acsl_assert_data.line = 27;
    __e_acsl_assert(X == 3,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"Y",0,Y);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Precondition";
    __gen_e_acsl_assert_data_2.pred_txt = "Y == 2";
    __gen_e_acsl_assert_data_2.file = "function_contract.i";
    __gen_e_acsl_assert_data_2.fct = "i";
    __gen_e_acsl_assert_data_2.line = 28;
    __e_acsl_assert(Y == 2,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
  }
  i();
  return;
}

/*@ requires X == 2; */
void __gen_e_acsl_h(void)
{
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Precondition";
    __gen_e_acsl_assert_data.pred_txt = "X == 2";
    __gen_e_acsl_assert_data.file = "function_contract.i";
    __gen_e_acsl_assert_data.fct = "h";
    __gen_e_acsl_assert_data.line = 21;
    __e_acsl_assert(X == 2,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  h();
  return;
}

/*@ ensures X == 2;
    ensures Y == 2; */
void __gen_e_acsl_g(void)
{
  g();
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Postcondition";
    __gen_e_acsl_assert_data.pred_txt = "X == 2";
    __gen_e_acsl_assert_data.file = "function_contract.i";
    __gen_e_acsl_assert_data.fct = "g";
    __gen_e_acsl_assert_data.line = 14;
    __e_acsl_assert(X == 2,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"Y",0,Y);
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Postcondition";
    __gen_e_acsl_assert_data_2.pred_txt = "Y == 2";
    __gen_e_acsl_assert_data_2.file = "function_contract.i";
    __gen_e_acsl_assert_data_2.fct = "g";
    __gen_e_acsl_assert_data_2.line = 15;
    __e_acsl_assert(Y == 2,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    return;
  }
}

/*@ ensures X == 1; */
void __gen_e_acsl_f(void)
{
  f();
  {
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"X",0,X);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Postcondition";
    __gen_e_acsl_assert_data.pred_txt = "X == 1";
    __gen_e_acsl_assert_data.file = "function_contract.i";
    __gen_e_acsl_assert_data.fct = "f";
    __gen_e_acsl_assert_data.line = 8;
    __e_acsl_assert(X == 1,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    return;
  }
}


