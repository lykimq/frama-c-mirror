/* run.config
   STDOPT: +"-deps -lib-entry -main f1 -slice-annot f1 -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -lib-entry -main f1 -slice-assert f1 -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -lib-entry -main f2 -slice-annot f2 -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -lib-entry -main f2 -slice-assert f2 -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -main test_infinite_loop_3 -slice-value G -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -main test_infinite_loop_4 -slice-value G -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -main test_infinite_loop_5 -slice-value G -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -main loop -slice-value Z  -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -slice-calls loop -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -slice-annot loop -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -slice-assert loop -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -main loop -slice-rd Y -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -main loop -slice-rd Z -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -main loop -slice-wr Y -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -main loop -slice-wr Z  -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -lib-entry -main stop_f1 -slice-annot stop_f1 -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -lib-entry -main stop_f1 -slice-assert stop_f1 -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -lib-entry -main stop_f2 -slice-annot stop_f2 -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -lib-entry -main stop_f2 -slice-assert stop_f2  -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -slice-value Z  -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -slice-rd Y -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -slice-rd Z -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -slice-wr Y -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -slice-wr Z -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"
   STDOPT: +"-deps -lib-entry -main alarm -slice-threat alarm -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps"


 */

int f1 (int c) {
  int x = 0, s = 0;
  if (c) {
    while(1) { /* infinite loop */
      s++;
      //@ assert s > 0 ;
      }
    //@ assert \false ;
    }
  else
    x = 1;
   //@ slice_preserve_stmt;
  x ++;
  return x;
}

void f2 (int c) {
  int x1 = 0, x2 = 0;

  while (1) {
    if (c)
      x1++;
    else
      x2++;
    //@slice_preserve_expr x1;
    //@ assert x2 > 0 ;
  }
}

/*-------------------------------------------*/
void stop(void) __attribute__ ((noreturn)) ;

int stop_f1 (int c) {
  int x = 0, s = 0;
  if (c) {
    while(s < c) {
      s++;
      //@ assert s > 0 ;
      }
    stop () ; /* never returns */
    }
  else
    x = 1;
   //@ slice_preserve_stmt;
  x ++;
  return x;
}

void stop_f2 (int c) {
  int x1 = 0, x2 = 0;

  while (x1+x2 < c + 10) {
    if (c)
      x1++;
    else
      x2++;
    //@slice_preserve_expr x1;
    //@ assert x2 > 0 ;
    stop () ; /* never loops nor returns */
    x1++;     /* dead code */
    //@ assert \false ;
  }
}
/*-------------------------------------------*/
int G ;
void test_infinite_loop_3 (int ctrl1, int ctrl2,
                           int no_ctrl,
                           int data1, int data2,
                           int no_data) {
  G = 0 ;
  if (ctrl1) {
    G = data1 ;
    if (no_ctrl) { /* Don't control an assignment of G
                    * which leads to the return */
      G = no_data ; /* Don't affect the final value of G
                     * because the assignment
                     * does not lead to the return */
      while (1)
        G = no_data ; /* Don't affect the final value of G
                       * because the assignment
                       * does not lead to the return */
      G = no_data ; /* Don't affect the final value of G
                     * because the assignment
                     * is dead code */
      }
    if (ctrl2)
      G = data2 ;
    }
  return;
}

void test_infinite_loop_4 (int ctrl1, int ctrl2, int no_ctrl,
                           int data1, int data2, int no_data) {
  G = 0 ;
  while (ctrl1) {
    G += data1 ;
    if (no_ctrl) { /* Don't control an assignment of G
                    * which leads to the return */
      G += no_data ; /* Don't affect the final value of G
                     * because the assignment
                     * does not lead to  the return */
      while (1)
        G += no_data ;  /* Don't affect the final value of G
                       * because the assignment
                       * does not lead to the return */
      G += no_data ; /* Don't affect the final value of G
                      * because the assignment
                      * is dead code */
      }
    if (ctrl2)
      G += data2 ;
    }
  return;
}

void test_infinite_loop_5 (int ctrl1, int ctrl2, int no_ctrl,
                           int data1, int data2, int no_data) {
  G = 0 ;
  while (ctrl1) {
    G += data1 ;
    if (no_ctrl) { /* Don't control the final value of G.
                    * It only controls the terminaison of the function.
                    */
      G += no_data ;   /* Don't affect ... */
      while (1)
        G += no_data ; /* Don't affect ... */
      G += no_data ;   /* Don't affect ...  dead code */
      }
    else /* <-- This is the difference with test_infinite_loop_4.
          * It is only a syntactical difference,
          * and not a semantical difference
          * since the previous statement "G += no_data" is dead.
          */
      if (ctrl2)
        G += data2 ;
    }
  return;
}
/*-------------------------------------------*/
int C1 = 1, C2 = 1 ;
int X, Y, Z ;

void loop (int cond) {
  if (cond) {
    int c = 0 ;
    while (1) {
      //@ slice_preserve_ctrl ;
      if (c) {
        X++;
        Y = Z ;
        }
      c=1;
      //@ assert  c==1 ;
      }
    }
  Z = Y ; // dead code with -main main
}
/*---------------------*/
/*@ assigns *p \from p, y, Z ;
 */
void may_write_Y_from_Z (int * p, int y) ;
void test_assigns (int * p, int y) { if (y < Z) *p = y + Z; }
/*---------------------*/
void main (int y) {
  int no_ctrl = 1 ;
  Z = 0;
  if (no_ctrl)
    Z = X ;
  may_write_Y_from_Z (&Y, y) ;
  if (C1) {
    int cond = C2 ;
    loop (cond) ;
    }
}
/*-------------------------------------------*/

void alarm() {
  int i = 1;
  volatile int j = 3;
  //@ assert i == 1;
  j++;
}
