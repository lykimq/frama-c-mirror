/* run.config
  STDOPT: +"-slice-calls send -lib-entry -main g -slicing-level 0 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send -lib-entry -main g -slicing-level 1 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send -lib-entry -main g -slicing-level 2 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send -lib-entry -main g -slicing-level 3 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send,send_bis -lib-entry -main g -slicing-level 0 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send,send_bis -lib-entry -main g -slicing-level 1 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send,send_bis -lib-entry -main g -slicing-level 2 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send,send_bis -lib-entry -main g -slicing-level 3 -no-slice-callers -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send,send_bis -lib-entry -main g -slicing-level 1 -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send,send_bis -lib-entry -main g -slicing-level 2 -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
  STDOPT: +"-slice-calls send,send_bis -lib-entry -main g -slicing-level 3 -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check"
*/
int G,H,I;

int get (int y) ;

void send(int x);
void send_bis(int x);

void k_bis(int ab, int c, int d) {
  H = c;
  if (ab)
    send_bis (d);
}

int k(int a, int b, int c, int d) {
  int cond = get (d) ;
  G = b;
  k_bis (cond, c, d);
  return a;
}

void g(int b, int c) {
  int r = k(0,0,c,0);
  f(b);
}

int f(int y) {
  k(0,0,0,0);
  int r = k(0,y,0,0);
  int z = k(G,0,0,0);
  //@ slice_preserve_expr z;
  send (z);
  return z;
}
