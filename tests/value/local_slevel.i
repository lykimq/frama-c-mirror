/* run.config*
 PLUGIN: @EVA_MAIN_PLUGINS@ constant_propagation
   STDOPT: +" -eva-slevel-function main2:100000 -print -then -scf -then-on propagated -eva -eva-show-progress -no-scf"
*/
int *p;

void main1() {
  int v;
  unsigned int r = 0;
  for (int i=0; i<80; i++) {
    //@ assigns v; ensures \true;  // Also test the pretty-printer
    //@ slevel 50;
    if (i%2) {
      v = 1;
    } else {
      v = -1;
    }
    Frama_C_show_each(v, i, r);
    v = v * v;
    r = r + 2 / (v+1);
    //@ slevel default;
    ;
  }
}

void g() {// Do not crash when loop unrolling clears the dependencies of the AST
  //@ loop unfold 1;
  for (int i=0; i<5; i++) {
  }
}

int t[100];
volatile int vol;

void main2() {
  for (int i = 0; i < 100; i++) {
    Frama_C_show_each(i);
    int n = vol;
    if (n>=3) {
      t[i] = n;
    } else t[i] = n+(1 << 30)+1;
    //@ slevel merge; // same effect as merge-after-loop; uses 200 slevel
    ;
  }
}

void main() {
  main1();
  main2();
}
