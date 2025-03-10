/* run.config*
   
   STDOPT: +"-eva-msg-key initial-state -eva-initialization-padding-globals no -lib-entry -main main -eva-context-depth 3 -then -main main2 -then -eva-context-width 4"
*/
int f_int(int x);

int *gpi;
/*@ assigns \result \from indirect:x, gpi; */
int *f_star_int(int x);

int ****G; volatile int v;
int G0,*G1;

typedef int (*pfun)(int *p1, const int *p2);



pfun gen(void);
extern pfun f;

float *gpf;
/*@ assigns \result \from gpf; */
float *i(void);

double *gpd;
/*@ assigns \result \from gpd; */
double *k(void);

void main(pfun g) {
  G0 = f_int(2);
  G1 = f_star_int(5);
  *G1 = 5;
  ****G=1;

  int x = 3;
  int y = 4;
  pfun h = gen();
  if (v) { int z1 = f(&x, &y); }
  if (v) { int z2 = g(&x, &y); }
  if (v) { int z3 = h(&x, &y); }
  float *pf = i();
  float vf = *pf;
  *pf = 1.;
  *pf += *pf;
  double *pd = k();
  *pd = 2.;
}

struct {
  void (*f[2])();
} s;


struct {
  struct ss *p[8];
  struct ss *(*q)[8];
} ss;

void (*ff)();

struct {
  short bf1: 5;
  short  : 0; // 0-sized bitfield: do not attempt to initialize it
  unsigned int control: 14, : 0;
} s_bitfield;

void main2(){
  if (v) {
    //@ assert Unknown: G1 != 0;
    //@ assert \block_length(G1) >= 4;
    //@ assert \block_length(G1) <= 16;
    if (v) {
      //@ assert Unknown: \block_length(G1) == 4;
      Frama_C_show_each_reached_1();
    }
    if (v) {
      //@ assert Unknown: \block_length(G1) == 8; // True with context-width 2 and 4
      Frama_C_show_each_reached_2();
    }
    if (v) {
      //@ assert Unknown_Invalid: \block_length(G1) == 16; // True with context-width 4
      Frama_C_show_each_reached_3();
    }
  }
}
