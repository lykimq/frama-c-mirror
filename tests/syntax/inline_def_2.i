/* run.config
DONTRUN: main test is in inline_def_1.i
*/

int f(int x) { return x + 1; }

inline int f1 () { return 2; }

extern int f2(void);

int h(int x) { return f(x) + f1() + f2(); }
