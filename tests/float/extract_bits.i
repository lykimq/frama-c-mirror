/* run.config*
  OPT: -eva @EVA_CONFIG@ -eva-slevel 10 -big-ints-hex 0 -machdep ppc_32 -float-normal -kernel-warn-key parser:decimal-float=active
  OPT: -eva @EVA_CONFIG@ -eva-slevel 10 -big-ints-hex 0 -machdep x86_32 -float-normal -kernel-warn-key parser:decimal-float=active
*/

float f = 3.14;
double d = 2.71;

double stdtod_bug = 1.8254370818746402660437411213933955878019332885742187;
/*
http://www.exploringbinary.com/a-bug-in-the-bigcomp-function-of-david-gays-strtod/
*/

int fr[4];
int dr[8];

void main() {
  int i;
  for (i=0; i<4; i++)
    fr[i] = ((unsigned char*) &f)[i];
  for (i=0; i<8; i++)
    dr[i] = ((unsigned char*) &d)[i];

  long double x = 1.;
  Frama_C_show_each(*(unsigned char *)&x);
}
