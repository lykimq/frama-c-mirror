/* run.config

   STDOPT: +"-impact-annot impact" +"-lib-entry" +"-main impact"
   */

int a, b, c, e, x, y, z, f, w;

void impact() {
  /*@ impact_stmt; */
  b = a;
  if (c) {
    x = b + c;
    y = x + e;
  } else
    z = 12;
  z = 13;
  z = y + f;
  w = b;
}
