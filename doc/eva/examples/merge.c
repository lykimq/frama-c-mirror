int x,y;
char t[10];

int main(int c)
{
  if (c)
    * (int**) t = &x;
  else
    * (int**) (t+2) = &y;
  x = t[2];
  return x;
}
