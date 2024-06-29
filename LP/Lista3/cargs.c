#include <stdarg.h>
#include <stdio.h>

int foo(size_t nargs, ...) {
  int sum = 0;
  va_list ap;
  va_start(ap, nargs);
  while(nargs--) {sum += va_arg(ap, int);}
  va_end(ap);
  return sum;
}

int main() {
  printf("%d\n", foo(0));
  printf("%d\n", foo(1, 1));
  printf("%d\n", foo(2, 1, 1));
  printf("%d\n", foo(3, 1, 1, 1));
  printf("%d\n", foo(4, 1, 1, 1, 1));

  printf("%d%d%d",0,0);
}