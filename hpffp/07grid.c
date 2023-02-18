#include <stdio.h>

int f(int i, int j) {
  return ((6 * i + 7 * j - i * i) % 23) == 0;
}

int main() {
  int Mx = 10, My = 20;
  for (int i = My - 1; i >= 0; i--) {
    printf("%2d", i);
    for (int j = 0; j < Mx; j++) {
      if (f(i, j))
        printf("██");
      else
        printf("  ");
    }
    printf("\n");
  }
  printf("  0 1 2 3 4 5 6 7 8 9 \n");
  return 0;
}