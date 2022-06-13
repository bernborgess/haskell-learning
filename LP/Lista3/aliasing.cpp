#include <bits/stdc++.h>

// ! Risco de passagem por referencia: aliasing

void sigsum(int& n, int& ans) {
  ans = 0;
  int i = 1;
  while (i<=n) {
    ans += i;
    i++;
  }
}

int main() {
  int x = 10;
  int y;
  sigsum(x,y);
  printf("x = %d, y = %d\n",x,y);

  //! A bosta
  x = 10;
  sigsum(x,x);
  printf("x = %d, y = %d\n",x,x);

  return 0;
}

