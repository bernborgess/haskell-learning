#include <bits/stdc++.h>

using namespace std;

void swap(int& x,int& y) {
  int aux = x;
  x = y;
  y = aux;
}

int main() { 
  int a = 2;
  int b = 3;
  cout << a << ' ' << b << endl;
  swap(a,b);
  cout << a << ' ' << b << endl;
  return 0;
}

