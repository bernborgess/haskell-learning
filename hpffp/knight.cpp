// Courtesy of ChatGPT 2023-02-18
#include <iostream>

int main() {
  int x = 0, y = 0;

  while (true) {
    char c;
    std::cout << "Enter a direction (N, S, E, W, or Q to quit): ";
    std::cin >> c;

    if (c == 'N') {
      y++;
    } else if (c == 'S') {
      y--;
    } else if (c == 'E') {
      x++;
    } else if (c == 'W') {
      x--;
    } else if (c == 'Q') {
      break;
    } else {
      std::cout << "Invalid direction!" << std::endl;
    }

    std::cout << "Knight is at (" << x << ", " << y << ")" << std::endl;
  }

  return 0;
}
