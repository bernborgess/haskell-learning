#include <cstring>
#include <iostream>


char abc(int i) { return i % 26 + 'A'; }
bool vow(char c) { return strchr("AEIOU",c) != nullptr; }
void sid(bool b) { std::cout << b << std::endl; }

int main() {
    int i;
    std::cout<<"gib int"<<std::endl;
    std::cin>>i;
    sid(vow(abc(i)));
}
