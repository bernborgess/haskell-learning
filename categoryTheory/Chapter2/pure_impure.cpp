
// a) The factorial function
int fact(int n) {
    int i;
    int result = 1;
    for (i = 2; i <= n; ++i)
        result *= i;
    return result;
}    

// b) std::getchar()

// c) 
bool hello() {
    std::cout << "Hello!" << std::endl;
    return true;
}

// d)
int also(int x) 
{
    static int y = 0;
    y += x;
    return y;
}    
