#include <iostream>
using namespace std;

extern int bar(int);

int main(int argc, char **argv) {
  cout << "Hello, World!" << endl;
  cout << "Fib of 10 is " << bar(10) << endl;
  return 0;
}
