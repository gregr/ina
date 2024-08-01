#include <stdio.h>
/*extern;*/

/*//typedef foo;*/

/*typedef;*/

/*short short int x;*/

/*long long int y;*/

/*long double z;*/

struct ok { int foo : 5; };  // :5 is a bit field, not a default value

int main() {
  struct ok bar;
  printf("%d\n", bar.foo);
  return 0;
}
