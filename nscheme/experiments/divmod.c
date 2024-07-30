#include <stdio.h>

void showdivmod(int a, int b) {
  int c = a / b;
  int d = a % b;
  printf("%d divmod %d gives %d and %d\n", a, b, c, d);
}

int main() {
  showdivmod(123, 10);
  showdivmod(123, -10);
  showdivmod(-123, 10);
  showdivmod(-123, -10);
  return 0;
}

/* Output:
123 divmod 10 gives 12 and 3
123 divmod -10 gives -12 and 3
-123 divmod 10 gives -12 and -3
-123 divmod -10 gives 12 and -3
*/
