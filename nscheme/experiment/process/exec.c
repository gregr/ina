#include <errno.h>
#include <string.h>
#include <unistd.h>
char* get_error() {
  extern int errno;
  return strerror(errno);
}
int get_errno() {
  extern int errno;
  return errno;
}
int exec_ls() {
  //return execl("/bin/ls", "/bin/ls", "-l", 0);
  return execl("/bin/ls", "/bin/ls", 0);
}
int exec1(char* a) {
  return execl(a, a, 0);
}
int exec2(char* a, char* b) {
  return execl(a, a, b, 0);
}
