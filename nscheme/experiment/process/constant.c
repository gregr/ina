// gcc -o constant constant.c && ./constant && rm constant
#include <stdio.h>
#include <fcntl.h>
//#include <unistd.h>  // only needed for exec

int main(int argc, char** argv) {

  //execl("/bin/ls", "/bin/ls", "/", 0);
  //execl("/usr/bin/printenv", "/usr/bin/printenv", 0);

  printf("%d\n", F_SETFL);
  printf("%d\n", F_GETFL);
  printf("%d\n", F_GETFD);
  printf("%d\n", O_RDONLY);
  printf("%d\n", O_WRONLY);
  printf("%d\n", O_RDWR);
  printf("%d\n", O_NONBLOCK);
  printf("%d\n", O_ASYNC);
  printf("%d\n", O_APPEND);
  printf("%d\n", O_CLOEXEC);
  printf("%d\n", O_TRUNC);
  printf("%d\n", O_SYNC);
  printf("%d\n", O_NOFOLLOW);
  printf("%d\n", O_NOCTTY);
  printf("%d\n", O_DSYNC);
  printf("%d\n", O_CREAT);

  printf("%d\n", FD_CLOEXEC);
  return 0;
}

/*
;(require ffi/unsafe)
;(let-values (((in out) (open-pipe-streams/k panic values)))
;  (let ((fd.in  (cdr (assoc 'file-descriptor (in 'description))))
;        (fd.out (cdr (assoc 'file-descriptor (in 'description)))))
;    (define F_GETFD    1)
;    (define F_GETFL    3)
;    (define O_NONBLOCK 4)
;    (define lib (ffi-lib #f))
;    (define fcntl (get-ffi-obj "fcntl" lib (_fun _int _int _int -> _int)))
;
;    (pretty-write `((fcntl fd.in     F_GETFL 0): ,(fcntl fd.in  F_GETFL 0)))
;    (pretty-write `((fcntl fd.out    F_GETFL 0): ,(fcntl fd.out F_GETFL 0)))
;    (pretty-write `((fcntl fd.stdin  F_GETFL 0): ,(fcntl 0      F_GETFL 0)))
;    (pretty-write `((fcntl fd.stdout F_GETFL 0): ,(fcntl 1      F_GETFL 0)))
;    (pretty-write `((fcntl fd.stderr F_GETFL 0): ,(fcntl 2      F_GETFL 0)))
;
;    (pretty-write `((fcntl fd.in     F_GETFD 0): ,(fcntl fd.in  F_GETFD 0)))
;    (pretty-write `((fcntl fd.out    F_GETFD 0): ,(fcntl fd.out F_GETFD 0)))
;    (pretty-write `((fcntl fd.stdin  F_GETFD 0): ,(fcntl 0      F_GETFD 0)))
;    (pretty-write `((fcntl fd.stdout F_GETFD 0): ,(fcntl 1      F_GETFD 0)))
;    (pretty-write `((fcntl fd.stderr F_GETFD 0): ,(fcntl 2      F_GETFD 0)))
;    ))
*/
