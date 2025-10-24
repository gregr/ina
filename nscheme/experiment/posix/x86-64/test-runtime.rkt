#lang racket/base  ; TODO: remove dependency on racket
(require "../racket/nscheme.rkt")
(define out (open-output-file "test-runtime.c"))
(define (emit . str*) (for-each (lambda (str) (write-string str out)) str*))

(define (sep separator . s*) (sep* separator s*))
(define (sep* separator s*)
  (let loop ((s (car s*)) (s* (cdr s*)))
    (if (null? s*)
        (list s)
        (cons s (cons separator (loop (car s*) (cdr s*)))))))

(define (symbol?! x) (unless (symbol? x) (error "not a symbol" x)))

(define (C-type stx)
  (cond ((symbol? stx)                 (symbol->string stx))
        ((and (list? stx) (pair? stx)) (map C-type stx))
        (else                          (error "not a C type" stx))))

(define (C-statement stx)
  (define (fail!) (error "not a C statement" stx))

  include
  typedef

  type followed by variable name
  - the ?

  type followed by function name, parenthesized parameters, body
  - call this fn or def ?

  assignment, optionally with a type on lhs

  call

  return break continue

  alternative postfix struct access operators:
  .y
  ->y

  if
  for
  while
  do while

  switch
  label
  goto

  macros:
  %if ; one- or two-armed
  %ifdef
  %ifndef
  %define

  %verbatim


  )

(define (C-expr stx)
  (define (fail!) (error "not a C expression" stx))
  ;(define (wrap . str*)
  ;  (apply string-append
  ;         (cons "("
  ;               (let loop ((s0 (car str*)) (s* (cdr str*)))
  ;                 (if (null? s*)
  ;                     (list s0 ")")
  ;                     (append (list s0 " ") (loop (car s*) (cdr s*))))))))
  (define ($call rator rand*)
    (if (null? rand*)
        (list rator (vector "(" ")"))
        (list rator (vector "(" (sep* "," rand*) ")"))))
  (cond
    ((number?  stx) (number->string stx))
    ((string?  stx) (call-with-output-string (lambda (out) (write stx out))))
    ((boolean? stx) (if stx "true" "false"))
    ((symbol?  stx) (symbol->string stx))
    ((vector?  stx) (vector "{" (sep* "," (map C-expr (vector->list stx))) "}"))
    ((bytes?   stx) (vector "{" (sep* "," (map C-expr (bytes->u8* stx))) "}"))
    ((pair?    stx)
     (unless (list? stx) (fail!))
     (let* ((rator (car stx))
            (rand* (cdr stx))
            (randc (length rand*)))
       (define (randc=! n) (unless (= randc n) (fail!)))
       (cond
         ((= randc 1)
          (let ((rand (car rand*)))
            (case rator
              ((! ~ ++ -- * & - sizeof) (list (symbol->string rator) (C-expr rand)))
              (else                     (list (symbol->string rator) (list rand))))))
         ((< 1 randc)
          (case rator
            ((call) ($call (C-expr (car rand*)) (map C-expr (cdr rand*))))
            ((cast) (randc=! 2) (list (vector "(" (C-type (car rand*)) ")") (C-expr (cadr rand*))))
            ((if)
             (randc=! 3)
             (list (C-expr (car rand*)) "?" (C-expr (cadr rand*)) ":" (C-expr (caddr rand*))))
            ((begin) (sep* "," (map C-expr rand*)))
            ((->)
             (randc=! 2)
             (symbol?! (cadr rand*))
             (list (C-expr (car rand*)) "->" (C-expr (cadr rand*))))
            ((@)
             (randc=! 2)
             (symbol?! (cadr rand*))
             (list (C-expr (car rand*)) "." (C-expr (cadr rand*))))
            (([])
             (randc=! 2)
             (vector (C-expr (car rand*)) "[" (C-expr (cadr rand*)) "]"))
            ((+ - * &) (sep* (symbol->string rator) (map C-expr rand*)))
            ((and) (sep* "&&" (map C-expr rand*)))
            ((or)  (sep* "||" (map C-expr rand*)))
            ((bor) (sep* "|" (map C-expr rand*)))
            ((bor=)
             (randc=! 2)
             (list (C-expr (car rand*)) "|=" (C-expr (cadr rand*))))
            ((= += -= *= /= %= <<= >>= &= ^= ^ / % << >> == != < <= > >=)
             (randc=! 2)
             (list (C-expr (car rand*)) (symbol->string rator) (C-expr (cadr rand*))))
            (else ($call (C-expr rator) (map C-expr rand*)))))
         (else ($call (C-expr rator) (map C-expr rand*))))))
    (else (fail!))))

(emit
"#include <stdio.h>   // printf
#include <stdlib.h>  // exit
//#include <stddef.h>  // ptrdiff_t
//#include <stdint.h>  // SIZE_MAX
//#include <limits.h>  // SSIZE_MAX
#include <sys/mman.h>  // mmap
//#include <unistd.h>  // sysconf
#ifdef __APPLE__
#define HOST_OS_NAME \"apple\"
#endif
#ifdef __linux__
#define HOST_OS_NAME \"linux\"
#endif

typedef signed   char      s8;
typedef unsigned char      u8;
typedef signed   short     s16;
typedef unsigned short     u16;
typedef signed   int       s32;
typedef unsigned int       u32;
typedef signed   long long s64;
typedef unsigned long long u64;
typedef float              f32;
typedef double             f64;

#define BLOCK_SIZE ((u64)1<<(u64)14)

static u64 memory_size = 0;
static u8 *memory = 0;

")

(let ((assumptions
        '(("u8*" 8)
          ("s8" 1) ("u8" 1)
          ("s16" 2) ("u16" 2)
          ("s32" 4) ("u32" 4)
          ("s64" 8) ("u64" 8)
          ("f32" 4) ("f64" 8))))
  (define (check-size= expr expected)
    (let ((expected (number->string expected)))
      (emit "  if (sizeof(" expr ") != " expected ") {\n"
            "    fprintf(stderr, \"sizeof(" expr ")=%lu != " expected "\\n\", sizeof(" expr "));\n"
            "    fail = 1;\n  }\n")))
  (emit "void check_system_assumptions() {\n  int fail = 0;\n")
  (for-each check-size= (map car assumptions) (map cadr assumptions))
  (emit "  if (fail) exit(1);\n}\n"))

  (emit
"
void* os_memory_alloc(u64 bytes) {
  void* result = mmap(0, bytes, PROT_EXEC|PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (result == MAP_FAILED) {
    fprintf(stderr, \"os_memory_alloc failed\\n\");
    exit(1);
  }
  return result;
}

void os_memory_free(u8* memory, u64 bytes) {
  if (munmap(memory, bytes) != 0) {
    fprintf(stderr, \"os_memory_free failed\\n\");
    exit(1);
  }
}

int main(int argc, char** argv) {
  check_system_assumptions();

  memory_size = ((u64)BLOCK_SIZE*(u64)10);

  memory = os_memory_alloc(memory_size);

  memory[0] = 7;
  memory[10] = 77;

  os_memory_free(memory, memory_size);

  printf(\"NULL: %p\\n\", NULL);

  /*printf(\"sizeof(size_t): %lu\\n\", sizeof(size_t));*/
  /*printf(\"sizeof(ssize_t): %lu\\n\", sizeof(ssize_t));*/
  //printf(\"sizeof(ptrdiff_t): %lu\\n\", sizeof(ptrdiff_t));

  printf(\"sizeof(u8): %lu\\n\", sizeof(u8));
  printf(\"HOST_OS_NAME: %s\\n\", HOST_OS_NAME);
  printf(\"memory_size: %llu\\n\", memory_size);
  //printf(\"SIZE_MAX, SSIZE_MAX: %llu, %ld\\n\", SIZE_MAX, SSIZE_MAX);
  return 0;
}
")
