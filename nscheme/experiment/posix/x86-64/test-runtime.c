#include <stdio.h>   // printf
#include <stdlib.h>  // exit
//#include <stddef.h>  // ptrdiff_t
//#include <stdint.h>  // SIZE_MAX
//#include <limits.h>  // SSIZE_MAX
#include <sys/mman.h>  // mmap
//#include <unistd.h>  // sysconf
#ifdef __APPLE__
#define HOST_OS_NAME "apple"
#endif
#ifdef __linux__
#define HOST_OS_NAME "linux"
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

void check_system_assumptions() {
  int fail = 0;
  if (sizeof(u8*) != 8) {
    fprintf(stderr, "sizeof(u8*)=%lu != 8\n", sizeof(u8*));
    fail = 1;
  }
  if (sizeof(s8) != 1) {
    fprintf(stderr, "sizeof(s8)=%lu != 1\n", sizeof(s8));
    fail = 1;
  }
  if (sizeof(u8) != 1) {
    fprintf(stderr, "sizeof(u8)=%lu != 1\n", sizeof(u8));
    fail = 1;
  }
  if (sizeof(s16) != 2) {
    fprintf(stderr, "sizeof(s16)=%lu != 2\n", sizeof(s16));
    fail = 1;
  }
  if (sizeof(u16) != 2) {
    fprintf(stderr, "sizeof(u16)=%lu != 2\n", sizeof(u16));
    fail = 1;
  }
  if (sizeof(s32) != 4) {
    fprintf(stderr, "sizeof(s32)=%lu != 4\n", sizeof(s32));
    fail = 1;
  }
  if (sizeof(u32) != 4) {
    fprintf(stderr, "sizeof(u32)=%lu != 4\n", sizeof(u32));
    fail = 1;
  }
  if (sizeof(s64) != 8) {
    fprintf(stderr, "sizeof(s64)=%lu != 8\n", sizeof(s64));
    fail = 1;
  }
  if (sizeof(u64) != 8) {
    fprintf(stderr, "sizeof(u64)=%lu != 8\n", sizeof(u64));
    fail = 1;
  }
  if (sizeof(f32) != 4) {
    fprintf(stderr, "sizeof(f32)=%lu != 4\n", sizeof(f32));
    fail = 1;
  }
  if (sizeof(f64) != 8) {
    fprintf(stderr, "sizeof(f64)=%lu != 8\n", sizeof(f64));
    fail = 1;
  }
  if (fail) exit(1);
}

void* os_memory_alloc(u64 bytes) {
  void* result = mmap(0, bytes, PROT_EXEC|PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (result == MAP_FAILED) {
    fprintf(stderr, "os_memory_alloc failed\n");
    exit(1);
  }
  return result;
}

void os_memory_free(u8* memory, u64 bytes) {
  if (munmap(memory, bytes) != 0) {
    fprintf(stderr, "os_memory_free failed\n");
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

  printf("NULL: %p\n", NULL);

  /*printf("sizeof(size_t): %lu\n", sizeof(size_t));*/
  /*printf("sizeof(ssize_t): %lu\n", sizeof(ssize_t));*/
  //printf("sizeof(ptrdiff_t): %lu\n", sizeof(ptrdiff_t));

  printf("sizeof(u8): %lu\n", sizeof(u8));
  printf("HOST_OS_NAME: %s\n", HOST_OS_NAME);
  printf("memory_size: %llu\n", memory_size);
  //printf("SIZE_MAX, SSIZE_MAX: %llu, %ld\n", SIZE_MAX, SSIZE_MAX);
  return 0;
}
