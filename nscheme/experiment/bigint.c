// gcc -arch x86_64 -std=c99 -o bigint bigint.c && ./bigint
#include <stdio.h>
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
typedef signed char s8;
typedef signed short s16;
typedef signed int s32;
typedef signed long long s64;
typedef unsigned __int128 u128;
typedef struct { u64 low, high; } u64pair;
static inline u64pair LLL_umul128(u64 a, u64 b) {
 u128 full = ((u128)a) * ((u128)b);
 u64pair p;
 p.low = full;
 p.high = full >> 64;
 return p;
}

void bigadd(u64 count_a, u64 *a, u64 count_b, u64 *b, u64 *out) {
  u64 carry_bit = 0, count_overlap, count_full, *x;
  if (count_a < count_b) {
    count_overlap = count_a;
    count_full = count_b;
    x = b;
  } else {
    count_overlap = count_b;
    count_full = count_a;
    x = a;
  }
  for (u64 i = 0; i < count_overlap; ++i) {
    out[i] = __builtin_addcll(a[i], b[i], carry_bit, &carry_bit);
  }
  for (u64 i = count_overlap; i < count_full; ++i) {
    out[i] = __builtin_addcll(x[i], carry_bit, 0, &carry_bit);
  }
  out[count_full] = carry_bit;
}

void muladd(u64 factor, u64 count, u64 *in, u64 *out) {
  u64 carry = 0, carry_bit = 0;
  u64pair mp;
  for (u64 i = 0; i < count; ++i) {
    mp = LLL_umul128(in[i], factor);
    out[i] = __builtin_addcll(carry, out[i], 0, &carry_bit);
    out[i] = __builtin_addcll(mp.low, out[i], 0, &carry);
    // largest possible intermediate product:
    // > (number->string (* #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF) 16)
    // "fffffffffffffffe0000000000000001"
    // split into two 64-bit parts:
    // fffffffffffffffe 0000000000000001
    // worst case previous carry:
    // ffffffffffffffff
    // worst case current out[k]:
    // ffffffffffffffff
    // This means that, in the worst case, adding the low part to out[k]
    // will produce 2^65, which is a carry bit followed by a 64-bit 0, and the
    // carry bit will cause the high part to max out without another carry bit:
    // ffffffffffffffff 0000000000000000
    // Adding the low 0 to the carry from the previous iteration will also
    // never produce a carry bit.  In the worst case, the previous carry was
    // 2^64-1, giving a final worst case result of 128-bit -1:
    // ffffffffffffffff ffffffffffffffff
    carry += mp.high + carry_bit;  // so, this can use + instead of addcll
  }
  out[count] = carry;
}

void bigmul(u64 count_a, u64 *a, u64 count_b, u64 *b, u64 *out) {
  for (u64 i = 0; i < count_a; ++i) {
    muladd(a[i], count_b, b, out + i);
  }
}

void testbigadd3(u64 *a, u64 *b, u64 *out) {
  for (u64 i = 0; i < 4; ++i) { out[i] = 0; }
  bigadd(3, a, 3, b, out);
  printf("                 ");
  for (u64 i = 3; i > 0; --i) {
    printf("%016llx ", a[i-1]);
  }
  printf("\n+                ");
  for (u64 i = 3; i > 0; --i) {
    printf("%016llx ", b[i-1]);
  }
  printf("\n===================================================================\n");
  for (u64 i = 4; i > 0; --i) {
    printf("%016llx ", out[i-1]);
  }
  printf("\n");
}

void testbigmul3x3(u64 *a, u64 *b, u64 *out) {
  for (u64 i = 0; i < 6; ++i) { out[i] = 0; }
  bigmul(3, a, 3, b, out);
  printf("                                                   ");
  for (u64 i = 3; i > 0; --i) {
    printf("%016llx ", a[i-1]);
  }
  printf("\n*                                                  ");
  for (u64 i = 3; i > 0; --i) {
    printf("%016llx ", b[i-1]);
  }
  printf("\n=====================================================================================================\n");
  for (u64 i = 6; i > 0; --i) {
    printf("%016llx ", out[i-1]);
  }
  printf("\n");
}

int main (int argc, char **argv) {
  u64 a[3], b[3], out[6];
  for (u64 i = 0; i < 3; ++i) {
    a[i] = i+1;
    b[i] = 2*(i+1);
  }
  testbigadd3(a, b, out);
  printf("\n");
  for (u64 i = 0; i < 3; ++i) {
    a[i] = -1;
    b[i] = -1;
  }
  testbigadd3(a, b, out);
  printf("\n");
  for (u64 i = 0; i < 3; ++i) {
    a[i] = i+1;
    b[i] = 2*(i+1);
  }
  a[1] = 0x8000000000000000;
  testbigmul3x3(a, b, out);
  printf("\n");
  for (u64 i = 0; i < 3; ++i) {
    a[i] = -1;
    b[i] = -1;
  }
  testbigmul3x3(a, b, out);
  return 0;
}
