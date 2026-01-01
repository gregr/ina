// gcc -arch x86_64 -std=c99 -o bigint bigint.c && ./bigint && rm bigint
// gcc -arch x86_64 -std=c99 -o bigint bigint.c bigadd.s && ./bigint && rm bigint
// gcc -arch x86_64 -std=c99 -O2 -S bigint.c && cat bigint.s && rm bigint.s
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

// Toggle commenting if linking with bigint.s instead.
extern void bigadd(u64 count_a, u64 *a, u64 count_b, u64 *b, u64 *out);
void bigadd(u64 count_a, u64 *a, u64 count_b, u64 *b, u64 *out) {
  u64 carry_bit = 0;
  if (count_a < count_b) {
    u64 tmp = count_b; count_b = count_a; count_a = tmp;
    u64 *ptmp = b; b = a; a = ptmp;
  }
  for (u64 i = 0; i < count_b; ++i) {
    out[i] = __builtin_addcll(a[i], b[i], carry_bit, &carry_bit);
  }
  for (u64 i = count_b; i < count_a; ++i) {
    out[i] = __builtin_addcll(a[i], carry_bit, 0, &carry_bit);
  }
  out[count_a] = carry_bit;
}

// Generated via LLL:
// void bigadd(u64 count_a, u64 *a, u64 count_b, u64 *b, u64 *out) {
//  u64 rdi=count_a, rsi=(u64)a, rdx=count_b, rcx=(u64)b, r8=(u64)out,
//      LLL_flag_carry=0, rax, r9;
//  if (rdi >= rdx) goto L_loop1_init;
//  r9 = rdi;
//  rdi = rdx;
//  rdx = r9;
//  r9 = rsi;
//  rsi = rcx;
//  rcx = r9;
// L_loop1_init:
//  rax = 0ull;
//  if (rdx == 0ull) goto L_loop2_init;
// L_loop1:
//  r9 = *(u64*)(rsi + (rax<<3));
//  r9 = __builtin_addcll(r9,*(u64*)(rcx + (rax<<3)),LLL_flag_carry,&LLL_flag_carry);;
//  *(u64*)(r8 + (rax<<3)) = r9;
//  rax = rax + 1ull;
//  rdx = rdx - 1ull;
//  if (rdx != 0ull) goto L_loop1;
// L_loop2_init:
//  rdx = LLL_flag_carry;
//  rdi = rdi - rax;
//  if (rdi == 0ull) goto L_done;
//  LLL_flag_carry = __builtin_uaddll_overflow(rdx,-1ull,&rdx);
// L_loop2:
//  r9 = *(u64*)(rsi + (rax<<3));
//  r9 = __builtin_addcll(r9,0ull,LLL_flag_carry,&LLL_flag_carry);;
//  *(u64*)(r8 + (rax<<3)) = r9;
//  rax = rax + 1ull;
//  rdi = rdi - 1ull;
//  if (rdi != 0ull) goto L_loop2;
//  rdx = LLL_flag_carry;
// L_done:
//  *(u64*)(r8 + (rax<<3)) = rdx;
// }

// This version counts k down to 0 in a failed attempt to convince Clang and
// gcc to generate a better loop in x86-64.  It's possible for the main loop
// to have this structure:
//   `mov adc mov inc dec jnz`
// since `inc` and `dec` do not change the carry flag produced by `adc` that
// must be used by the next iteration.  Counting down to 0 using dec is the
// trick that lets us use `jnz` without a `cmp`.  A `cmp` would ruin the carry
// flag needed by the next iteration.  Unfortunately, even for this version,
// Clang and gcc track the carry in a register rather than using the carry flag
// directly, giving a significantly worse loop structure of:
//   `mov add setc add setc or movzbl mov inc cmp jne`
//void bigadd(u64 count_a, u64 *a, u64 count_b, u64 *b, u64 *out) {
//  u64 carry_bit = 0;
//  if (count_a < count_b) {
//    u64 tmp = count_b; count_b = count_a; count_a = tmp;
//    u64 *ptmp = b; b = a; a = ptmp;
//  }
//  for (u64 i = 0, k = count_b; k > 0; ++i, --k) {
//    out[i] = __builtin_addcll(a[i], b[i], carry_bit, &carry_bit);
//  }
//  for (u64 i = count_b, k = count_a - count_b; k > 0; ++i, --k) {
//    out[i] = __builtin_addcll(a[i], carry_bit, 0, &carry_bit);
//  }
//  out[count_a] = carry_bit;
//}

// out = a - b
u64 bigsub(u64 count_a, u64 *a, u64 count_b, u64 *b, u64 *out) {
  u64 carry_bit = 0, count_overlap = (count_a < count_b) ? count_a : count_b;
  for (u64 i = 0; i < count_overlap; ++i) {
    out[i] = __builtin_subcll(a[i], b[i], carry_bit, &carry_bit);
  }
  if (count_a < count_b) {
    for (u64 i = count_a; i < count_b; ++i) {
      out[i] = __builtin_subcll(0, b[i], carry_bit, &carry_bit);
    }
  } else {
    for (u64 i = count_b; i < count_a; ++i) {
      out[i] = __builtin_subcll(a[i], carry_bit, 0, &carry_bit);
    }
  }
  return carry_bit;
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

void testbigsub3(u64 *a, u64 *b, u64 *out) {
  for (u64 i = 0; i < 4; ++i) { out[i] = 0; }
  out[3] = -(bigsub(3, a, 3, b, out));
  printf("                 ");
  for (u64 i = 3; i > 0; --i) {
    printf("%016llx ", a[i-1]);
  }
  printf("\n-                ");
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
  testbigsub3(a, b, out);
  printf("\n");
  for (u64 i = 0; i < 3; ++i) {
    a[i] = -1;
    b[i] = -1;
  }
  testbigsub3(a, b, out);
  printf("\n");
  for (u64 i = 0; i < 3; ++i) {
    a[i] = 0;
    b[i] = -1;
  }
  testbigsub3(a, b, out);
  printf("\n");
  for (u64 i = 0; i < 3; ++i) {
    a[i] = 0;
    b[i] = 0;
  }
  b[0] = 1;
  testbigsub3(a, b, out);
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
  printf("\n");
  for (u64 i = 0; i < 3; ++i) {
    a[i] = 0;
    b[i] = 0;
  }
  a[0] = 0xFFFFFFFFFFFFFFFF;
  b[0] = 0xFFFFFFFFFFFFFFFF;
  testbigmul3x3(a, b, out);
  printf("\n");
  for (u64 i = 0; i < 3; ++i) {
    a[i] = 0;
    b[i] = 0;
  }
  a[0] = 0x7FFFFFFFFFFFFFFF;
  b[0] = 0x7FFFFFFFFFFFFFFF;
  testbigmul3x3(a, b, out);
  return 0;
}
