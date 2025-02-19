/* run.config*
 COMMENT: see below
 STDOPT:
*/

/* This test contains attributes that may appear in glibc, musl, case studies,
   etc. Most of these can be safely ignored by Frama-C (e.g. they provide
   hints to the compiler). Plug-ins such as E-ACSL may need to include such
   files and thus we want to avoid unnecessary warnings.
   This file should be periodically updated to follow libc/case study changes.
   Update hints:
   - copy function signatures directly, to more closely match what is found
     in the wild;
   - rename libc functions, to avoid conflicts with reserved identifiers;
   - include a reference to the source.
*/

// from glibc
extern int libc_remove (const char *__filename)
  __attribute__ ((__nothrow__ , __leaf__)) { return 0; }

// from glibc
extern int libc_fclose (int *__stream) __attribute__ ((__nonnull__ (1)))
  { return 0; }

// from glibc
extern int __sigsetjmp_cancel (void *__env[1], int __savemask)
  __asm__ ("" "__sigsetjmp") __attribute__ ((__nothrow__))
  __attribute__ ((__returns_twice__)) { return 0; }

// from gmp
extern int mpz_fits_sint_p (void *) __attribute__ ((__pure__)) { return 0; }

// from papabench (AVR-specific)
void signame (void) __attribute__ ((signal)) {}

// from curl
void
__attribute__((__warning__("curl_easy_setopt expects a long argument")))
__attribute__((__unused__))
__attribute__((__noinline__)) _curl_easy_setopt_err (void) {}

void body(void) {
  // from 8cc
  int save_hook __attribute__((unused, cleanup(pop_function)));
}
