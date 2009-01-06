#define ASSERT(e) if (not (e)) then (assertPanic __FILE__ __LINE__) else
#define ASSERT2(e,msg) if False && (const False (e,msg)) then pprPanic "ASSERT2" (msg) else
#if !defined(GHCI)
#define GHCI
#endif

#define GLOBAL_VAR(name,value,ty)  \
{-# NOINLINE name #-};             \
name :: IORef (ty);                \
name = Util.global (value);


#include <ghc_boot_platform.h>

