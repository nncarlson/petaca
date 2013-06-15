#ifdef PP_HAS_CONCATENATE
#define CONCATENATE(a,b) a ## b
#else
#define IDENTITY(x) x
#define CONCATENATE(a,b) IDENTITY(a)IDENTITY(b)
#endif
