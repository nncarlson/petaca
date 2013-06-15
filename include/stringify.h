#ifdef PP_HAS_STRINGIFY
#define TO_STRING(name) #name
#else
#define TO_STRING(name) "name"
#endif
