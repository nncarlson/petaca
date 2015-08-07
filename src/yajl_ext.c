/* Fortran interoperable interfaces to the varargs function yajl_config */

#include "yajl/yajl_parse.h"

int yajl_set_option(yajl_handle h, yajl_option opt)
{
  return yajl_config(h, opt, 1);
}

int yajl_unset_option(yajl_handle h, yajl_option opt)
{
  return yajl_config(h, opt, 0);
}
