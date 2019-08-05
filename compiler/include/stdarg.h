#ifndef mclib_stdarg_h__17_12_07____16_47_53
#define mclib_stdarg_h__17_12_07____16_47_53
#include <mclib.h>

#include <stdlib.h>

/**
 * FIXME: Requires co-operation with the compiler to implement.
 * TODO: Implement Variadic Function Complete Implementation.
 */

typedef char* va_list;

void va_start_impl(va_list* list,void* from)
{
*list = from;
}



void* va_arg_impl(va_list* list,int size){
	void* t = *list;
	*list +=size;
	return t;
}

void va_end_impl(va_list* list){
	free(list);
  free(*list);
}




#define va_start(list,from) va_start_impl(&list,(&from)+1)

#define va_arg(list,type) *(type*)(va_arg_impl(&list,sizeof(type)))

#define va_end(list) va_end_impl(&list)

#endif
