#ifndef mclib_stdarg_h__17_12_07____16_47_53
#define mclib_stdarg_h__17_12_07____16_47_53
#include <mclib.h>

#include <stdlib.h>

/**
 * FIXME: Requires co-operation with the compiler to implement.
 * TODO: Implement Variadic Function Complete Implementation.
 */

typedef char* va_list;

static void __va_start_impl(va_list* list,void* from)
{
*list = from;
}



static void* __va_arg_impl(va_list* list,int size){
	void* t = *list;
	*list +=size;
	return t;
}

static void __va_end_impl(va_list* list){
	
}




#define va_start(list,from) __va_start_impl(&list,(&from)+1)

#define va_arg(list,type) *(type*)(__va_arg_impl(&list,sizeof(type)))

#define va_end(list) __va_end_impl(&list)

#endif
