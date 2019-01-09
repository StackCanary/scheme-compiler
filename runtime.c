#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>

#define HEAP_SIZE 1024

extern long scheme_entry(); long *hptr; long cptr;

char *root; // For potential shadow stack

long hptr_con(long a, long b)
{
    // printf("Wrote %ld at %p", value , hptr);

    long save_hptr = (long) hptr;
    
    *hptr = a; hptr++;
    *hptr = b; hptr++;

    return save_hptr | 1;
}

void hptr_inc(long a)
{
    *hptr = a; hptr++;
}

long hptr_ptr(long tag)
{
    return ((long) hptr) | tag;
}

long hptr_car(long ptr)
{
    long *p = (long *) (ptr & ~7);      return *p;
}

long hptr_cdr(long ptr)
{
    long *p = (long *) (ptr & ~7); p++; return *p;
}

long hptr_closure_len()
{
    return hptr_car(cptr);
}

long hptr_closure_lab()
{
    return hptr_cdr(cptr);
}

long hptr_get_freevar(long off, long *tmp)
{
    long *p = (long *) (cptr & ~3); *tmp = p[off + 2];
}

void hptr_set_clsptr(long ptr)
{
    cptr = ptr;
}

long hptr_get_clsptr()
{
    return cptr;
}


long hptr_vector_mak(long size, long value)
{
    long retval = hptr_ptr(5);

    hptr_inc(size);

    size = size / 4;

    for (int i = 0; i < size; i++)
	hptr_inc(value);

    return retval;
}

void hptr_vector_set(long vectr, long index, long  val)
{
    long *p = (long *) (vectr & ~7);

    if ( index / 4  < p[0] / 4)
	p[index / 4 + 1] = val;
    else
	printf("Index out of bounds in vector-set\n");
    
}

void hptr_vector_ref(long vectr, long index, long *tmp)
{
    long *p = (long *) (vectr & ~7);

    if ( index / 4  < p[0] / 4)
	*tmp = p[index / 4 + 1];
    else
	printf("Index out of bounds in vector-ref\n");
    
}



char *alloc_protected_space(int size)
{
    int page = getpagesize(); int aligned_size = ((size + page - 1) / page) * page;
    
    char* p = mmap(0, aligned_size + 2 * page, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);

    mprotect(p, page, PROT_NONE);

    mprotect(p + page + aligned_size, page, PROT_NONE);

    return (p + page);
}

void free_protected_space(char* p, int size)
{
    int page = getpagesize(); int aligned_size = ((size + page - 1) / page) * page;
    
    munmap(p - page, aligned_size + 2 * page);
}



void print_ptr(int retval)
{
    if      ((retval &  0x03) == 0   ) { printf("%li\n", retval >> 2);              }
    else if ((retval &  0xFF) == 0x0F) { printf("%c\\%c\n", '#', retval >> 8);      }
    else if ((retval &  0x7F) == 0x1F) { printf("%s\n", retval >> 7 ? "#t" : "#f"); }
    else if  (retval == 0x2F)          { printf("'()\n");                           }
    else if ((retval &  0x07) == 1   ) { printf("Pair at %p\n",    retval);         }
    else if ((retval &  0x07) == 2   ) { printf("Closure at %p\n", retval);         }
    else if ((retval &  0x07) == 5   ) { printf("Vector at %p\n",  retval);         }
    else                               { printf("Unknown 0x%x\n",  retval);         }
}

int main()
{
    char * heap = alloc_protected_space(HEAP_SIZE);

    hptr = (long *) heap;

    print_ptr(scheme_entry());

    free_protected_space(heap, HEAP_SIZE);
    
    return 0;
}


