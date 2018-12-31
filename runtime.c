#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>

#define HEAP_SIZE 1024

extern long scheme_entry(); char *hptr;

char *root; // For potential shadow stack

void hptr_inc(long value)
{
    *((long *) hptr++) = value;
}

long hptr_ptr()
{
    return (long) hptr;
}

long hptr_car(long ptr)
{
    return 0;
}

long hptr_cdr(long ptr)
{
    return 0;
}

unsigned char *alloc_protected_space(int size)
{
    int page = getpagesize(); int aligned_size = ((size + page - 1) / page) * page;
    
    char* p = mmap(0, aligned_size + 2 * page, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);

    mprotect(p, page, PROT_NONE);

    mprotect(p + page + aligned_size, page, PROT_NONE);

    return (p + page);
}

void free_protected_space(unsigned char* p, int size)
{
    int page = getpagesize(); int aligned_size = ((size + page - 1) / page) * page;
    
    munmap(p - page, aligned_size + 2 * page);
}

void print_ptr(int retval)
{
    if      ((retval &  0x03) == 0   ) { printf("%li\n", retval >> 2);               }
    else if ((retval &  0xFF) == 0x0F) { printf("%c\\%c\n", '#', retval >> 8);      }
    else if ((retval &  0x7F) == 0x1F) { printf("%s\n", retval >> 7 ? "#t" : "#f"); }
    else if  (retval == 0x2F)          { printf("'()\n");                           }
    else if ((retval &  0x03) == 1   ) { printf("Pair at %p\n", retval);    }
    else                               { printf("Unknown 0x%x\n", retval);          }
}

int main()
{
    char * heap = hptr = alloc_protected_space(HEAP_SIZE); printf("Heap pointer at %p\n", heap);
    
    print_ptr(scheme_entry());

    free_protected_space(heap, HEAP_SIZE);
    
    return 0;
}


