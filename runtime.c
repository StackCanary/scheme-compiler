#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>

#define HEAP_SIZE (1024 / 2)
#define STCK_SIZE 1024 * 1024

#define HEAP_SIZE_LONG (HEAP_SIZE / sizeof(long))

#define GC_METRIC

#define is_fnum(retval) ((retval &  0x03) == 0   )
#define is_char(retval) ((retval &  0xFF) == 0x0F)
#define is_bool(retval) ((retval &  0x7F) == 0x1F)
#define is_null(retval)  (retval == 0x2F)
#define is_pair(retval) ((retval &  0x07) == 1   )
#define is_clsr(retval) ((retval &  0x07) == 2   )
#define is_vect(retval) ((retval &  0x07) == 5   )
#define is_strn(retval) ((retval &  0x07) == 6   )

#define is_value(retval) (is_fnum(retval) || is_char(retval) || is_bool(retval) || is_null(retval))
#define is_point(retval) (is_pair(retval) || is_clsr(retval) || is_vect(retval) || is_strn(retval)) 

extern long scheme_entry();


long *base; // Pointer to Base of Heap
long *hptr; // Pointer to Top of Heap
long  cptr; // Pointer to Current Closure Context
long *sptr; // Pointer to Top of Shadow Stack
long *root; // Pointer to Base of Shadow Stack


int marked_vect = 0;
int marked_clsr = 0;
int marked_pair = 0;
int mem_collect = 0;

int gc_triggered = 0;


void gcsweep();
void gc_mark(long *p);
void gc();
void print_ptr(long ret);


void gc_stack_psh(long val)
{
    *sptr++ = val; 
}

void gc_stack_pop(long cnt)
{
    while(cnt --> 0)
	sptr--;
}

void show_memory()
{
    long *p = base;
    
    while(p != hptr)
    {
	printf("%p:%p\n", p, (long *) *p); p++;
    }
}


void gc()
{
    gc_triggered = 1;
    
    long * p = root;
  
    while(p != sptr)
    {
	gc_mark(p); p++;
    }

    gcsweep();

    
}

#define mark 0x8000000000000000
#define mask 0x8000000000000000

void gc_mark(long *p)
{
    long at_p = *p;

    if (is_point(at_p))
    {

	*p |= mark; // Mark Value at p


	long tag_stripped = at_p & ~7;
	long * tag_stripped_p = (long*) tag_stripped;
		
	// Call gc_mark on children 
	if (is_pair(at_p))
	{
	    marked_pair++;
	    
	    gc_mark(tag_stripped_p);
	    gc_mark(tag_stripped_p + 1);
	}
	
	if(is_clsr(at_p))
	{
	    marked_clsr++;
	    
	    long len = *tag_stripped_p / 4;
	    long fcn = *tag_stripped_p + 1;

	    for (int i = 0; i < len; i++)
	    {
		gc_mark(tag_stripped_p + i + 2);
	    }
	    
	}
	   
	if(is_vect(at_p))
	{
	    marked_vect++;
	    
	    long len = *tag_stripped_p / 4;

	    for (int i = 0; i < len; i++)
	    {
		gc_mark(tag_stripped_p + i + 1);
	    }
	    
	}

	// A string contains no pointers
    }	
}


// Collect and Untag 
void gcsweep()
{
    long *p = base;
  
    while(p != hptr)
    {
	if ((*p & mask) != mark)
	{
	    mem_collect++;
	}

	*p = *p & ~mask; // Strip mark

	p++;
	
    }
}

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

    /* if (hptr == base + HEAP_SIZE_LONG) */
    /* { */
    /* 	gc(); */
    /* } */
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

long hptr_string_mak(long size, long value)
{
    long retval = hptr_ptr(6);

    hptr_inc(size);

    size = size / 4;

    for (int i = 0; i < size; i++)
	hptr_inc(value);

    return retval;
}

long hptr_vector_len(long vectr)
{
    long *p = (long *) (vectr & ~7); return p[0] / 4;
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


void show_pair(long ptr)
{
    
    printf("(");

    while(1)
    {
    
	long car = hptr_car(ptr);
	long cdr = hptr_cdr(ptr);

	print_ptr(car); printf(" ");

	if (is_value(cdr))
	{
	    if (!is_null(cdr))
	    {
		printf(". "); print_ptr(cdr);
	    }
	
	    break;
	}

	if (is_pair(cdr))
	{
	    ptr = cdr;
	}

    }

    printf(")");
    
}

void show_vect(long ptr)
{
    long len = hptr_vector_len(ptr);
    long tmp = 0;

    printf("#(");

    for (int i = 0; i < len; i++)
    {
	hptr_vector_ref(ptr, i * 4, &tmp); print_ptr(tmp);

	if (i < len - 1)
	    printf(" ");
    }

    printf(")");
}

void show_strn(long ptr)
{
    long len = hptr_vector_len(ptr);
    long tmp = 0;

    printf("\"");

    for (int i = 0; i < len; i++)
    {
	hptr_vector_ref(ptr, i, &tmp); printf("%c", tmp >> 8);
    }

    printf("\"");
}

void print_ptr(long retval)
{
    if      is_fnum(retval) { printf("%li", retval >> 2);              }
    else if is_char(retval) { printf("%c\\%c", '#', retval >> 8);      }
    else if is_bool(retval) { printf("%s", retval >> 7 ? "#t" : "#f"); }
    else if is_null(retval) { printf("'()");                           }
    else if is_pair(retval) { show_pair(retval);                       }
    else if is_clsr(retval) { printf("Procedure at %p", retval);         }
    else if is_vect(retval) { show_vect(retval);                       }
    else if is_strn(retval) { show_strn(retval);                       }
    else                    { printf("Unrecognised Value 0x%x",  retval);         }
}

int main()
{
    char * heap = alloc_protected_space(HEAP_SIZE);
    char * stck = alloc_protected_space(STCK_SIZE);

    base = (long *) heap;
    hptr = (long *) heap;
    sptr = (long *) stck;
    root = (long *) stck;

    print_ptr(scheme_entry()); printf("\n");

    if (gc_triggered)
    {
	printf("Marked Vect %d\n", marked_vect);
	printf("Marked Clsr %d\n", marked_clsr);
	printf("Marked Pair %d\n", marked_pair);
	printf("Collected %d memory locations\n", mem_collect);
    }

    free_protected_space(heap, HEAP_SIZE);
    free_protected_space(stck, STCK_SIZE);
    
    return 0;
}
