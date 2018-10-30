#include <stdio.h>

extern int scheme_entry();

int main()
{
  int retval = scheme_entry();

  if      ((retval &  0x03) == 0   ) { printf("%d\n", retval >> 2);               }
  else if ((retval &  0xFF) == 0x0F) { printf("%c\\%c\n", '#', retval >> 8);               }
  else if ((retval &  0x7F) == 0x1F) { printf("%s\n", retval >> 7 ? "#t" : "#f"); }
  else if  (retval == 0x2F)          { printf("'()\n");                           }
  else                               { printf("Unknown 0x%x\n", retval);          }
  
}
