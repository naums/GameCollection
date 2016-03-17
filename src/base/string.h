#ifndef NAUMS_STRING
#define NAUMS_STRING

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

namespace string
{
    inline void readline ( std::string& str )
    {
        char input[2]={' ', '\0'};
        do
        {
            *input = getchar();
        }
        while (!isgraph(*input));
        
        str += input;        
        do
        {
            *input = getchar();
            if (isgraph(*input))
            {
                str += input;
            }
        }
        while (*input!='\n');
    }
}

#endif
