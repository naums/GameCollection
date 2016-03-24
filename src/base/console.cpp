#include "console.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

Console::Console ()
{
    this->foreColor = WHITE;
    this->backColor = BLACK;
    this->foreBright = false;
    
    this->setPosition (0,0);
    this->posX = 0;
    this->posY = 0;
    
    this->clear();
}

Console::~Console()
{
    
}

void Console::_setColor ()
{
    ::printf ("%c[%d,%dm", ESCAPE, (30+this->foreColor), (this->foreBright ? 2 : 21));
    ::printf ("%c[%dm", ESCAPE, (40+this->foreColor));
}

void Console::_setPosition ()
{
    ::printf("%c[%d;%dH", ESCAPE, this->posY, this->posX);
}

void Console::_clear ()
{
    ::printf ("%c[2J", ESCAPE);
}

void Console::clear ()
{
    this->_clear();
}

void Console::setForeColor ( enum color fc )
{
    this->foreColor = fc;
    this->_setColor();
}

void Console::setBackColor ( enum color bc )
{
    this->backColor = bc;
    this->_setColor();
}

void Console::home()
{
    this->posX = 0;
    this->posY = 0;
    this->_setPosition();
}

void Console::setPosition ( int x, int y )
{
    this->posX = x;
    this->posY = y;
    this->_setPosition();
}

int Console::getX()
{
    return this->posX;
}

int Console::getY()
{
    return this->posY;
}

int Console::getForeColor ()
{
    return this->foreColor;
}

int Console::getBackColor ()
{
    return this->backColor;
}

void Console::setBright ( bool fb )
{
    this->foreBright = fb;
}

int Console::printf ( const char* format, ... )
{
    this->_setPosition();

    va_list list;
    va_start(list, format);
    
    char* buffer=(char*) malloc(1024*sizeof(char));
    char* b = buffer;
    int result=vsprintf (buffer, format, list);
       
    while (*b)
    {
        if (*b=='\n')
        {
            this->posX=-1;
            this->posY++;
        } 
        putchar(*b);
        this->posX++;
        b++;
    }
    
    free(buffer);
    va_end(list);
    return result;
}


