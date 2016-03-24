#ifndef NAUMS_CONSOLE
#define NAUMS_CONSOLE

#define ESCAPE 0x1B

class Console
{
    private:
        enum color
        {
            BLACK=0,
            RED=1,
            GREEN=2,
            YELLOW=3,
            BLUE=4,
            MAGENTA=5,
            CYAN=6,
            WHITE=7,
            NOTUSED=8,
            DEFAULT=9
        };
    
        int posX, posY;
        enum color foreColor, backColor;
        bool foreBright;
        
        int sizeX, sizeY;
        
        void _setColor ();
        void _setPosition ();
        void _clear();
    
    public:
        Console ();
        ~Console();
        
        void setBright ( bool fb );
        void setForeColor( enum color fc );
        void setBackColor ( enum color bc );
        int getForeColor ();
        int getBackColor ();
        void home ();
        void clear ();
        void setPosition ( int x, int y );
        int getX();
        int getY();
        
        int printf ( const char* format, ... );
};  

#endif
