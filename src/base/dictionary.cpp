#include "dictionary.h"
#include "file.h"

#include <cstring>
#include <cstdlib>

namespace base
{
    Dictionary::Dictionary ( const char* dictionaryfile )
    {
        enum parserstate state;
    
        values = new base::ArrayList<struct dict_value*>();
        values->setSortingFunc ( Dictionary::DictSort );
        
        FILE* f = fopen ( dictionaryfile, "r");
        if (!f)
        {
            debug::log ( "Keine Konfigurationsdatei gefunden");
            return;
        }
        
        int fs = base::file::filesize ( f );
        char* ptr = (char*) malloc ( fs + 1 );
        fread(ptr, fs, 1, f );
        fclose (f);
        ptr[fs]='\0';
        char* begin, *vbegin;
        state = NAME;
        
        while (!((*ptr>='A' && *ptr<='Z') || ((*ptr>='a' && *ptr<='z'))) && *ptr)
            ptr++;
        begin=ptr;
        while (*ptr != '\0')
        {
            if (*ptr == '=') 
            {
                if (state == NAME)
                {
                    *ptr='\0';
                    ptr++;
                    while (!((*ptr>='A' && *ptr<='Z') || ((*ptr>='a' && *ptr<='z'))) && *ptr)
                        ptr++;
                    vbegin = ptr;
                    state = VALUE;
                }
                else
                {
                    debug::log ( "Mit der Konfigdatei stimmt etwas nicht");
                    return;
                }
            }
            else if (*ptr == '\n' || *ptr == '\r' || *ptr == ';')
            {
                if (state == NAME)
                {
                    debug::log ( "Mit der Konfigdatei stimmt etwas nicht");
                    return;

                }
                else
                {
                    *ptr='\0';
                    struct dict_value* n = new struct dict_value;
                    char* name = (char*) malloc ( strlen(begin) + 1 );
                    char* value = (char*) malloc ( strlen(vbegin) + 1 );
                    strcpy ( name, begin );   name[strlen(begin)]='\0';
                    strcpy ( value, vbegin);  value[strlen(vbegin)]='\0';
                    n->name = name;
                    n->value = value;
                    values->insert ( n );
                    
                    vbegin = NULL;   
                    ptr++;                 
                    while (!((*ptr>='A' && *ptr<='Z') || ((*ptr>='a' && *ptr<='z'))) && *ptr)
                        ptr++;
                    begin = ptr;
                    state = NAME;
                }
            }
            ptr++;
        }
    }
    
    Dictionary::~Dictionary ()
    {
        int i = values->getSize() -1;
        for (; i>=0; i++)
        {
            struct dict_value* n = values->getValue( i );
            values->remove ( i );
            free (n->name);
            free (n->value);
            free (n);
        }
        delete values;
    }
    
    void Dictionary::setString ( const char* name, const char* value )
    {
        int lenname = strlen(name);
        int lenvalue = strlen(value);
        char* val = (char*) malloc ( lenvalue + 1 );
        strncpy ( val, value, lenvalue );
        val[lenvalue]='\0';
        
        struct dict_value* m = new struct dict_value;
        
        char* nm = (char*) malloc ( strlen( name ) +1);
        strncpy ( nm, name, lenname );
        nm[strlen(name)] = '\0';
        
        m->name = nm;
        m->value = val;
        
        
        struct dict_value* n = values->findSorted ( m );
        if (n->name == NULL || n->value == NULL)
        {
            values->insert ( m );
        }
        else
        {
            free ( n->value );
            free (nm);
            n->value = val;
            delete m;
        }
    }
    
    char* Dictionary::getString ( char* name )
    {
        struct dict_value req;
        req.name = name;
        req.value = NULL;
        struct dict_value* n = values->findSorted ( &req );
        if (n == NULL) return NULL;
        return n->value;
    }

    int Dictionary::getInt ( char* name )
    {
        return atoi ( this->getString ( name ) );
    }
}
