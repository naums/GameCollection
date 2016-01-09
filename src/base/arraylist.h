#ifndef NAUMS_BASE_ARRAYLIST
#define NAUMS_BASE_ARRAYLIST

#include <cstdio>
#include <cstdlib>

namespace base{

/**
 * \class ArrayList a list, build upon the array as data-structure, that is dynamicly realloc'd
 **/
template <typename t1>
class ArrayList
{
    private:
        /// the actuall array containing the payload-data
        t1* payload;
        /// the current size of the currently malloc'd payload-memory space
        int size;
        /// pointer to the element AFTER the last saved element in payload
        int end;
        /// the size in Bytes of the element-type (int => 4, bool => 1,...)
        int elemSize;
        /// the size, the array will be made bigger, on next realloc
        int offsetSize;
        
        /**
         * \brief function for sorting (comparing) two elements within the list
         * \note used for insertion-sort
         * \param[in] $1 the first element for comparing
         * \param[in] $2 the second element for comparing
         * \return -1 if $1<$2, 1 if $1>$2 and 0 if $1==$2
         **/
        int (* sortFunc) (t1, t1);
        
        /** 
         * \brief reallocs the payload-Array
         * \param[in] nextSize the size of the array AFTER realloc
         **/
        void realloc ( int nextSize )
        {
            payload = (t1*) ::realloc ( payload, nextSize * this->elemSize );
            this->size=nextSize;
        }
    
    public:
        /**
         * \brief constructor, mallocs memory,...
         * \param[in] initialSize the initial size of the payload-array
         * \param[in] eSize the size of the element type
         * \param[in] offsetSize number of elements the array will be made bigger on every realloc
         * \warn if no eSize is given, sizeof(t1) will be used
         **/
        ArrayList( int initialSize = 10, int eSize=-1, int offsetSize  = 10 )
        {
            this->sortFunc = NULL;
            this->size = initialSize;
            this->offsetSize = offsetSize;
        
            if (eSize<=0)
            {
                payload = (t1*) malloc ( initialSize * sizeof(t1) );
                this->elemSize = sizeof(t1);
            }
            else
            {
                payload = (t1*) malloc ( initialSize * eSize );
                this->elemSize = eSize;
            }
                
            this->end = 0;
        }
        
        /**
         * \brief free's the malloc'd payload-memory space
         **/
        ~ArrayList ()
        {
            free (this->payload);
        }
        
        /**
         * \brief set the new sorting-function for this exact data-structure
         * \warn only AFTER setting this function, there will be sorting done on INSERT! (no sorting in add!)
         * \note myfunc must return -1 if a < b , 0 if a == b and 1 if a > b
         **/
        void setSortingFunc ( int (*myfunc) (t1, t1) )
        {
            this->sortFunc = myfunc;
        }
        
        /**
         * \brief does insertion-sort with the element.
         * \param[in] elem the element you want to sorted into the list
         * \warn make sure sortFunc is set! Make sure the list is completely sorted from lowest value to highest!
         **/
        void insert ( t1 elem )
        {
            if (this->end == 0)
                return this->doInsert(elem, 0);
            if ((this->getSize()+1) >= this->getInternalSize())
            {
                this->realloc ( this->size + this->offsetSize );
            }
            int i=0;
            for (; i<this->end; i++)
            {
                if (this->sortFunc (this->payload[i], elem) != -1)
                {
                    this->doInsert ( elem, i );
                    return;
                }
            }
            
            this->doInsert ( elem, i );
            return;
        }
        
        /**
         * \brief does the insert in a specific position
         **/
        void doInsert ( t1 elem, int pos )
        {
            int end = this->end;
            for (; end > pos; end--)
            {
                this->payload[end] = this->payload[(end-1)];
            }
            this->payload[pos] = elem;
            this->end++;
        }
        
        /**
         * \brief adds an element to the list
         * \param[in] elem the value of the new element
         * \note will realloc dynamicly if needed
         **/        
        void add ( t1 elem )
        {
            if ((this->getSize()+1) >= this->getInternalSize())
            {
                this->realloc ( this->size + this->offsetSize );
            }
            this->payload[this->end] = elem;
            this->end++;
        }
        
        /**
         * \brief finds an element within a sorted list! 
         * \param[in] elem the element you may want to find. 
         * \note does sequential search for now, not got binary search to work :/
         **/
        t1 findSorted ( t1 elem )
        {
            for (int i=0; i<this->end; i++)
            {
                if (this->sortFunc(this->payload[i], elem) == 0)
                    return this->payload[i];
            }
            
            t1 n;
            char* bl = (char*)&n;
            for (unsigned i=0; i<sizeof(t1); i++)
            {
                *bl = 0; bl++;
            }
            return n;
        }
                
        /**
         * \brief removes the element at the given index from the list
         * \param[in] index the index of the element to be removed
         **/
        void remove ( int index )
        {
            if (index>=end)
                return;
            while ( index<this->end )
            {
                payload[index] = payload[index+1];
                index++;
            }
            this->end --;
        }
        
        /**
         * \brief retrieves the index of the element with the given value
         * \param[in] value the value wanted
         * \return the index
         **/
        int index ( t1 value )
        {
            t1* ptr = this->payload;
            for (int i=0; i < this->end;i++)
            {
                if (*ptr == value)
                {
                    return i;
                }
                ptr++;
            }
            return -1;
        }
        
        /**
         * \brief retrieves the value of the element at the given index
         * \param[in] index the index wanted
         * \return the value of the element
         **/
        t1 getValue ( int index )
        {
            return this->payload[index];
        }
    
        /**
         * \brief returns the size of the malloc'd memory space
         **/
        int getInternalSize ()
        {
            return this->size;
        }
        
        /**
         * \brief return the number of elements in the array
         **/
        int getSize()
        {
            return this->end;
        }
};

}

#endif
