#ifndef NAUMS_GAME_DICTIONARY
#define NAUMS_GAME_DICTIONARY

#include "arraylist.h"
#include "debug.h"

namespace base
{
    /**
     * \class Dictionary class for holding (name,value)-pairs and easy access. 
     * parses a dictionary-file. NAME=VALUE\nNAME=VALUE...
     **/
    class Dictionary
    {
        private: 
            enum parserstate
            {
                NAME, VALUE
            };
        
            /// struct with name-value-pair
            struct dict_value
            {
                char* name;     ///< name of value
                char* value;    ///< value according to name
            };
            
            /// the (sorted) arraylist holding the values
            base::ArrayList<struct dict_value*>* values;
            
            /**
             * \brief describes a sorting-function for two config_value types. 
             * \note compares the .name-attribute of the struct
             * \param[in] v1 first struct
             * \param[in] v2 second struct
             * \return -1 if v1<v2, 0 if v1==v2, 1 if v1>v2
             **/
            static int DictSort ( struct dict_value* v1, struct dict_value* v2 )
            {
                char* n1 = v1->name;
                char* n2 = v2->name;
                
                while (*n1 && *n2)
                {
                    if (*n1 < *n2) return -1;
                    if (*n1 > *n2) return 1;
                    n1++; n2++;
                }
                
                if (*n1 < *n2) return -1;
                if (*n1 > *n2) return 1;
                return 0;
            }
        
        public: 
            /**
             * \brief reads the file and fills the values-array accordingly
             * \param[in] dictionaryfile the file to be read as data
             **/
            Dictionary ( const char* dictionaryfile );
            /// destructor, deletes all the rubbish
            ~Dictionary();
            
            /**
             * \brief sets a value (adds it if it does not exist)
             * \param[in] name the name of the value
             * \param[in] value the value
             **/
            void setString ( const char* name, const char* value );
            /**
             * \brief returns the string to an name
             * \param[in] name the name of the value
             **/
            char* getString ( char* name );
            /**
             * \brief returns the integer-value of a string to an name
             * \param[in] name the name of the value
             **/
            int getInt ( char* name );
    };
    
}

#endif
