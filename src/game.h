#ifndef NAUMS_GAME
#define NAUMS_GAME

#include <string>
#include "base/arraylist.h"
#include "systems.h"
#include "base/date.h"

namespace Collection
{
    /**
     * \brief represents a game in your collection
     **/
    class Game
    {
        public:
            /// ID of the game in the database
            int id;
            /// the title of the game
            std::string name;
            /// the publisher of the game
            std::string publisher;
            /// the developing studio of the game
            std::string developer;
            /// some keywords describing the game (may be used for a search-function later on)
            std::string keywords;
            /// the releasedate of the game
            base::date releasedate;

        
            /**
             * \brief constructor, will set the necessary values
             * \param[in] name the title of the game
             * \param[in] publisher the publisher of the game
             * \param[in] developer the developer of the game
             * \param[in] releasedate the date of release of the game
             **/
            Game ( int id, std::string name, std::string publisher, std::string developer, time_t releasedate );
            
            /**
             * \brief retrieves the timestamp from a date (dd.mm.YYYY)
             * \param[in] date the date-string
             * \return its unix-timestamp aquivalent
             **/
            static time_t timeFromDate ( std::string date );
    };
}

#endif