#ifndef NAUMS_COLLECTION_INTERFACE
#define NAUMS_COLLECTION_INTERFACE

#include "game.h"
#include "systems.h"
#include "base/arraylist.h"
#include <iostream>

#include "mysql/store.h"
#include "base/dictionary.h"

/**
 * \brief a class representing the interface (planned: generic interface for all interfaces (web, gui, cli, ..)
 **/
class Interface
{
    private: 
        /// the maximal current ID (useful for adding a new game)
        int maxId;
        /// the database-connection for getting games and saving data
        dbstore* db;
        /// the configuration-data
        base::Dictionary* cfg;
        
    public:
        /// holds a list of the games in the collection
        base::ArrayList<Collection::Game*> collection;    
        
        /**
         * \brief initiates the object
         **/
        Interface();
        /**
         * \brief deletes the game-list
         **/
        ~Interface();
        
        /**
         * \brief the mainloop cares about redrawing and input-handling
         **/
        void mainloop ();
        
        /**
         * \brief sets the database-connection in the interface-object
         **/
        void setDatabase ( dbstore* d );
        /**
         * \brief sets the configuration-dictionary in the interface-object
         **/
        void setConfig ( base::Dictionary* c );
        
        /**
         * \brief prints or updates the game-list
         **/
        void printList ();
        
        /**
         * \brief prints helptext to the screen
         **/
        void printHelp();
        
        /**
         * \brief opens a form for adding a new game
         **/
        void addNewGame ();
        
        void removeGame ();
        void editGame();
        
        /**
         * \brief adds a game to the collection
         **/
        void addExistingGame ( Collection::Game* ng );
        
        /**
         * \brief loads the game from the database into the collection-list
         **/
        void loadGames();

        
        
    
};

#endif
