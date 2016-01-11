#ifndef NAUMS_MYSQL_TRACE_STORE
#define NAUMS_MYSQL_TRACE_STORE

#include "mysql.h"
#include "../game.h"

/**
 * \brief wrapper to the mysql-object;
 **/
class dbstore
{
    private:
        /// mysql-database object
        mysql* db;
    
    public:
        /**
         * \brief connects to the database server and creates the internal mysql-datastructure
         * \param[in] c the config-object, which contains database-names and accountinfo and stuff
         * \return a valid object for using the MySQL-Database
         **/
        static dbstore* connect ( const char* srv, const char* usr, const char* pass, const char* dbname );
        
        /**
         * \brief executes an arbitrary query and returns the results
         * \param[in] q the SQL-Query
         * \return results
         **/
        mysqlpp::StoreQueryResult query ( const char* q );
        
        void storeGame ( Collection::Game* game );
        void addGame ( Collection::Game* game );
        void deleteGame ( Collection::Game* game );
        
        mysql* getMySQL();
};

#endif
