#include "store.h"
#include "../base/debug.h"

#include <cstdlib>

dbstore* dbstore::connect ( const char* srv, const char* usr, const char* pass, const char* dbname )
{
    dbstore* tmp = new dbstore();
    tmp->db = mysql::connect ( srv, usr, pass, dbname );
    if (tmp->db)
        return tmp;
    else
    {
        debug::log ("No DB Connection, %s, %s, (pass ;)), %s", srv, usr, dbname );
        return NULL;
    }
}

mysqlpp::StoreQueryResult dbstore::query ( const char* q )
{
    return this->db->query( q );
}

void dbstore::storeGame ( Collection::Game* game )
{                
    mysqlpp::Query q (db->getConnection());
    q << "UPDATE game "
      << "SET title='" << game->name << "', publisher='" << game->publisher << "', developer='" << game->developer 
            << "', releasedate=" << game->releasedate.getTimestamp()
      << "WHERE id=" << game->id;
    db->put ( q );
}

void dbstore::addGame ( Collection::Game* game )
{
    mysqlpp::Query q (db->getConnection());
    q << "INSERT INTO game (id, title, publisher, developer, releasedate, cover, system, keywords)"
      << "VALUES (" << game->id << ",'" << game->name << "','" << game->publisher << "','" 
                    << game->developer << "'," << game->releasedate.getTimestamp() << ", '', '', '')";
    db->put ( q );
}

void dbstore::deleteGame ( Collection::Game* game )
{
    mysqlpp::Query q (db->getConnection());
    q << "DELETE FROM game WHERE id=" << game->id;
    db->put ( q );
}

mysql* dbstore::getMySQL()
{
    return this->db;
}
