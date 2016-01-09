#include "interface.h"
#include "game.h"
#include "base/dictionary.h"
#include <iostream>

#include "mysql/mysql.h"

int main ( int argc, char* argv[] )
{
    Interface* ui = new Interface();
    base::Dictionary* cfg = new base::Dictionary ( "config.cfg" );
    mysql* db = mysql::connect ( cfg->getString ("mysql_host"),
                                 cfg->getString ("mysql_username"),
                                 cfg->getString ("mysql_password"),
                                 cfg->getString ("mysql_database") );
    
    ui->setDatabase ( db );
    ui->setConfig ( cfg );
    
    ui->loadGames();    
    ui->mainloop();
    
    delete ui;
    return 0;
}
