#include "interface.h"
#include "base/date.h"
#include "base/date_parser.h"

Interface::Interface () 
{
    this->maxId = 0;
    this->db = 0;
    this->cfg = 0;
}
Interface::~Interface()
{
    //this->collection.clean();
}

void Interface::setDatabase ( mysql* d )
{
    this->db = d;
}

void Interface::setConfig ( base::Dictionary* c )
{
    this->cfg = c;
}

void Interface::loadGames ()
{
    mysqlpp::StoreQueryResult res = db->query ( "SELECT * FROM game ORDER BY title ASC" );
    for (size_t i = 0; i<res.num_rows(); i++)
    {
        int id = res[i]["id"];
        
        std::string dev = res[i]["developer"].c_str();
        std::string pub = res[i]["publisher"].c_str();
        time_t date = res[i]["releasedate"];
        std::string title = res[i]["title"].c_str();
        
        Collection::Game *ng = new Collection::Game ( id, title, dev, pub, date );
        this->addExistingGame ( ng );
    }
}

void Interface::mainloop ()
{
    char input = 'l';

    while ( input != 'x' )
    {
        switch (input)
        {
            case 'l':
                this->printList ();
                break;
            case 'a':
                this->addNewGame ();
                break;
            default:
                break;
        }

        input = getchar ();
    }
}

void Interface::printList ()
{
    int i = 0;
    for (i=0; i<collection.getSize(); i++)
    {
        Collection::Game* current = collection.getValue ( i );
        printf ("{%d} %10s (%5s, %5s) [%10s]\n", current->id, current->name.c_str(), current->developer.c_str(), current->publisher.c_str(), current->releasedate.toString());
    }
    if (i==0)
    {
        std::cout << "Aktuell keine Spiele vorhanden" << std::endl;
    }
}

void Interface::addNewGame()
{
    std::string name, developer, publisher, date;
    std::cout << "Name: "; 
    std::cin >> name;
    std::cout << "Developer: ";
    std::cin >> developer;
    std::cout << "Publisher: ";
    std::cin >> publisher;
    std::cout << "Erscheinungsdatum (dd.mm.YYYY): ";
    std::cin >> date;
    
    int day = base::date_parser::atoi ( &date.c_str()[0],2 );
    int month = base::date_parser::atoi ( &date.c_str()[3],2 );
    int year = base::date_parser::atoi ( &date.c_str()[6],4 );
    
    this->maxId++;
    Collection::Game* ng = new Collection::Game ( this->maxId, name, developer, publisher, base::date::mktime ( day, month, year ) );
    this->collection.add ( ng );
}

void Interface::addExistingGame ( Collection::Game* ng )
{
    if (ng->id > this->maxId)
    {
        this->maxId = ng->id;
    }
    
    this->collection.add ( ng );
}
