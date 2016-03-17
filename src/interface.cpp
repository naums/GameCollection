#include "interface.h"
#include "base/date.h"
#include "base/date_parser.h"
#include "base/string.h"


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

void Interface::setDatabase ( dbstore* d )
{
    this->db = d;
}

void Interface::setConfig ( base::Dictionary* c )
{
    this->cfg = c;
}

void Interface::loadGames ()
{
    mysql* msql = db->getMySQL();
    mysqlpp::StoreQueryResult res = msql->query ( "SELECT * FROM game ORDER BY title ASC" );
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

void Interface::printHelp()
{
    printf ("Following commands are allowed: \n  l   -> print gamelist \n  a   -> add new game \n  r   -> remove a game \n  e   -> edit an existing game \n  h   -> print this helptext\n  q|x -> exit the application");
}

void Interface::mainloop ()
{
    char input = 'l';

    while ( input != 'x' && input != 'q' )
    {
        switch (input)
        {
            //case '\n' : case '\r': case ' ': case '\t':
            //    continue;
            //    break;
            case 'l':
                //this->printList ();
                break;
            case 'a':
                this->addNewGame ();
                break;
            case 'r':
                this->removeGame ();
                break;
            case 'e':
                this->editGame ();
                break;
            case 'h':
                this->printHelp ();
                break;
            default:
                break;
        }
        
        this->printList();

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
    string::readline ( name );
    std::cout << "Developer: ";
    string::readline ( developer );
    std::cout << "Publisher: ";
    string::readline ( publisher );
    std::cout << "Erscheinungsdatum (dd.mm.YYYY): ";
    string::readline ( date );
    
    int day = base::date_parser::atoi ( &date.c_str()[0],2 );
    int month = base::date_parser::atoi ( &date.c_str()[3],2 );
    int year = base::date_parser::atoi ( &date.c_str()[6],4 );
    
    this->maxId++;
    Collection::Game* ng = new Collection::Game ( this->maxId, name, publisher, developer, base::date::mktime ( day, month, year ) );
    this->collection.add ( ng );
    
    db->addGame ( ng );
}

void Interface::removeGame ()
{
    std::cout << "Spiel löschen" << std::endl 
              << "-------------" << std::endl 
              << "ID: ";
    int id;
    std::cin >> id;
    
    if (id<0 || id>this->maxId) 
    {
        std::cout << "Es gibt nichts zu löschen" << std::endl;
        return;
    }
    
    for (int i=0; i<collection.getSize(); i++)
    {
        Collection::Game* mygame = collection.getValue(i);
        if (mygame->id == id)
        {
            collection.remove ( i );
            db->deleteGame ( mygame );
            std::cout << "Habe Spiel "<< mygame->name << " gelöscht" << std::endl;
            delete mygame;
            return;
        }   
    }
    
    std::cout << "Es gibt nichts zu löschen" << std::endl;
    return;
}

void Interface::editGame ()
{
    std::cout << "Spiel bearbeiten" << std::endl 
              << "-------------" << std::endl 
              << "ID: ";
    int id;
    std::cin >> id;
    
    if (id<0 || id>this->maxId) 
    {
        std::cout << "Es gibt nichts zu bearbeiten" << std::endl;
        return;
    }
    
    Collection::Game* mygame;
    for (int i=0; i<collection.getSize(); i++)
    {
        mygame = collection.getValue(i);
        if (mygame->id == id)
        {
            break;
        }   
    }
    
    base::date* releasedate = new base::date ( mygame->releasedate );
    std::cout << "Folgendes Spiel wurde gewählt:"
              << "> Titel:             " << mygame->name << std::endl
              << "> Publisher:         " << mygame->publisher << std::endl
              << "> Developer:         " << mygame->publisher << std::endl
              << "> Erscheinungsdatum: " << releasedate->toString ( base::DDMMYYYY ) << std::endl << std::endl;
              
    std::string name, developer, publisher, date;
    std::cout << "Name: "; 
    std::cin >> name;
    std::cout << "Developer: ";
    std::cin >> developer;
    std::cout << "Publisher: ";
    std::cin >> publisher;
    std::cout << "Erscheinungsdatum (dd.mm.YYYY): ";
    std::cin >> date;
    
    bool changed = false;
    
    if (date.length() >= 10)
    {
        int day = base::date_parser::atoi ( &date.c_str()[0],2 );
        int month = base::date_parser::atoi ( &date.c_str()[3],2 );
        int year = base::date_parser::atoi ( &date.c_str()[6],4 );
        
        mygame->releasedate = base::date::mktime ( day, month, year );
        changed = true;
    }
    if (name.length() >= 1)
    {
        mygame->name = name;
        changed = true;
    }
    if (developer.length() >= 1)
    {
        mygame->developer = developer;
        changed = true;
    }
    if (publisher.length() >= 1)
    {
        mygame->publisher = publisher;
        changed = true;
    }
    
    if (changed)
    {
        db->storeGame ( mygame );
        std::cout << "Spiel " << mygame->name << " wurde gespeichert" << std::endl;
    }
    else
    {
        std::cout << "Nichts geändert, speichere nicht erneut" << std::endl;
    }
}

void Interface::addExistingGame ( Collection::Game* ng )
{
    if (ng->id > this->maxId)
    {
        this->maxId = ng->id;
    }

    this->collection.add ( ng );
}
