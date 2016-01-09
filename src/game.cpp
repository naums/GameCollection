#include "game.h"

namespace Collection
{
    Game::Game ( int id, std::string name, std::string publisher, std::string developer, time_t releasedate )
    {
        this->id=id;
        this->name = name;
        this->publisher = publisher;
        this->developer = developer;
        this->releasedate = base::date(releasedate);
    }
    
    void Game::addSystem ( enum Systems sys )
    {
        systems.add ( sys );
    }
    
    void Game::removeSystem ( enum Systems sys )
    {
        systems.remove ( systems.index(sys) );
    }
}
