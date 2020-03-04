#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <list>

unsigned long int play_game(const unsigned int n_players, const unsigned int last)
{
    std::vector<unsigned int> marbles;
    std::list<unsigned int> circle;
    std::list<unsigned int>::iterator cur_it = circle.begin();

    // create all the marbles...
    for (unsigned int i = 0; i <= last; ++i)
        marbles.push_back(i);
    
    // ...and the players 
    std::vector<unsigned int> v_players(n_players);
    size_t cur_player = 0;

    // let player 0 have a go
    unsigned int m_val = marbles[0];

    cur_it = circle.insert(cur_it, 0);
    cur_player = (cur_player + 1) % n_players;

    for (size_t m = 1; m < marbles.size(); ++m)
    {
        // get the samllest marble
        m_val = marbles[m];     
        if ((m_val % 23) == 0) // all hell breaks loose
        {
            // decrement by 7
            for (unsigned int i = 0; i < 7; ++i)
            {
                if (cur_it == circle.begin())
                    cur_it = circle.end();
             
                --cur_it;
            }
            v_players[cur_player] += (m_val + (*cur_it));
            cur_it = circle.erase(cur_it);
        } 
        else
        {
            // increment by 2; insert goes before the pointer
            for (unsigned int i = 0; i < 2; ++i)
            {
                ++cur_it;

                if (cur_it == circle.end())
                    cur_it = circle.begin();
            }          
            cur_it = circle.insert(cur_it, m_val);

        }
        cur_player = (cur_player + 1) % n_players;
    }

    unsigned int max_score = 0;
    for (size_t i = 0; i < v_players.size(); ++i)
        if (v_players[i] > max_score) 
            max_score = v_players[i];


    return max_score;
}

int main(int argc, char *argv[])
{   
    std::cout << play_game(468, 71010) << std::endl;
    std::cout << play_game(468, 71010 * 100) << std::endl;
    return 0;

}