#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <iterator>
#include <vector>
#include <list>

void play_game_1(const unsigned int upto)
{
    std::vector<unsigned int> recipes;
    size_t it_1;
    size_t it_2;

    // initial state
    recipes.push_back(3);
    recipes.push_back(7);

    it_1 = 0;
    it_2 = 0;
    ++it_2;

    while(recipes.size() < upto + 10)
    {
        unsigned int a = recipes[it_1] + recipes[it_2]; // get new recipe...
        if (a > 9)                          // add new recipe[s]...
        {
            recipes.push_back(a / 10);
            recipes.push_back(a % 10);
        }
        else
        {
            recipes.push_back(a);
        }
        // move the 'current recipes'
        const unsigned int move_1 = recipes[it_1] + 1;
        const unsigned int move_2 = recipes[it_2] + 1;
        for (unsigned int mv = 0; mv < move_1; ++mv)
        {
            ++it_1;
            if (it_1 == recipes.size())
                it_1 = 0;
        }
        for (unsigned int mv = 0; mv < move_2; ++mv)
        {
            ++it_2;
            if (it_2 == recipes.size())
                it_2 = 0;
        }
    }

    for (size_t i = upto; i < upto + 10; ++i)
        std::cout << recipes[i];

    std::cout << std::endl;

    return;
}

void play_game_2(const unsigned int seq)
{
    std::vector<unsigned int> par_seq;
    unsigned int seq_cop = seq;
    while (seq_cop > 0)
    {
        par_seq.push_back(seq_cop % 10);
        seq_cop /= 10;
    }
    const size_t par_len = par_seq.size();

    std::vector<unsigned int> recipes;
    size_t it_1;
    size_t it_2;

    // initial state
    recipes.push_back(3);
    recipes.push_back(7);

    it_1 = 0;
    it_2 = 0;
    ++it_2;

    bool match = false;
    unsigned int solution = 0;
    while(!match)
    {
        unsigned int a = recipes[it_1] + recipes[it_2]; // get new recipe...
        if (a > 9)                          // add new recipe[s]...
        {
            recipes.push_back(a / 10);
            recipes.push_back(a % 10);
        }
        else
        {
            recipes.push_back(a);
        }
        // move the 'current recipes'
        const unsigned int move_1 = recipes[it_1] + 1;
        const unsigned int move_2 = recipes[it_2] + 1;
        for (unsigned int mv = 0; mv < move_1; ++mv)
        {
            ++it_1;
            if (it_1 == recipes.size())
                it_1 = 0;
        }
        for (unsigned int mv = 0; mv < move_2; ++mv)
        {
            ++it_2;
            if (it_2 == recipes.size())
                it_2 = 0;
        }

        // check if the final n values match par_seq (backwards)
        if (recipes.size() < par_len)
            continue;

        match = true;
        for (unsigned int i = 0; i < par_len; ++i)
        {
            match &= (recipes[recipes.size() - 1 - i] == par_seq[i]);
            if (!match)
                break;
        }

        if (match)
        {
            solution = recipes.size() - par_len;
            break;
        }

        if (recipes.size() < par_len + 1)
            continue;

        match = true;
        for (unsigned int i = 0; i < par_len; ++i)
        {
            match &= (recipes[recipes.size() - 2 - i] == par_seq[i]);
            if (!match)
                break;
        }

        if (match)
        {
            solution = recipes.size() - par_len - 1;
            break;
        }
    }
    std::cout << solution << std::endl;
}

int main(int argc, char *argv[])
{   
    play_game_1(147061);
    play_game_2(147061);

    return 0;

}