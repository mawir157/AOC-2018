#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

// convert rules to binary
// 01.02.04.08.16 => 01
// #.#.# => # == 1 + 4 + 16 => 32 = (21, 1)
unsigned int chtoin(const char ch)
{
    if (ch == '#')
        return 1;
    else if (ch == '.')
        return 0;
    else
        return 0;
}

char intoch(const unsigned int i)
{
    if (i == 1)
        return '#';
    else if (i == 0)
        return '.';
    else
        return '!';
}

class Rule
{
    public:
        unsigned int lhs; // (0 to 32)
        unsigned int rhs; // (0 or 1)
        Rule(const int a0, const int a1, const int a2, 
             const int a3, const int a4, const int b);
};

Rule::Rule(const int a0, const int a1, const int a2, 
             const int a3, const int a4, const int b) :
   lhs(a0 + (2*a1) + (4*a2) + (8*a3) + (16*a4))
 , rhs(b) {};


class World
{
    public:
        std::vector<int> mv_pots;
        std::vector<Rule> mv_rules;
        unsigned int mn_offset;
        void tick();
        void print() const;
        int sum_of_pots() const;

        World(const std::vector<int>& pots,
              const std::vector<Rule>& rules,
              const unsigned int offset);
};

World::World(const std::vector<int>& pots,
              const std::vector<Rule>& rules,
              const unsigned int offset) :
    mv_pots(pots)
  , mv_rules(rules)
  , mn_offset(offset)

{};

void World::print() const
{
    for (unsigned int i = 0; i < mv_pots.size(); ++i)
        std::cout << intoch(mv_pots[i]);
    std::cout << std::endl;
}

void World::tick()
{
    std::vector<int> jean_luc(mv_pots.size());

    for (unsigned int i = 2; i < mv_pots.size() - 2;  ++i)
    {
        unsigned int bin_comb = mv_pots[i - 2] + 
                                2*mv_pots[i - 1] + 
                                4*mv_pots[i] + 
                                8*mv_pots[i + 1] + 
                                16*mv_pots[i + 2];
        // see if this is one of our rules
        for (size_t j = 0; j < mv_rules.size(); ++j)
        {
            if (mv_rules[j].lhs == bin_comb)
            {
                jean_luc[i] = mv_rules[j].rhs;
                break;
            }
        }
    }
    for (size_t i = 0; i < mv_pots.size(); ++i)
        mv_pots[i] = jean_luc[i];

    return;
}

int World::sum_of_pots() const
{
    int total = 0;
    for (size_t i = 0; i < mv_pots.size(); ++i)
        total += (mv_pots[i]*(i - mn_offset));

    return total;
}

World parse_input(const std::string path) 
{
    std::string line;
    std::ifstream data_file(path);

    // line 1 parse to std::vector<int>
    getline(data_file, line);
    std::vector<int> pots;
    // prepend 20 empty pots
    unsigned int offset = 2000; //20;
    for (unsigned int i = 0; i < offset; ++i)
        pots.push_back(0);
    // the add the initial pots
    for (unsigned int i = 15; i < line.size(); ++i)
    {
        char ch = line.substr(i, 1).at(0);
        pots.push_back(chtoin(ch));
    }
    // append another 20 empty pots
    for (unsigned int i = 0; i < offset; ++i)
        pots.push_back(0);
    // line 3 IGNORE
    getline(data_file, line);

    std::vector<Rule> rules;
    while (getline(data_file, line))
    {
        unsigned int a1 = chtoin(line.substr(0, 1).at(0));
        unsigned int a2 = chtoin(line.substr(1, 1).at(0));
        unsigned int a3 = chtoin(line.substr(2, 1).at(0));
        unsigned int a4 = chtoin(line.substr(3, 1).at(0));
        unsigned int a5 = chtoin(line.substr(4, 1).at(0));
        unsigned int b  = chtoin(line.substr(9, 1).at(0));
        Rule r(a1, a2, a3, a4, a5, b);
        rules.push_back(r);
    }

    World wld(pots, rules, offset);

    data_file.close();
    return wld;
}

int main(int argc, char *argv[])
{   
    std::string path = "";
    if (argc > 1)
    {
        std::string val;

        std::istringstream iss_upto(argv[1]);
        if (iss_upto >> val)
            path = val;
    }
    else
    {
        std::cout << "Supply a file path\n";
        return -1;
    }

    World garden = parse_input(path);
    for (unsigned int i = 0; i < 20; ++i)
        garden.tick();

    std::cout << garden.sum_of_pots() << std::endl;

    // it goes linear after a few hundred so calculate
    // score = a * generations + b;
    int score_1 = 0;
    int score_2 = 0;
    const unsigned int upto = 2000;
    for (unsigned int i = 20; i <= upto; ++i)
    {
        garden.tick();
        
        if (i == upto - 1)
            score_1 =  garden.sum_of_pots();

        if (i == upto)
            score_2 =  garden.sum_of_pots();
    }

    long long a = score_2 - score_1;
    long long b = score_2 - (a * (upto + 1));
    std::cout << a << "," << b << " ~ " << a * 50000000000 + b << std::endl;


    return 0;
}
