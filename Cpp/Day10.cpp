#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <set>
#include <algorithm>
#include <map>

class Star
{
    public:
        int x;
        int y;
        int x_vel;
        int y_vel;

        Star(const int x, const int y, const int x_vel, const int y_vel);
};

Star::Star(const int x, const int y, const int x_vel, const int y_vel) :
    x(x)
  , y(y)
  , x_vel(x_vel)
  , y_vel(y_vel) {};

class Sky
{
    public:
        std::vector<std::vector<unsigned int>> m_grid;
        std::vector<Star> m_stars;

        void tick();
        void tock();
        unsigned int x_bound() const;
        unsigned int y_bound() const;
        void print() const;

        Sky(const std::vector<Star> stars); 
};

Sky::Sky(const std::vector<Star> stars) :
    m_stars(stars) {};

void Sky::tick()
{
    for (size_t i = 0; i < m_stars.size(); ++i)
    {
        m_stars[i].x += m_stars[i].x_vel;
        m_stars[i].y += m_stars[i].y_vel;
    }
    return;
}

void Sky::tock()
{
    for (size_t i = 0; i < m_stars.size(); ++i)
    {
        m_stars[i].x -= m_stars[i].x_vel;
        m_stars[i].y -= m_stars[i].y_vel;
    }
    return;
}

void Sky::print() const
{
    int max_x = -1000000;
    int min_x = 1000000;
    int max_y = -1000000;
    int min_y = 1000000;

    for (size_t i = 0; i < m_stars.size(); ++i)
    {
        if (m_stars[i].y < min_y)
            min_y = m_stars[i].y;
        if (m_stars[i].y > max_y)
            max_y = m_stars[i].y;
        if (m_stars[i].x < min_x)
            min_x = m_stars[i].x;
        if (m_stars[i].x > max_x)
            max_x = m_stars[i].x;
    }
    for (int y_pos = min_y; y_pos <= max_y; ++ y_pos)
    {
        for (int x_pos = min_x; x_pos <= max_x; ++ x_pos)
        {
            bool star = false;
            for (size_t i = 0; i < m_stars.size(); ++i)
            {
                if ((m_stars[i].y == y_pos) && (m_stars[i].x == x_pos))
                    star = true;
            }

            if (star)
                std::cout << "#";
            else
                std::cout << " ";
        }
        std::cout << std::endl;      
    }
}

unsigned int Sky::x_bound() const
{
    int max_x = -1000000;
    int min_x = 1000000;
    for (size_t i = 0; i < m_stars.size(); ++i)
    {
        if (m_stars[i].x < min_x)
            min_x = m_stars[i].x;
        if (m_stars[i].x > max_x)
            max_x = m_stars[i].x;
    }
    return max_x - min_x;
}

unsigned int Sky::y_bound() const
{
    int max_y = -1000000;
    int min_y = 1000000;
    for (size_t i = 0; i < m_stars.size(); ++i)
    {
        if (m_stars[i].y < min_y)
            min_y = m_stars[i].y;
        if (m_stars[i].y > max_y)
            max_y = m_stars[i].y;
    }
    return max_y - min_y;
}

std::vector<Star> parse_input(const std::string& path)
{
    std::string line;
    std::ifstream data_file(path);
    std::vector<Star> stars;

    while (getline(data_file, line))
    {
        const int bra1 = line.find("<");
        const int comma1 = line.find(",");
        const int ket1 = line.find(">");

        const int bra2 = line.find("<", ket1 + 1);
        const int comma2 = line.find(",", ket1 + 1);
        const int ket2 = line.find(">", ket1 + 1);

        const size_t x  = std::stoi(line.substr(bra1 + 1, comma1));
        const size_t y  = std::stoi(line.substr(comma1 + 2, ket1));
        const int x_vel = std::stoi(line.substr(bra2 + 1, comma2));
        const int y_vel = std::stoi(line.substr(comma2 + 2, ket2));

        stars.push_back(Star(x, y, x_vel, y_vel));
    }
    return stars;
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

    std::vector<Star> stars = parse_input(path);

    std::cout << stars.size() << std::endl;
    Sky universe(stars);

    unsigned int prev_x_bound = 10000000;
    unsigned int prev_y_bound = 10000000;
    unsigned int counter = 0;
    for (unsigned int i = 0; i < 100000; ++i)
    {
        ++counter;
        universe.tick();

        const unsigned int new_x_bound = universe.x_bound();
        const unsigned int new_y_bound = universe.y_bound();
        
        if ((new_x_bound >= prev_x_bound) && (new_y_bound >= prev_y_bound))
        {
            universe.tock();
            --counter;
            break;
        }

        prev_x_bound = new_x_bound;
        prev_y_bound = new_y_bound;
    }
    universe.print();
    std::cout << counter << std::endl;

    return 0;
}
