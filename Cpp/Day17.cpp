#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <iterator>

class Coord
{
    public:
        int x;
        int y;

        Coord(const int x, const int y);
};

Coord::Coord(const int x, const int y) :
    x(x)
  , y(y) 
{};

class Water
{
    public:
        Coord pos;
        bool live;
        bool still;

        Water(const int x, const int y, const bool live);
};

Water::Water(const int x, const int y, const bool live) :
    pos(x,y)
  , live(live)
{};

class World
{
    public:
        std::vector<Coord> walls;
        std::vector<Water> water;

        // 0 down, 1 left, 2 right
        bool check(Water* p, const int dir);
        bool check_down(Water* pw)  { return check(pw, 0); }
        bool check_left(Water* pw)  { return check(pw, 1); }
        bool check_right(Water* pw) { return check(pw, 2); }

        void fill_horz(Water* pw);
        void fill_vert(Water* pw);

        std::vector<size_t> recur_index;
        int max_y;

        bool tick();
        void print(bool small = false) const;

        World();
};

World::World() : 
    max_y(0)
{};

bool World::check(Water* pw, const int dir)
{
    std::vector<Water>::iterator dead_it;
    std::vector<Coord>::iterator wall_it;

    int ud = 0;
    int lr = 0;
    if (dir == 0)
        ud = 1;
    else if (dir == 1)
        lr = -1;
    else if (dir == 2)
        lr = 1;
    // see if a wall is stopping the water falling
    for (wall_it = walls.begin(); wall_it != walls.end(); ++wall_it)
    {
        // wall is stopping the water falling
        if ((wall_it->x == pw->pos.x + lr) &&
            (wall_it->y  == pw->pos.y + ud))
            return false;
    }
    // see if still water is stopping the water falling
//    for (dead_it = water.begin(); dead_it != water.end(); ++dead_it)
//    {
//        // water is stopping the water falling
//        if ((dead_it->pos.x == pw.pos.x + lr) &&
//            (dead_it->pos.y == pw.pos.y + ud))
//            return false;
//    }
    return true;
}

void World::fill_vert(Water* pw)
{
    while (check_down(pw))
    {
        pw->live = false;
        const int x = pw->pos.x;
        const int y = pw->pos.y;
        Water head = Water(x, y + 1, false);
        water.emplace_back(head);

        std::cout << head.pos.x << "," << head.pos.y << std::endl;
        pw = head.front();
    }
    std::cout << std::endl;
}

// water block at pw has hit something from below
void World::fill_horz(Water* pw)
{
    while (check_left(pw)) // move left
    {
        pw->live = false;
        const int x = pw->pos.x;
        const int y = pw->pos.y;
        Water head = Water(x - 1, y, true);
        if (!check_down(head)) // if there's ground beneath keep going
        {
            head.live = false;
            water.emplace_back(head);
        }
        else
        {
            water.emplace_back(head);
            break;
        }
    }

    while (check_right(pw)) // move left
    {
        pw->live = false;
        const int x = head.pos.x;
        const int y = head.pos.y;
        Water head = Water(x - 1, y, true);
        if (!check_down(head)) // if there's ground beneath keep going
        {
            head.live = false;
            water.emplace_back(head);
        }
        else
        {
            water.emplace_back(head);
            break;
        }
    }
}

World parse_input(const std::string& path)
{
    std::string line;
    std::ifstream data_file(path);

    World wld;

    while (getline(data_file, line))
    {
        const char first = line.at(0);
        bool fixed_x = (first == 'x');

        const int cm = line.find(",", 0);
        const unsigned int fixed = std::stoi(line.substr(2, cm - 2));
        const int eq = line.find("=", cm);
        const int dot_dot = line.find(".", eq);

        const unsigned int var_lo = std::stoi(line.substr(eq + 1, dot_dot - eq - 1));
        const unsigned int var_hi = std::stoi(line.substr(dot_dot + 2, 5));

        if (fixed_x)
        {
            for (unsigned int y = var_lo; y <= var_hi; ++y)
                wld.walls.emplace_back(fixed, y);
        }
        else
        {
            for (unsigned int x = var_lo; x <= var_hi; ++x)
                wld.walls.emplace_back(x, fixed);
        }

    }

    for (size_t i = 0; i < wld.walls.size(); ++i)
        if (wld.walls[i].y > wld.max_y)
            wld.max_y = wld.walls[i].y;

    std::cout << "max y = " << wld.max_y << std::endl;

    // add spring at x=500, y=0
    wld.water.emplace_back(500, 0, true);

    return wld;
}

bool World::tick()
{
    size_t init_size = water.size();
    for (size_t i = 0; i < init_size; ++i)
        if (water[i].live)
           fill_vert(water[i]);

    std::cout << "----->" << water.size() << "<-----" << std::endl;
    for (size_t i = 0; i < water.size(); ++i)
    {
        std::cout << "(" << water[i].pos.x << ","
                         << water[i].pos.y << "|"
                         << water[i].live << ")"
                         << std::endl;
    }

/*    for (size_t i = 0; i < water.size(); ++i)
        if (water[i].live)
           fill_horz(water[i]); */

    return false;
}

void World::print(bool small) const
{
    // get bounds
    int min_x =  1000000;
    int max_x = -1000000;
    int min_y =  1000000;
    int max_y = -1000000;

    for (size_t i = 0; i < water.size(); ++i)
    {
        if (water[i].pos.x < min_x)
            min_x = water[i].pos.x;

        if (water[i].pos.x > max_x)
            max_x = water[i].pos.x;

        if (water[i].pos.y < min_y)
            min_y = water[i].pos.y;

        if (water[i].pos.y > max_y)
            max_y = water[i].pos.y;
    }

    for (size_t i = 0; i < walls.size(); ++i)
    {
        if (walls[i].x < min_x)
            min_x = walls[i].x;

        if (walls[i].x > max_x)
            max_x = walls[i].x;

        if (walls[i].y < min_y)
            min_y = walls[i].y;

        if (walls[i].y > max_y)
            max_y = walls[i].y;
    }
    std::cout << "limits: [" << min_x << "," << max_x << "] "
              << "[" << min_y << "," << max_y << "]" <<std::endl;

    for (int y = min_y; y <= max_y; ++y)
    {
        for (int x = min_x; x <= max_x; ++x)
        {
            bool drawn = false;
            for (size_t i = 0; i < water.size(); ++i)
            {
                if ((water[i].pos.x == x) &&
                    (water[i].pos.y == y))
                {
                    std::cout << "~";
                    drawn = true;
                    break;
                }
            }
            if (drawn)
                continue;
            for (size_t i = 0; i < walls.size(); ++i)
            {
                if ((walls[i].x == x) &&
                    (walls[i].y == y))
                {
                    std::cout << "#";
                    drawn = true;
                    break;
                }
            }
            if (drawn)
                continue;
            std::cout << " ";
        }
        std::cout << std::endl;    
    }
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


    World solaris = parse_input(path);

    size_t prev_water = 0;
    //while (solaris.tick())
    for (int i = 0; i < 2; ++i)
    {        
        std::cout << solaris.water.size() - 1 << std::endl;
        std::cout << "================================" << std::endl;
        solaris.tick();
    }
    std::cout << solaris.water.size() - 1 << std::endl;
    std::cout << "================================" << std::endl; 
    solaris.print(); 

/*    for (size_t i = 0; i < solaris.water.size(); ++i)
    {
        std::cout << "(" << solaris.water[i].pos.x << ","
                         << solaris.water[i].pos.y << ")"
                         << std::endl;
    }*/


    return 0;
}