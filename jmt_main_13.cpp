#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>

class Coord
{
    public:
        int x;
        int y;
        Coord(const unsigned  int x, const unsigned  int y); 
};

Coord::Coord(const unsigned  int x, const unsigned  int y) :
    x(x)
  , y(y) {};

class Train
{
    public:
        Coord pos;
        Coord vel;
        int turn_dir; // -1 = left, 0 = straight, 1 = right
        bool crashed;

        Train(const Coord pos, const Coord vel);
        void move();
        void turn(const char c);
};

Train::Train(const Coord pos, const Coord vel) :
    pos(pos)
  , vel(vel) 
  , turn_dir(-1)
  , crashed(false) {};

void Train::move()
{
    pos.x += vel.x;
    pos.y += vel.y;
}

void Train::turn(const char c)
{
    if (c == '+')
    {
        if (turn_dir == -1)
        {
            if ((vel.x == 1) && (vel.y == 0))
            {
                vel.x = 0; vel.y = 1;
            }
            else if ((vel.x == 0) && (vel.y == 1))
            {
                vel.x = -1; vel.y = 0;
            }
            else if ((vel.x == -1) && (vel.y == 0))
            {
                vel.x = 0; vel.y = -1;
            }
            else if ((vel.x == 0) && (vel.y == -1))
            {
                vel.x = 1; vel.y = 0;
            }
        }
        else if (turn_dir == 1)
        {
            if ((vel.x == 1) && (vel.y == 0))
            {
                vel.x = 0; vel.y = -1;
            }
            else if ((vel.x == 0) && (vel.y == 1))
            {
                vel.x = 1; vel.y = 0;
            }
            else if ((vel.x == -1) && (vel.y == 0))
            {
                vel.x = 0; vel.y = 1;
            }
            else if ((vel.x == 0) && (vel.y == -1))
            {
                vel.x = -1; vel.y = 0;
            }
        }
        ++turn_dir;
        if (turn_dir == 2)
            turn_dir = -1;
    }
    else if (c == '/')
    {
        const int dx = vel.x;
        const int dy = vel.y;
        vel.x = -dy;
        vel.y = -dx;
    }
    else if (c == '\\')
    {
        const int dx = vel.x;
        const int dy = vel.y;
        vel.x = dy;
        vel.y = dx;
    }
}

bool train_order (const Train& tr_1, const Train& tr_2)
{
    if (tr_1.pos.x != tr_2.pos.x)
        return tr_1.pos.x < tr_2.pos.x;

    return tr_1.pos.y < tr_2.pos.y;
}

class Track
{
    public:
        const char value; // / \ or +
        const Coord pos;

        Track(const char v, const Coord p);
};

Track::Track(const char v, const Coord p) :
    value(v)
  , pos(p) {};

class Metro
{
    public:
        std::vector<Train> mv_trains;
        std::vector<Track> mv_tracks;
        bool tick();
        bool collisions(const Coord c);
        char at(const Coord c) const;
        unsigned int active_trains() const;

        Metro(const std::vector<Train>& trains,
              const std::vector<Track>& metro);
};

Metro::Metro(const std::vector<Train>& trains,
             const std::vector<Track>& metro) :
    mv_trains(trains)
  , mv_tracks(metro) {};

bool Metro::tick()
{
    std::vector<Train>::iterator tr;
    for (tr = mv_trains.begin(); tr != mv_trains.end(); ++tr)
    {
        if (tr->crashed)
            continue;

        // advance train by v
        tr->move();
        // check for collisions
        collisions(tr->pos);

        char head = at(tr->pos); // what does the track look like here
        // change direction if required
        tr->turn(head);
    }

    if (active_trains() == 1)
    {
        for (size_t i = 0; i < mv_trains.size(); ++i)
        {
            Train tr = mv_trains[i];
            if (!tr.crashed)
                std::cout << "final train: " << tr.pos.y
                          << "," << tr.pos.x <<std::endl;
        }
        return false;
    }

    // we need to iterate over the trains in a specif order!
    std::sort(mv_trains.begin(), mv_trains.end(), train_order);
    return true;
}

char Metro::at(const Coord c) const
{
    for (size_t i = 0; i < mv_tracks.size(); ++i)
    {
        Track t = mv_tracks[i];
        if ((c.x == t.pos.x) && (c.y == t.pos.y))
            return t.value;
    }
    return ' ';
}

bool Metro::collisions(const Coord c)
{
    int train_count = 0;
    std::vector<size_t> crash_index;
    for (size_t i = 0; i < mv_trains.size(); ++i)
    {
        Train tr = mv_trains[i];
        if ((tr.pos.x == c.x) && (tr.pos.y == c.y) && !tr.crashed)
        {
            ++train_count;
            crash_index.push_back(i);
        }
    }

    if (train_count > 1)
    {
        for (size_t i = 0; i < crash_index.size(); ++i)
            mv_trains[crash_index[i]].crashed = true;

        std::cout << c.y << "," << c.x << " ~ " << active_trains() << std::endl;
    }

    return (train_count > 1);
}

unsigned int Metro::active_trains() const
{
    unsigned int a_trains = 0;  
    std::vector<Train>::const_iterator tr;    
    for (tr = mv_trains.begin(); tr != mv_trains.end(); ++tr)
        if (!tr->crashed) 
            ++a_trains;

    return a_trains;
}

Metro parse_input(const std::string path) 
{
    std::string line;
    std::ifstream data_file(path);

    std::vector<Train> trains;
    std::vector<Track> bits_of_track;
    unsigned int x = 0;
    unsigned int y = 0;
    while (getline(data_file, line))
    {
        y = 0;
        for (unsigned int i = 0; i < line.size(); ++i)
        {    
            const Coord p(x, y);
            const char c = line.substr(i, 1).at(0);

            // track component
            if ((c == '/') || (c == '\\') || (c == '+'))
            {
                const Track m(c, p);
                bits_of_track.push_back(m);
            }

            // train
            if ((c == '<') || (c == '^') || (c == '>') || (c == 'v'))
            {
                Coord v(0, 0);
                if (c == '<')
                {
                    v.x = 0; v.y = -1;
                }
                else if (c == '^')
                {
                    v.x = -1; v.y = 0;
                }
                else if (c == '>')
                {
                    v.x = 0; v.y = 1;
                }
                else if (c == 'v')
                {
                    v.x = 1; v.y = 0;
                }
                const Train t(p, v);
                
                trains.push_back(t);
            }
            ++y;
        }
        ++x;
    }
    std::sort(trains.begin(), trains.end(), train_order);
    Metro trk(trains, bits_of_track);

    data_file.close();
    return trk;
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

    Metro paris_metro = parse_input(path);

    while (paris_metro.tick()) {}

    return 0;
}
