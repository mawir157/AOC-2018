#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

inline unsigned int abs_dist(const size_t x, const size_t y)
{
    if (x == y)
        return 0;

    return std::max(x, y) - std::min(x, y);
}

inline unsigned int taxi_dist(const size_t x_1, const size_t y_1,
                 const size_t x_2, const size_t y_2)
{
    return abs_dist(x_1, x_2) + abs_dist(y_1, y_2);
}

class Point
{
    public:
        size_t x;
        size_t y;
        unsigned int id;
        Point(const size_t x, const size_t y, const unsigned int id);
};

Point::Point(const size_t x, const size_t y, const unsigned int id) : 
    x(x)
  , y(y)
  , id(id) {};

class Grid
{
    public:
        std::vector<std::vector<int>> m_grid;
        std::vector<Point> m_pts;
        unsigned int m_x_offset;
        unsigned int m_y_offset;

        Grid(const std::vector<Point>& pts);
        void Dirichlet();
        void Total_Distance();
        unsigned int Biggest_Non_Infinite();
        unsigned int Area_Less_Than(const unsigned int n);
};

Grid::Grid(const std::vector<Point>& pts) :
    m_pts(pts)
{
    // step 1 find x/y limits
    unsigned int x_min = 1000;
    unsigned int x_max = 0;
    unsigned int y_min = 1000;
    unsigned int y_max = 0;
    for (size_t i = 0; i < pts.size(); ++i)
    {
        const Point pt = pts[i];
        if (pt.x < x_min)
            x_min = pt.x;
        if (pt.x > x_max)
            x_max = pt.x;
        if (pt.y < y_min)
            y_min = pt.y;
        if (pt.y > y_max)
            y_max = pt.y;
    }
    unsigned int x_width = x_max - x_min + 1;
    unsigned int y_width = y_max - y_min + 1;

    m_x_offset = x_min;
    m_y_offset = y_min;

    m_grid.resize(x_width, std::vector<int>(y_width));
}

void Grid::Dirichlet()
{
    for (size_t i = 0; i < m_grid.size(); ++i)
    {
        for (size_t j = 0; j < m_grid[i].size(); ++j)
        {
            unsigned int real_x = i + m_x_offset;
            unsigned int real_y = j + m_y_offset;

            unsigned int near_dist = 100000;
            int near_id = -2; 
            for (size_t k = 0; k < m_pts.size(); ++k)
            {
                Point pt = m_pts[k];
                unsigned int dist = taxi_dist(real_x, real_y, pt.x, pt.y);
                if (dist == near_dist)
                {
                    near_id = -1;
                }
                else if (dist < near_dist)
                {
                    near_id = pt.id;
                    near_dist = dist;
                }
            }
            m_grid[i][j] = near_id;
        }
    }
}

void Grid::Total_Distance()
{
    for (size_t i = 0; i < m_grid.size(); ++i)
    {
        for (size_t j = 0; j < m_grid[i].size(); ++j)
        {
            unsigned int real_x = i + m_x_offset;
            unsigned int real_y = j + m_y_offset;

            unsigned int total_distance = 0;
            for (size_t k = 0; k < m_pts.size(); ++k)
            {
                Point pt = m_pts[k];
                total_distance += taxi_dist(real_x, real_y, pt.x, pt.y);
            }
            m_grid[i][j] = total_distance;
        }
    }
}

unsigned int Grid::Biggest_Non_Infinite()
{
    unsigned int biggest_area = 0;
    for (size_t k = 0; k < m_pts.size(); ++k)
    {
        bool is_infinite = false;
        unsigned int area = 0;
        for (size_t i = 0; i < m_grid.size(); ++i)
        {
            for (size_t j = 0; j < m_grid[i].size(); ++j)
            {

                if (m_grid[i][j] == int(m_pts[k].id))
                {
                    ++area;
                    if ((j == 0) || (j == m_grid[i].size() - 1) ||
                        (i == 0) || (i == m_grid.size() - 1))
                        is_infinite = true;
                }
            }
        }

        if (!is_infinite)
        {
            if (area > biggest_area)
                biggest_area = area;
        }
    }
    return biggest_area;
}

unsigned int Grid::Area_Less_Than(const unsigned int n)
{
    unsigned int area = 0;
    for (size_t i = 0; i < m_grid.size(); ++i)
    {
        for (size_t j = 0; j < m_grid[i].size(); ++j)
        {
            if (m_grid[i][j] < int(n))
                ++area;
        }
    }
    return area;
}

std::vector<Point> parse_input(const std::string& path)
{
    std::string line;
    std::ifstream data_file(path);
    std::vector<Point> out_vector;
    out_vector.reserve(50);
    unsigned int id = 0;

    while (getline(data_file, line))
    {
        const int comma = line.find(",");
        
        const unsigned int x = std::stoi(line.substr(0, comma));
        const unsigned int y = std::stoi(line.substr(comma + 2, 100));

        out_vector.push_back(Point(x,y,id));
        ++id;   
    }
    return out_vector;
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

    std::vector<Point> points = parse_input(path);

    Grid the_universe(points);
    the_universe.Dirichlet();
    std::cout << the_universe.Biggest_Non_Infinite() << std::endl;
    the_universe.Total_Distance();
    std::cout << the_universe.Area_Less_Than(10000) << std::endl;

    return 0;
}