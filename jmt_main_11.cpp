#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>


/*Find the fuel cell's rack ID, which is its X coordinate plus 10.
Begin with a power level of the rack ID times the Y coordinate.
Increase the power level by the value of the grid serial number (your puzzle input).
Set the power level to itself multiplied by the rack ID.
Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
Subtract 5 from the power level.*/
inline int power_level(const int rack_ID, const size_t y, const int serial)
{
    int power_level = rack_ID * (y + 1);
    power_level += serial;
    power_level *= rack_ID;
    power_level /= 100; // integer divison by 100;
    power_level %= 10; //get the unit by modulo arithmetic
    power_level -= 5;
    return power_level;
}

inline int summed_power(const std::vector<std::vector<int>>& g,
                        const size_t x, const size_t y, const unsigned int w)
{
    //put bounds check in here
    if ((x + w > g.size()) || (y + w > g[0].size()))
        return -1000000;

    int sum = 0;
    for (unsigned int i = 0; i < w; ++i)
        for (unsigned int j = 0; j < w; ++j)
            sum += g[x + i][y + j];

    return sum;
}

class Pair
{
    public:
        int power;
        int width;
        Pair(const int p, const int w);
};

Pair::Pair(const int p, const int w) :
    power(p)
  , width(w) {}; 

inline Pair recursive_summed_power(const std::vector<std::vector<int>>& g,
                                  const size_t x, const size_t y)
{
    Pair power_width(-100000, 0);
    int del_x = g.size() - x;
    int del_y = g[0].size() - y;
    int max_box_size = std::min(del_x, del_y);
    int running_power = 0;
    for (int bs = 0; bs < max_box_size; ++bs)
    {
        for (int i = 0; i < bs; ++i)
        {
            running_power += g[x + i][y + bs];
            running_power += g[x + bs][y + i];
        }
        running_power += g[x + bs][y + bs];

        if (running_power > power_width.power)
        {
            power_width.power = running_power;
            power_width.width = bs + 1;
        }
    }
    return power_width;
}

int main(int argc, char *argv[])
{   
    const size_t grid_dim = 300;
    const int serial = 7672;

    std::vector<std::vector<int>> grid;
    grid.resize(grid_dim, std::vector<int>(grid_dim));
    // step 1 fill in grid  // grid[x][y]
    for (size_t x = 0; x < grid.size(); ++x)
    {
        const int rack_ID = (x + 1) + 10;
        for (size_t y = 0; y < grid[x].size(); ++y)
        {
            grid[x][y] = power_level(rack_ID, y, serial);
        }        
    }

    //part 1
    //iterate across grid looking for best 3x3 grid
    int max_power = -1000;
    size_t best_x = 0;
    size_t best_y = 0;
    for (size_t x = 0; x < grid.size() - 2; ++x)
    {
        for (size_t y = 0; y < grid[x].size() - 2; ++y) 
        {
            int s = summed_power(grid, x, y, 3);

            if (s > max_power)
            {
                max_power = s;
                best_x = x + 1;
                best_y = y + 1;
            }
        }
    } 

    std::cout << best_x << "," << best_y << std::endl;

    //part 2 better
    unsigned int best_w = 0;
    max_power = -1000;
    best_x = 0;
    best_y = 0;
    for (size_t x = 0; x < grid.size(); ++x)
    {
        for (size_t y = 0; y < grid[x].size(); ++y) 
        {
            Pair pr = recursive_summed_power(grid, x, y);
            if (pr.power > max_power)
            {
                max_power = pr.power;
                best_x = x + 1;
                best_y = y + 1;
                best_w = pr.width;                
            }
        }
    }

    std::cout << best_x << "," << best_y << "," << best_w << std::endl;

    return 0;
}




