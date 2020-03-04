#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

#define X_DIM 1000
#define Y_DIM 1000

typedef const unsigned int cuint;

size_t coords_to_index(const size_t x, const size_t y,
                       const size_t x_dim, const size_t y_dim)
{
    return (x - 1) + (y - 1) * x_dim;
}

void add_box(const size_t x_offest, const size_t y_offest, 
             const size_t x_length, const size_t y_length,
             std::vector<unsigned int>& grid)
{
    for (size_t x = 1; x <= x_length; ++x)
    {
        for (size_t y = 1; y <= y_length; ++y)
        {
            const size_t index = coords_to_index(x_offest + x, y_offest + y, X_DIM, Y_DIM);
            ++grid[index];
        }
    }
}

bool check_box(const size_t x_offest, const size_t y_offest, 
               const size_t x_length, const size_t y_length,
               const std::vector<unsigned int>& grid)
{
    for (size_t x = 1; x <= x_length; ++x)
    {
        for (size_t y = 1; y <= y_length; ++y)
        {
            const size_t index = coords_to_index(x_offest + x, y_offest + y, X_DIM, Y_DIM);
            if (grid[index] != 1)
                return false;
        }
    }
    return true;
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

    std::string line;

    std::vector<unsigned int> grid(X_DIM * Y_DIM);

    std::ifstream data_file(path);
    while (getline(data_file, line))
    {
        cuint at_sign = line.find("@");
        cuint comma = line.find(",");
        cuint colon = line.find(":");
        cuint times = line.find("x");

        const size_t x_offest = std::stoi(line.substr(at_sign + 2, comma - at_sign - 2));
        const size_t y_offest = std::stoi(line.substr(comma + 1, colon - comma - 1));
        const size_t x_length = std::stoi(line.substr(colon + 2, times - colon - 2));
        const size_t y_length = std::stoi(line.substr(times + 1, times - colon - 1));

        add_box(x_offest, y_offest, x_length, y_length, grid);
    }

    unsigned int overlap = 0;
    for (size_t i = 0; i < grid.size(); ++i)
        if (grid[i] >= 2)
            ++overlap;

    std::cout << overlap<< std::endl;

    // part 2 got round again
    data_file.clear();
    data_file.seekg(0);
    while (getline(data_file, line))
    {
        cuint at_sign = line.find("@");
        cuint comma = line.find(",");
        cuint colon = line.find(":");
        cuint times = line.find("x");

        const size_t x_offest = std::stoi(line.substr(at_sign + 2, comma - at_sign - 2));
        const size_t y_offest = std::stoi(line.substr(comma + 1, colon - comma - 1));
        const size_t x_length = std::stoi(line.substr(colon + 2, times - colon - 2));
        const size_t y_length = std::stoi(line.substr(times + 1, times - colon - 1));

        if (check_box(x_offest, y_offest, x_length, y_length, grid))
        {
            std::cout << line << std::endl;
            break;
        }
    }

    data_file.close();

    return 0;
}
