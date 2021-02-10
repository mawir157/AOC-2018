#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <map>
#include <vector>

size_t string_diff(const std::string str_1, const std::string str_2)
{
    size_t diff = 0;
    // assume both string are the same length
    for (size_t i = 0; i < str_1.size(); ++i)
        if (str_1[i] != str_2[i])
            ++diff;

    return diff;
}

unsigned int string_code(const std::string code)
{
    std::map<char, int> alpha_dict;
    for (const char& c : code)
        ++alpha_dict[c];

    unsigned int two = 0;
    unsigned int three = 0;
    for (auto const& x : alpha_dict)
    {
        if (x.second == 2)
        {
            two = 1;     
        }
        else if (x.second == 3)
        {
            three = 1;      
        }
    }

    return (1 * two) + (2 * three);
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

    std::ifstream data_file(path);
    unsigned int twos = 0;
    unsigned int threes = 0;

    std::vector<std::string> v_codes;
    while (getline(data_file, line))
    {
        v_codes.push_back(line);
        const unsigned int return_code = string_code(line);
        // this code contains a 3
        if (return_code % 2)
            ++threes;

        // this code contains a 2
        if (return_code >> 1)
            ++twos;
    }

    data_file.close();
    std::cout << twos * threes << std::endl;

    for (size_t i = 0; i < v_codes.size(); ++i)
    {
        const std::string str_1 = v_codes[i];
        for (size_t j = i; j < v_codes.size(); ++j)
        {
            const std::string str_2 = v_codes[j];
            if (string_diff(str_1, str_2) == 1)
                std::cout << str_1 << std::endl << str_2 << std::endl;
        }        
    }

    return 0;
}
