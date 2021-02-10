#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

bool are_inverse(const char a, const char b)
{
    const char lo = std::min(a, b);
    const char hi = std::max(a, b);

    return ((hi - 32) == lo);
}

char get_inverse(const char a)
{
    if (a < 91)
        return a + 32;
    else
        return a - 32;
}

std::vector<char> parse_input_1(const std::string& path)
{
    std::string line;
    std::ifstream data_file(path);
    std::vector<char> out_vector;
    out_vector.reserve(1000000);

    while (getline(data_file, line))
    {
        for (const char& nxt : line)
        {
            char cur = ' ';
            if (out_vector.size() == 0)
                cur = '!';
            else
                cur = out_vector.back();

            if (are_inverse(cur, nxt))
                out_vector.pop_back();
            else
                out_vector.push_back(nxt);
        }
    }
    return out_vector;
}

std::vector<char> reduce_again(const std::vector<char>& word, const char c)
{
    // step 1 remove all c / Cs
    std::vector<char> new_word;
    new_word.reserve(word.size());
    for (size_t i = 0; i < word.size(); ++i)
    {
        if ((word[i] == c) || (word[i] == get_inverse(c)))
            continue;

        new_word.push_back(word[i]);
    }

    std::vector<char> out_vector;
    for (size_t i = 0; i < new_word.size(); ++i)
    {
        char cur = ' ';
        char nxt = new_word[i];
        if (out_vector.size() == 0)
            cur = '!';
        else
            cur = out_vector.back();

        if (are_inverse(cur, nxt))
            out_vector.pop_back();
        else
            out_vector.push_back(nxt);
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

    std::vector<char> reduced_word = parse_input_1(path);

    std::cout << reduced_word.size() << std::endl;

    size_t min_length = reduced_word.size();
    for (char c = 'a'; c <= 'z'; ++c)
    {
        const std::vector<char> rereduced_word = reduce_again(reduced_word, c);
        if (rereduced_word.size() < min_length)
            min_length = rereduced_word.size();
    }
    std::cout << min_length << std::endl;

    return 0;
}