#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

enum Symbol
{
    OPEN = 0,
    TREE,
    LUMB
};

typedef std::vector<std::vector<Symbol>> matrix;

matrix parse_input(const std::string& path)
{
    matrix mat;

    std::string line;
    std::ifstream data_file(path);

    while (getline(data_file, line))
    {
        std::vector<Symbol> row;
        for (unsigned int i = 0; i < line.size(); ++i)
        {
            const char c = line.at(i);
            if (c == '.')
                row.push_back(OPEN);
            else if (c == '|')
                row.push_back(TREE);
            else if (c == '#')
                row.push_back(LUMB);
        }
        mat.push_back(row);
    }
    return mat;
}

matrix tick(const matrix& mat)
{
    matrix new_mat(mat);

    for (size_t i = 0; i < mat.size(); ++i)
    {
        for (size_t j = 0; j < mat[i].size(); ++j)
        {
            Symbol sym = mat[i][j];
            size_t i_lo = (i == 0) ? 0 : i - 1;
            size_t i_hi = (i == mat.size() - 1) ? mat.size() - 1 : i + 1;

            size_t j_lo = (j == 0) ? 0 : j - 1;
            size_t j_hi = (j == mat.size() - 1) ? mat.size() - 1 : j + 1;

            unsigned int tree_count = 0;
            unsigned int lumb_count = 0;

            for(size_t c_i = i_lo; c_i <= i_hi; ++c_i)
            {
                for(size_t c_j = j_lo; c_j <= j_hi; ++c_j)
                {
                    if (mat[c_i][c_j] == TREE)
                        ++tree_count;
                    if (mat[c_i][c_j] == LUMB)
                        ++lumb_count;
                }
            }

            if (sym == OPEN)
            {
                if (tree_count >= 3)
                    new_mat[i][j] = TREE;
            }
            else if (sym == TREE)
            {
                if (lumb_count >= 3)
                    new_mat[i][j] = LUMB;
            }
            else if (sym == LUMB)
            {
                if ((lumb_count >= 2) && (tree_count >= 1))
                    new_mat[i][j] = LUMB;
                else
                    new_mat[i][j] = OPEN;
            }
        }
    } 
    return new_mat;   
}

void print_mat(const matrix& mat)
{
    for (size_t i = 0; i < mat.size(); ++i)
    {
        for (size_t j = 0; j < mat[i].size(); ++j)
        {
            if (mat[i][j] == OPEN)
                std::cout << '.';
            if (mat[i][j] == TREE)
                std::cout << '|';
            if (mat[i][j] == LUMB)
                std::cout << '#';
        }
        std::cout << std::endl;
    }
    return;
}

unsigned int score(const matrix& mat)
{
    unsigned int tree_count = 0;
    unsigned int lumb_count = 0;
    for (size_t i = 0; i < mat.size(); ++i)
    {
        for (size_t j = 0; j < mat[i].size(); ++j)
        {
            if (mat[i][j] == TREE)
                ++tree_count;
            if (mat[i][j] == LUMB)
                ++lumb_count;
        }
    } 
    return tree_count * lumb_count;   
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

    matrix mat = parse_input(path);

    // part 1
    for(unsigned int i = 0; i < 10; ++i)
        mat = tick(mat);

    std::cout << "score: " << score(mat) << std::endl;

    // part 2
    //mat = parse_input(path);

    // the solution is eventually periodic
    const unsigned int burn_in = 500;
    for(unsigned int i = 10; i < burn_in; ++i)
        mat = tick(mat);

    std::vector<unsigned int> prev_scores;
    //at this point we've been running for 1000 seconds!
    for (unsigned int i = 0; i < 10000; ++i)
    {
        const unsigned int sc = score(mat);
        bool is_in = false;
        for (size_t j = 0; j < prev_scores.size(); ++j)
        {
            if (sc == prev_scores[j])
            {
                is_in = true;
                break;
            }
        }
        if (is_in)
            break;
        else
            prev_scores.push_back(sc);

        mat = tick(mat);
    }
    unsigned int period = prev_scores.size();
    std::cout << "period is " << period << std::endl;
    const unsigned long big_n = 1000000000;
    const size_t soln_index = (big_n - burn_in) % period;
    std::cout << "score at " << big_n << " is " << prev_scores[soln_index];

    return 0;
}