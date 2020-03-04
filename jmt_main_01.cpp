#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <set>
#include <vector>

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
    int i_running_total = 0;
    std::set<int> seen_set;
    std::vector<int> freqs;
    freqs.reserve(10000);

    // needed for part 2
    std::set<int> cum_freqs;
    std::vector<int> vec_cum_freqs;
    vec_cum_freqs.reserve(10000);

    std::ifstream data_file(path);

    while(getline(data_file, line))
    {
        const int freq = std::stoi(line);
        i_running_total += freq;
        freqs.push_back(freq);
        cum_freqs.insert(i_running_total);
        vec_cum_freqs.push_back(i_running_total);
    }

    data_file.close();
    std::cout << i_running_total << std::endl;

    // part 2 redux
    bool got_it = false;
    unsigned int loop_n = 1;
    int first_rep_freq = -1;
    while (!got_it)
    {
        std::vector<int>::iterator it;
        for (it = vec_cum_freqs.begin(); it != vec_cum_freqs.end(); ++it)
        {   
            if ((*it) * i_running_total < 0)
                continue;

            int test = (*it) + loop_n * i_running_total;

            // check if this number is in cum_freq;
            if (cum_freqs.find(test) != cum_freqs.end())
            {
                first_rep_freq = test;
                got_it = true;
                break;
            }
        }
        ++loop_n;    
    }
    std::cout << first_rep_freq << std::endl;

    return 0;
}
