#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include <map>

typedef const unsigned int cuint;

class DateTime
{
    public:
        unsigned int m_month;
        unsigned int m_day;
        unsigned int m_hour;
        unsigned int m_minute;

        DateTime(unsigned int month, unsigned int day,
                 unsigned int hour, unsigned int minute);

        bool operator<(const DateTime& dt) const;
};

DateTime::DateTime(unsigned int month,
                   unsigned int day,
                   unsigned int hour,
                   unsigned int minute) : 
    m_month(month)
  , m_day(day)
  , m_hour(hour)
  , m_minute(minute) {};

bool DateTime::operator<(const DateTime& dt) const
{
    if (m_month != dt.m_month)
        return (m_month < dt.m_month);

    if (m_day != dt.m_day)
        return (m_day < dt.m_day);

    if (m_hour != dt.m_hour)
        return (m_hour < dt.m_hour);

    if (m_minute != dt.m_minute)
        return (m_minute < dt.m_minute);

    return false;
}

class Record 
{ 
    public: 
        int mn_guard_id; 
        DateTime mc_timestamp;
        bool mb_sleep;

        bool operator<(const Record& rec) const;
        Record(int guard_id, DateTime timestamp, bool sleep);
};

Record::Record(int guard_id,
               DateTime timestamp,
               bool sleep) : 
    mn_guard_id(guard_id)
  , mc_timestamp(timestamp)
  , mb_sleep(sleep) {};

bool Record::operator<(const Record& rec) const
{
    return mc_timestamp < rec.mc_timestamp;
}

class Guard
{
    public:
        int mn_id;
        std::vector<unsigned int> mv_asleep;
        unsigned int total_sleep() const;
        size_t minute_of_max_sleep() const;
        unsigned int max_sleep_in_minute() const;
 
        Guard(int id);
};

Guard::Guard(int id) :
    mn_id(id)
  , mv_asleep(60) {};

unsigned int Guard::total_sleep() const
{
    unsigned int total = 0;
    for (size_t i = 0; i < mv_asleep.size(); ++i)
        total += mv_asleep[i];

    return total;
}

size_t Guard::minute_of_max_sleep() const
{
    unsigned int max_sleep = 0;
    size_t max_sleep_hour = 0;
    for (size_t i = 0; i < mv_asleep.size(); ++i)
    {
        if (mv_asleep[i] > max_sleep)
        {
            max_sleep = mv_asleep[i];
            max_sleep_hour = i;
        }
    }
    return max_sleep_hour;
}

unsigned int Guard::max_sleep_in_minute() const
{
    unsigned int max_sleep = 0;
    for (size_t i = 0; i < mv_asleep.size(); ++i)
        if (mv_asleep[i] > max_sleep)
            max_sleep = mv_asleep[i];

    return max_sleep;
}

std::vector<Record> parse_input(const std::string path) 
{
    std::string line;
    std::ifstream data_file(path);

    std::vector<Record> v_records;
    v_records.reserve(2000);
    while (getline(data_file, line))
    {
        const int dash1 = line.find("-");
        const int dash2 = line.find("-", dash1 + 1);
        const int spa_1 = line.find(" ");
        const int colon = line.find(":");
        const int rbrac = line.find("]");
        const int hash  = line.find("#");
        const int let_b = line.find("b");
        
        const unsigned int month = std::stoi(line.substr(dash1 + 1, 2));
        const unsigned int day   = std::stoi(line.substr(dash2 + 1, 2));
        const unsigned int hour  = std::stoi(line.substr(spa_1 + 1, 2));
        const unsigned int mins  = std::stoi(line.substr(colon + 1, 2));

        const DateTime dt(month, day, hour, mins);

        if (let_b < 0) { // this is either wake up of fall asleep record
            const std::string w_s  = line.substr(rbrac + 2, 1);
            const bool sleep = (w_s == "f") ? true : false;
            const Record rec(-1, dt, sleep);
            v_records.push_back(rec);
        } else { // this is a guard coming on duty
            const std::string guard = line.substr(hash + 1, let_b - hash - 2);
            const Record rec(std::stoi(guard), dt, false);
            v_records.push_back(rec);
        }
    }

    data_file.close();
    return v_records;
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

    // read records from file
    std::vector<Record> v_records = parse_input(path);
    // sort into chronological order
    std::sort (v_records.begin(), v_records.end());

    unsigned int guard_id = -1;
    std::vector<Guard> v_guards;
    for (size_t i = 0; i < v_records.size(); ++i) {
        const Record rec = v_records[i];
        if (rec.mn_guard_id > 0)
        {// we're onto a new record
            guard_id = rec.mn_guard_id;
        }
        else
        {
            // add a sleep period
            // find guard
            size_t guard_index = -1;
            for (size_t j = 0; j < v_guards.size(); ++j)
            {
                if (v_guards[j].mn_id == guard_id)
                {
                    guard_index = j;
                    break;
                }
            }

            // we haven't seen this record before so create a new one
            if (guard_index == -1)
            {
                v_guards.push_back(Guard(guard_id));
                guard_index = v_guards.size() - 1;
            }

            const unsigned int start = rec.mc_timestamp.m_minute;
            const Record rec_next = v_records[i + 1]; // WARNING NOT SAFE!
            const unsigned int end = rec_next.mc_timestamp.m_minute;

            for (unsigned int j = start; j < end; ++j)
                ++v_guards[guard_index].mv_asleep[j];
            
            ++i;
        }
    }

    int worst_sleeper_index = 0;
    unsigned int most_sleep = 0;
    for (size_t i = 0; i < v_guards.size(); ++i)
    {
        unsigned int sleep = v_guards[i].total_sleep();
        if (sleep > most_sleep)
        {
            worst_sleeper_index = i;
            most_sleep = sleep;
        }
    }

    int max_sleep_guard_id = v_guards[worst_sleeper_index].mn_id;
    size_t max_sleep_hour = v_guards[worst_sleeper_index].minute_of_max_sleep();

    std::cout << max_sleep_guard_id << "x" << max_sleep_hour 
              << " = " << max_sleep_guard_id * max_sleep_hour << std::endl;

    // part 2
    worst_sleeper_index = 0;
    most_sleep = 0;
    for (size_t i = 0; i < v_guards.size(); ++i)
    {
        unsigned int max_sleep_minute = v_guards[i].max_sleep_in_minute();
        if (max_sleep_minute > most_sleep)
        {
            worst_sleeper_index = i;
            most_sleep = max_sleep_minute;
        } 
    }

    max_sleep_guard_id = v_guards[worst_sleeper_index].mn_id;
    max_sleep_hour = v_guards[worst_sleeper_index].minute_of_max_sleep();

    std::cout << max_sleep_guard_id << "x" << max_sleep_hour 
              << " = " << max_sleep_guard_id * max_sleep_hour << std::endl;

    return 0;
}
