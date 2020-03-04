#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <set>
#include <algorithm>
#include <map>

class Relation
{
    public:
        char from;
        char to;

        Relation(const char from, const char to);
};

Relation::Relation(const char from, const char to) :
    from(from)
  , to(to) {};

class Graph
{
    public:
        std::set<char> mv_vertices;
        std::vector<Relation> mv_edges;
        void GetOrder() const;
        void GetOrderWithTime(const unsigned int elves) const;

        Graph(const std::vector<Relation>& relns);
};

Graph::Graph(const std::vector<Relation>& relns) :
    mv_edges(relns)
{
    // step 1 build a set of vertices
    for (size_t i = 0; i < mv_edges.size(); ++i)
    {
        mv_vertices.insert(mv_edges[i].from);
        mv_vertices.insert(mv_edges[i].to);
    }
}

void Graph::GetOrder() const
{
    // step 1 find vertices with no ancestors
    std::vector<char> available_jobs;
    std::set<char> jobs_remaining(mv_vertices);
    std::vector<Relation> dependencies(mv_edges);
    std::set<char>::iterator it;
    while(jobs_remaining.size() > 0)
    {
        for (it = jobs_remaining.begin(); it != jobs_remaining.end(); ++it)
        {
            bool good = true;
            const char cur = (*it);
            for (size_t j = 0; j < dependencies.size(); ++j)
            {
                if (dependencies[j].to == cur)
                {
                    good = false;
                    break;
                }
            }
            if (good)
                available_jobs.push_back(cur);
        }
        // remove the first cutting edge alphabetically
        std::sort(available_jobs.begin(), available_jobs.end());
        jobs_remaining.erase(available_jobs[0]);
        std::cout << available_jobs[0];
        available_jobs.clear();

        // now rebuild graph that without edges that start at delete vertices
        dependencies.clear();
        for (size_t j = 0; j < mv_edges.size(); ++j)
        {
            Relation temp = mv_edges[j];
            if (jobs_remaining.find(temp.from) != jobs_remaining.end())
               dependencies.push_back(temp);
        }
    }
    std::cout << std::endl;
}

void Graph::GetOrderWithTime(const unsigned int elves) const
{
    std::set<char> jobs_remaining(mv_vertices);
    std::vector<Relation> dependencies(mv_edges);
    std::set<char> available_jobs;
    std::map<char, int> jobs_in_progress;
    unsigned int time_elapsed = 0;

    while(jobs_remaining.size() > 0)
    {
        int free_elves = elves - jobs_in_progress.size();

        // find all available jobs
        available_jobs.clear();
        std::set<char>::iterator it;
        for (it = jobs_remaining.begin();
            it != jobs_remaining.end(); ++it)
        {
            bool good = true;
            const char cur = (*it);
            // if there are no dependencies the graph consists of disconnected vertices
            // so all jobs are good unless already in progess
            if (dependencies.size() == 0)
            {
                if (jobs_in_progress.find(cur) != jobs_in_progress.end())
                {
                    good = false;
                    break;
                }
            }
            else
            {
                for (size_t j = 0; j < dependencies.size(); ++j)
                {
                    // needs to have no dependencies and not alreadt be taken
                    if ((dependencies[j].to == cur) ||
                        (jobs_in_progress.find(cur) != jobs_in_progress.end()))
                    {
                        good = false;
                        break;
                    }
                }
            }
            if (good) // this job has no dependencies so the elf can get on with it
                available_jobs.insert(cur);
        }


        // assign jobs to the elves
        while (free_elves > 0)
        {
            if (available_jobs.size() == 0) // there are no jobs for the elves :(
                break;

            // remove the first alphabetically
            std::set<char>::iterator kt;
            char first_job = 'Z' + 1;
            for (kt = available_jobs.begin(); kt != available_jobs.end(); ++kt)
                if ((*kt) < first_job)
                    first_job = (*kt);

            jobs_in_progress[first_job] = int(first_job - 64 + 60);
            available_jobs.erase(first_job); //this job is taken!

            free_elves = elves - jobs_in_progress.size();
        }

        // tick down on the jobs
        std::map<char, int>::iterator jt;
        for (jt = jobs_in_progress.begin(); jt != jobs_in_progress.end(); ++jt)
            --(jt->second);     

        bool removed_job = false;
        for (jt = jobs_in_progress.begin(); jt != jobs_in_progress.end(); ++jt)
        {
            if (jt->second <= 0) // the job has finished
            {
                char job_name = (jt->first);
                jobs_in_progress.erase(job_name);
                jobs_remaining.erase(job_name);
                removed_job = true;
            }
        }

        // rebuild the graph if necessary
        if (removed_job)
        {
            dependencies.clear();
            for (size_t j = 0; j < mv_edges.size(); ++j)
            {
                Relation temp = mv_edges[j];
                if (jobs_remaining.find(temp.from) != jobs_remaining.end())
                   dependencies.push_back(temp);
            }
        }

        // tick up the clock
        ++time_elapsed;  
    }
    std::cout << time_elapsed << std::endl;
}

std::vector<Relation> parse_input(const std::string& path)
{
    std::string line;
    std::ifstream data_file(path);
    std::vector<Relation> out_set;

    while (getline(data_file, line))
    {
        const char from = line.at(5);
        const char to = line.at(36);

        out_set.push_back(Relation(from, to));
    }
    return out_set;
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

    std::vector<Relation> rlns = parse_input(path);
    Graph universe(rlns);
    universe.GetOrder();
    universe.GetOrderWithTime(5);

    return 0;
}