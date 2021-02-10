#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <iterator>
#include <memory>

std::vector<unsigned int> parse_input(const std::string& path)
{
    std::string line;
    std::ifstream data_file(path);
    std::vector<unsigned int> out_vector;

    while (getline(data_file, line))
    {
        int from = 0;
        int space = line.find(" ", from);

        while (space > 0)
        {
            const unsigned int value = std::stoi(line.substr(from, space - from));
            from = space + 1;
            space = line.find(" ", from);
            out_vector.push_back(value);
        }
        // do once more to catch final value
        const unsigned int value = std::stoi(line.substr(from, 5));
        out_vector.push_back(value);
    }
    return out_vector;
}

class Node
{
    public:
        size_t parent_index;
        std::vector<size_t> child_indices;
        std::vector<unsigned int> data;

        Node(size_t const pid);
};

Node::Node(size_t const pid) :
    parent_index(pid)
{};

class Tree
{
    public:
        std::vector<Node> nodes;
        std::vector<unsigned int> input;
        size_t add_node(const size_t p_ind, size_t i);
        unsigned int sum_metadata() const;
        void find_children();
        unsigned int value_town(const size_t i) const;

        Tree(const std::vector<unsigned int> data);
};

Tree::Tree(const std::vector<unsigned int> data)
{
    input = data;
    // Tree Root
    add_node(1000000, 0);
};

size_t Tree::add_node(const size_t p_ind, size_t i)
{
    unsigned int children = input[i];
    ++i;
    unsigned int values = input[i];
    Node temp(p_ind);
    nodes.push_back(temp);
    size_t cur_ind = nodes.size() - 1;

    if (children > 0)
    {
        for (unsigned int c_i = 0; c_i < children; ++c_i)
        {
            ++i;                     
            i = add_node(cur_ind, i);
        }
    }

    if (values > 0)
    {
        for (size_t v_i = 0 ; v_i < values; ++v_i)
        {
            ++i;
            nodes[cur_ind].data.push_back(input[i]);
        }
    }

    return i;
};

unsigned int Tree::sum_metadata() const
{
    unsigned int total = 0;

    for (size_t i = 0; i < nodes.size(); ++i)
    {
        Node nd = nodes[i];
        for (size_t j = 0; j < nd.data.size(); ++j)
        {
            total+= nd.data[j];
        }
    }
    return total;   
}

void Tree::find_children()
{
    for (size_t i = 0; i < nodes.size(); ++i)
    {
        for (size_t j = 0; j < nodes.size(); ++j)
        {
            if (nodes[j].parent_index == i)
                nodes[i].child_indices.push_back(j);
        }
    }
}

unsigned int Tree::value_town(const size_t i) const
{
    unsigned int value = 0;
    Node nd = nodes[i];
    size_t kid_count = nd.child_indices.size();
    for (size_t i = 0; i < nd.data.size(); ++i)
    {
        unsigned int dt = nd.data[i];
        if (kid_count == 0)
            value += dt;
        else
        {
            unsigned int dt = nd.data[i];
            if (dt <= nd.child_indices.size())
            {
                const unsigned int child_index = nd.child_indices[dt - 1];
                value += value_town(child_index);
            }
        }
    }
    return value;
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

    std::vector<unsigned int> values = parse_input(path);
    Tree mighty_oak(values);
    std::cout << mighty_oak.sum_metadata() << std::endl;
    mighty_oak.find_children();
    std::cout << mighty_oak.value_town(0) << std::endl;

    return 0;
}

/*
0 1 2 3 4 5 6 7  8 9 0 1 2  3 4 5 6
2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
A----------------------------------
    B----------- C-----------
                     D-----
*/