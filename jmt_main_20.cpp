#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <iterator>
#include <memory>

class Node
{
    public:
        const size_t parent_index;
        std::vector<char> v_steps;

        void add_char(const char c) { v_steps.push_back(c); }
        void print(bool newline = true) const;

        Node(const size_t pid);
};

Node::Node(size_t const pid) :
    parent_index(pid)
{};

void Node::print(bool newline) const
{
    for(unsigned int i = 0; i < v_steps.size(); ++i)
        std::cout  << v_steps[i];

    std::cout << " <-- [" << parent_index << "]";

    if (newline)
        std::cout<< std::endl;
}

class Tree
{
    public:
        std::vector<Node> nodes;
        const std::string input;

        size_t add_node(const size_t par_ind, size_t i, unsigned int depth = 0);

        Tree(const std::string input);
};

// ^W(SS|NN(EEE(NNNN|SSSS)|WWW(EEEE|WWWW(EEEEE|SSSSS))))$"

size_t Tree::add_node(const size_t p_ind, size_t i, unsigned int depth)
{
std::cout << "\n";
for (unsigned int p = 0; p < depth; ++p)
    std::cout << "\t";
std::cout << "[" << depth << "]";

    Node temp(p_ind);
    nodes.push_back(temp);
    size_t cur_ind = nodes.size() - 1; // id of node we've just added;
    ++i;
    char head_c = input.at(i); // current char

    // add chars to current node
    while ((head_c == 'N') || (head_c == 'E') || 
           (head_c == 'W') || (head_c == 'S'))
    {
std::cout << head_c;
            nodes[cur_ind].add_char(head_c);
            ++i;
            head_c = input.at(i);
    }
    // done adding chars two options now
    // add a child node or go back upstream
    if (head_c == ')') // go back upstream
    {
        return i;
    }

    if (head_c == '(') // add a child node
    {
        i = add_node(cur_ind, i, depth + 1);
    }

    if (head_c == '|')
    {
        return i;
    }

    std::cout << head_c;
    return i;
}

Tree::Tree(const std::string input) :
    input(input)
{
    // Tree Root
    add_node(1000000, 0);
};

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

    //const std::string input = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$";
    const std::string input = "^W(SS|NN(EEE(NNNN|SSSS)|WWW(EEEE|WWWW(EEEEE|SSSSS))))$";
    //const std::string input = "^ENWWW(NEEE|SSE(EE|N))$";
    Tree treebeard = Tree(input);

    std::cout << std::endl;
    for (size_t i = 0; i < treebeard.nodes.size(); ++i)
    {
        std::cout << "[" << i << "] ";
        treebeard.nodes[i].print();
    }

    return 0;
}



/*WSSEESWWWNW
           ----->S
           ----->NENNEEEENN
                           ----->ESSSSW
                                       ----->NWSW
                                       ----->SSEN
                           ----->WSWWN
                                      ----->E
                                      ----->WWS
                                               ----->E
                                               ----->SS*/