#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <numeric>

enum Opcode
{
    ADDR = 0,
    ADDI,
    MULR,
    MULI,
    BANR,
    BANI,
    BORR,
    BORI,
    SETR,
    SETI,
    GTIR,
    GTRI,
    GTRR,
    EQIR,
    EQRI,
    EQRR,
    BAD
};

class Device
{
    public:
        std::vector<unsigned int> v;

        Device(const unsigned int v_1, const unsigned int v_2,
               const unsigned int v_3, const unsigned int v_4);

        void apply(const Opcode code, const unsigned int A, const unsigned int B,
                   const unsigned int C);

        bool operator==(const Device& rhs);

        void print() const;
        void apply_all(const std::vector<unsigned int>& inst);
};

Device::Device(const unsigned int v_1, const unsigned int v_2,
               const unsigned int v_3, const unsigned int v_4)
{
    v.push_back(v_1);
    v.push_back(v_2);
    v.push_back(v_3);
    v.push_back(v_4);
};

bool Device::operator==(const Device& rhs)
{
    for (size_t i = 0; i < 4; ++i)
        if (v[i] != rhs.v[i])
            return false;

    return true;
}

void Device::print() const
{
    std::cout << "[" << v[0] << ", " << v[1] << ", " << v[2] << ", " << v[3] << "]" << std::endl;
}

void Device::apply(const Opcode code, const unsigned int A, const unsigned int B,
                   const unsigned int C)
{
    switch(code) {
        case ADDR : 
            v[C] = v[A] + v[B]; break;
        case ADDI : 
            v[C] = v[A] + B; break;
        case MULR : 
            v[C] = v[A] * v[B]; break;
        case MULI : 
            v[C] = v[A] * B; break;
        case BANR : 
            v[C] = (v[A] & v[B]); break;
        case BANI : 
            v[C] = (v[A] & B); break;
        case BORR : 
            v[C] = (v[A] | v[B]); break;
        case BORI : 
            v[C] = (v[A] | B); break;
        case SETR : 
            v[C] = v[A]; break;
        case SETI : 
            v[C] = A; break;
        case GTIR : 
            v[C] = A > v[B] ? 1 : 0; break;
        case GTRI : 
            v[C] = v[A] > B ? 1 : 0; break;
        case GTRR : 
            v[C] = v[A] > v[B] ? 1 : 0; break;
        case EQIR : 
            v[C] = (A == v[B]) ? 1 : 0; break;
        case EQRI : 
            v[C] = (v[A] == B) ? 1 : 0; break;
        case EQRR : 
            v[C] = (v[A] == v[B]) ? 1 : 0; break;
        case BAD:
        default:
            std::cout << "Nope!" << std::endl;
    }
}

std::vector<Opcode> assign_codes(std::vector<Device>& before, std::vector<Device>& after,
                                 std::vector<std::vector<unsigned int>>& instructions)
{
    unsigned int count_1 = 0;
    unsigned int count_2 = 0;
    std::vector<Opcode> opcodes(16);
    std::vector<std::vector<unsigned int>> code_mat;
    for (size_t i = 0; i < before.size(); ++i)
    {
        std::vector<unsigned int> options(16);
        count_1 = 0;
        std::vector<unsigned int> input = instructions[i];
        for (int op = ADDR; op <= EQRR; ++op)
        {
            Device test = before[i];
            test.apply(static_cast<Opcode>(op), input[1], input[2], input[3]);
            if (test == after[i])
            {
                ++count_1;
                ++options[op];
            }
        }

        code_mat.push_back(options);

        if (count_1 >= 3)
            ++count_2;
    }

    std::cout << count_2 << std::endl;

    // now assign numbers to opt codes!
    unsigned int assigned = 0;
    Opcode temp = BAD;
    while (assigned < 16)
    {
        for (size_t ex = 0; ex < code_mat.size(); ++ex)
        {
            std::vector<unsigned int> row = code_mat[ex];
            const unsigned int sum = std::accumulate(row.begin(), row.end(), 0);
            if (sum == 1) // there is only one possible operator for this row
            {
                // find that operator
                for (int op = ADDR; op <= EQRR; ++op)
                {
                    if (row[op] == 1) 
                    {
                        temp = static_cast<Opcode>(op);
                        break;
                    }
                }
                unsigned int ind = instructions[ex][0];
                opcodes[ind] = temp;
                ++assigned;
                // now remove this operator from all other rows
                for (size_t del = 0; del < code_mat.size(); ++del)
                    code_mat[del][temp] = 0;
            }
            if (assigned == 16)
                break;
         }
    }

    return opcodes;
}

void boring(std::vector<std::vector<unsigned int>>& ins,
            std::vector<Opcode>& assign_codes)
{
    Device dev(0, 0, 0, 0); // INITIAL STATE IS 0,0,0,0!!!!!
    for (size_t i = 0; i < ins.size(); ++i)
    {
        Opcode op = assign_codes[ins[i][0]];
        dev.apply(op, ins[i][1], ins[i][2], ins[i][3]);
    }
    dev.print();
}

void parse_input(const std::string& path,
                 std::vector<Device>& before_devs,
                 std::vector<Device>& after_devs,
                 std::vector<std::vector<unsigned int>>& instructions,
                 std::vector<std::vector<unsigned int>>& instructions_2)
{
    std::string line;
    std::ifstream data_file(path);

    getline(data_file, line);
    bool part_1 = true;

    while (part_1)
    {
        const unsigned int v_1 = std::stoi(line.substr(9, 1));
        const unsigned int v_2 = std::stoi(line.substr(12, 1));
        const unsigned int v_3 = std::stoi(line.substr(15, 1));
        const unsigned int v_4 = std::stoi(line.substr(18, 1));

        before_devs.emplace_back(v_1, v_2, v_3, v_4);

        // move to next line
        getline(data_file, line);
        // find first space
        int from = 0;
        int space = line.find(" ", from);
        const unsigned int i_1 = std::stoi(line.substr(from, space - from));
        from = space + 1;
        space = line.find(" ", from);
        const unsigned int i_2 = std::stoi(line.substr(from, space - from));
        from = space + 1;
        space = line.find(" ", from);
        const unsigned int i_3 = std::stoi(line.substr(from, space - from));
        from = space + 1;
        const unsigned int i_4 = std::stoi(line.substr(from, 3));
        std::vector<unsigned int> inst_set{i_1, i_2, i_3, i_4};
        instructions.push_back(inst_set);

        // move to next line
        getline(data_file, line);
        const unsigned int o_1 = std::stoi(line.substr(9, 1));
        const unsigned int o_2 = std::stoi(line.substr(12, 1));
        const unsigned int o_3 = std::stoi(line.substr(15, 1));
        const unsigned int o_4 = std::stoi(line.substr(18, 1));

        after_devs.emplace_back(o_1, o_2, o_3, o_4);

        // move to next line
        getline(data_file, line);
        getline(data_file, line);

        part_1 = (line.size() > 0);
    }

    // part 2
    while(getline(data_file, line))
    {
        if (line.size() == 0)
            continue;

        int from = 0;
        int space = line.find(" ", from);
        const unsigned int i_1 = std::stoi(line.substr(from, space - from));
        from = space + 1;
        space = line.find(" ", from);
        const unsigned int i_2 = std::stoi(line.substr(from, space - from));
        from = space + 1;
        space = line.find(" ", from);
        const unsigned int i_3 = std::stoi(line.substr(from, space - from));
        from = space + 1;
        const unsigned int i_4 = std::stoi(line.substr(from, 3));
        std::vector<unsigned int> inst_set{i_1, i_2, i_3, i_4};
        instructions_2.push_back(inst_set);
    }

    return;
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

    std::vector<Device> before;
    std::vector<Device> after;
    std::vector<std::vector<unsigned int>> instructions;
    std::vector<std::vector<unsigned int>> instructions_2;

    parse_input(path, before, after, instructions, instructions_2);
    std::vector<Opcode> code_look_up = assign_codes(before, after, instructions);
    boring(instructions_2, code_look_up);

    return 0;
}