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

Opcode str_to_op(const std::string str)
{
    if (str == "addr")
        return ADDR;
    else if (str == "addi")
        return ADDI;
    else if (str == "mulr")
        return MULR;
    else if (str == "muli")
        return MULI;
    else if (str == "banr")
        return BANR;
    else if (str == "bani")
        return BANI;
    else if (str == "borr")
        return BORR;
    else if (str == "bori")
        return BORI;
    else if (str == "setr")
        return SETR;
    else if (str == "seti")
        return SETI;
    else if (str == "gtir")
        return GTIR;
    else if (str == "gtri")
        return GTRI;
    else if (str == "gtrr")
        return GTRR;
    else if (str == "eqir")
        return EQIR;
    else if (str == "eqri")
        return EQRI;
    else if (str == "eqrr")
        return EQRR;
    else
    {
        std::cout << "WTF!" <<std::endl;
        return BAD; 
    }
}

class Instruction
{
    public:
        Opcode op;
        size_t v_0;
        size_t v_1;
        size_t v_2;

        Instruction(const Opcode op, const size_t v_0,
                    const size_t v_1, const size_t v_2);
};

Instruction::Instruction(const Opcode op, const size_t v_0,
                         const size_t v_1, const size_t v_2) :
    op(op)
  , v_0(v_0)
  , v_1(v_1)
  , v_2(v_2)
{};

class Device
{
    public:
        std::vector<size_t> v;
        const size_t ip;

        Device(const size_t ip,
               const size_t v_0, const size_t v_1,
               const size_t v_2, const size_t v_3,
               const size_t v_4, const size_t v_5);

        void apply(const Instruction& ins);

        bool operator==(const Device& rhs);

        void apply_all(const std::vector<size_t>& inst);
};

Device::Device(const size_t ip,
               const size_t v_0, const size_t v_1,
               const size_t v_2, const size_t v_3,
               const size_t v_4, const size_t v_5) :
    ip(ip)
{
    v.push_back(v_0);
    v.push_back(v_1);
    v.push_back(v_2);
    v.push_back(v_3);
    v.push_back(v_4);
    v.push_back(v_5);
};

void Device::apply(const Instruction& ins)
{
    const size_t A = ins.v_0;
    const size_t B = ins.v_1;
    const size_t C = ins.v_2;

    switch(ins.op) {
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

    // increment the value of ip
    ++v[ip];
}

Device parse_input(const std::string& path,
                   std::vector<Instruction>& instructions)
{
    std::string line;
    std::ifstream data_file(path);

    // line 0 is of the form #ip n
    getline(data_file, line);
    const size_t init_ip = std::stoi(line.substr(4, 1));
    Device dev(init_ip, 0, 0, 0, 0, 0, 0);

    while (getline(data_file, line))
    {
        // lines are of the form CODE XX XX XX
        const std::string code = line.substr(0, 4);
        Opcode op = str_to_op(code);
        // find first space
        int from = 5;
        int space = line.find(" ", from);
        const unsigned int v_0 = std::stoi(line.substr(from, space - from));
        from = space + 1;
        space = line.find(" ", from);
        const unsigned int v_1 = std::stoi(line.substr(from, space - from));
        from = space + 1;
        space = line.find(" ", from);
        const unsigned int v_2 = std::stoi(line.substr(from, 3));

        instructions.emplace_back(op, v_0, v_1, v_2);
    }
    return dev;
}

unsigned int sum_of_divisors(const unsigned int n)
{
    unsigned int T = 0;
    for (unsigned int div = 1; div <= n; ++div)
    {
        if ((n % div) == 0)
            T += div;
    }
    return T;   
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

    std::vector<Instruction> instructions;

    Device dev = parse_input(path, instructions);
    Device dev_2 = dev;

    // grab the ip
    size_t ip = dev.v[dev.ip];
    while (ip < instructions.size())
    {
        // grab the instruction corresponding to the ip
        Instruction in = instructions[ip];
        // apply that instruction to the device
        dev.apply(in);
        // get new ip
        ip = dev.v[dev.ip];
    }
    std::cout << dev.v[0] << std::endl;

    // part 2
    dev_2.v[0] = 1;
    unsigned int X = 0;

    // grab the ip
    ip = dev_2.v[dev.ip];
    while (ip < instructions.size())
    {
        // grab the instruction corresponding to the ip
        Instruction in = instructions[ip];
        // apply that instruction to the device
        dev_2.apply(in);
        // get new ip
        ip = dev_2.v[dev_2.ip];

        if (ip == 3) // we're about to enter the loop..
        {            // ..so bail out
            X = dev_2.v[1];
            break;
        }
    }

    // T is equal to the sume of all divisors (including itself)!
    std::cout << sum_of_divisors(X) << std::endl;

    return 0;
}