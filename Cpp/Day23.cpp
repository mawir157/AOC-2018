#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <climits>
#include <set>
#include <climits>

typedef std::vector<std::vector<bool>> dist_mat;

unsigned int m_d(const int x_1, const int y_1, const int z_1,
                 const int x_2, const int y_2, const int z_2)
{
  return abs(x_1 - x_2) + abs(y_1 - y_2) + abs(z_1 - z_2); 
}

class Irreg_Octo
{
  public:
    Irreg_Octo(const int x, const int y,
               const int z, const unsigned int r);
    bool intersect(const Irreg_Octo& octopod);
    void print() const;

  public:
    int A_top;
    int A_bottom;
    int B_top;
    int B_bottom;
    int C_top;
    int C_bottom;
    int D_top;
    int D_bottom;
};

Irreg_Octo::Irreg_Octo(const int x, const int y,
                       const int z, const unsigned int r) :
    A_top(x + y + z + r)
  , A_bottom(x + y + z - r)
  , B_top(x + y - z + r)
  , B_bottom(x + y - z - r)
  , C_top(x - y + z + r)
  , C_bottom(x - y + z - r)
  , D_top(-x + y + z + r)
  , D_bottom(-x + y + z - r)
{}

bool Irreg_Octo::intersect(const Irreg_Octo& o)
{
  const int temp_A_top    = std::min(A_top, o.A_top);
  const int temp_A_bottom = std::max(A_bottom, o.A_bottom);
  if (temp_A_bottom > temp_A_top)
    return false;

  const int temp_B_top    = std::min(B_top, o.B_top);
  const int temp_B_bottom = std::max(B_bottom, o.B_bottom);
  if (temp_B_bottom > temp_B_top)
    return false;

  const int temp_C_top    = std::min(C_top, o.C_top);
  const int temp_C_bottom = std::max(C_bottom, o.C_bottom);
  if (temp_C_bottom > temp_C_top)
    return false;

  const int temp_D_top   = std::min(D_top, o.D_top);
  const int temp_D_bottom = std::max(D_bottom, o.D_bottom);
  if (temp_D_bottom > temp_D_top)
    return false;

  A_top    = temp_A_top;
  A_bottom = temp_A_bottom;
  B_top    = temp_B_top;
  B_bottom = temp_B_bottom;
  C_top    = temp_C_top;
  C_bottom = temp_C_bottom;
  D_top    = temp_D_top;
  D_bottom = temp_D_bottom;

  return true;
}

void Irreg_Octo::print() const
{
  std::cout << "A = [" << A_top << "," << A_bottom << "] "
            << "B = [" << B_top << "," << B_bottom << "] "
            << "C = [" << C_top << "," << C_bottom << "] "
            << "D = [" << D_top << "," << D_bottom << "]"
            << std::endl;
}

class Bot
{
  public:
    Bot(const int x, const int y, const int z, const unsigned int r);
    unsigned int m_distance(Bot& b) const;
    bool in_range(Bot& b) const;
    bool intersect(Bot& b) const;
    int get_x() const { return x; }
    int get_y() const { return y; }
    int get_z() const { return z; }
    unsigned int get_r() const { return r; }
    Irreg_Octo get_otco() const { return octo; }

    void print() const;

  private:
    const int x;
    const int y;
    const int z;
    const unsigned int r;
    const Irreg_Octo octo;
};

Bot::Bot(const int x, const int y,
         const int z, const unsigned int r) :
    x(x)
  , y(y)
  , z(z)
  , r(r)
  , octo(x, y, z, r)
{}

unsigned int Bot::m_distance(Bot& b) const
{
  return m_d(x, y, z, b.get_x(), b.get_y(), b.get_z());
}

bool Bot::in_range(Bot& b) const
{
  return m_distance(b) <= r;
}

bool Bot::intersect(Bot& b) const
{
  return m_distance(b) <= (r + b.get_r());
}

void Bot::print() const
{
  std::cout << "[" << x << "," << y << "," << z << "|" << r << "]" << std::endl;
  return;
}
         
std::vector<Bot> parse_input(const std::string& path)
{
  std::string line;
  std::ifstream data_file(path);
  std::vector<Bot> out_vector;

  while (getline(data_file, line))
  {
    int bra = line.find("<", 0);
    int comma_1 = line.find(",", 0);
    int comma_2 = line.find(",", comma_1 + 1);
    int ket = line.find(">", 0);
    int eq = line.find("=", ket);

    const int x = std::stoi(line.substr(bra + 1, comma_1 - bra + 1));
    const int y = std::stoi(line.substr(comma_1 + 1, comma_2 - comma_1 + 1));
    const int z = std::stoi(line.substr(comma_2 + 1, comma_2 - ket + 1));
    const int r = std::stoi(line.substr(eq + 1, 25));

    out_vector.emplace_back(x, y, z, r);
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

  std::vector<Bot> bots = parse_input(path);
  size_t best_index = 0;
  unsigned int best_radius = 0;
  const size_t n_bots = bots.size();
  for (unsigned int i = 0; i < n_bots; ++i)
  {
    if (bots[i].get_r() > best_radius) 
    {
      best_index = i;
      best_radius = bots[i].get_r();
    }
  }
  std::cout << n_bots << std::endl;
  Bot T800 = bots[best_index];

  unsigned int count = 0;
  for (unsigned int i = 0; i < n_bots; ++i)
    if (T800.in_range(bots[i])) 
      ++count;

  // part 1
  std::cout << count << std::endl;

  // part 2
  // construct the intersection graph
  /*dist_mat D(n_bots, std::vector<bool>(n_bots));
  for (size_t i = 0; i < n_bots; ++i)
  {
    for (size_t j = i; j < n_bots; ++j)
    {
      D[i][j] = bots[i].intersect(bots[j]);
      D[j][i] = bots[i].intersect(bots[j]);
    }
  }*/

  // This is filthy and can fail if the balls are in the wrong order.
  // Really we should calculate which balls form largest complete subgraph of
  // the intersection graph above and then iterate over that.
  // But by blind luck we don't have to do that in this case
  Irreg_Octo soul = bots[0].get_otco();
  for (size_t i = 1; i < bots.size(); ++i)
    if (!soul.intersect(bots[i].get_otco()))
      std::cout << "No intersection" << std::endl;

  unsigned int x = (soul.B_top + soul.C_top) / 2;
  unsigned int y = (soul.B_top + soul.D_top) / 2;
  unsigned int z = (soul.C_top + soul.D_top) / 2;
  std::cout << "x = " << x
            << " y = " << y
            << " z = " << z << std::endl
            << "x + y + z  = " << x + y + z
            << std::endl;

  return 0;
}
