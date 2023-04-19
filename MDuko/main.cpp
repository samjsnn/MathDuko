#include <vector>
#include <string>
#include <unordered_set>
#include <stack>
#include <algorithm>
#include <iostream>
#include <chrono>
#include <unordered_map>
#include <stack>
#include <vector>
#include <random>
#include <set>
#include <string>
#include <sstream>
#include <bitset>
#include <cstring>
#ifndef INT_MAX
#define INT_MAX 2147483647
#endif // !INT_MAX
#ifndef INT_MIN
#define INT_MIN -2147483648
#endif // !INT_MAX

// Device for generating random numbers
std::random_device dev;

// Function for generating a random integer within the specified number range
int Randint(int low, int high)
{
    // Random number generator using the Mersenne Twister algorithm
    std::mt19937 rng(dev());

    std::uniform_int_distribution<std::mt19937::result_type> dist(low, high);

    return dist(rng);
}
// Enum for different number bases
enum class NumberBase
{
    HEX = 0,
    BIN = 1,
    DEC = 2,
};
// Function for getting a random operator from a string of operators
char GetOps(std::string &ops, bool special = true)
{
    char op = 0;
    while (true)
    {
        op = ops[Randint(0, ops.length() - 1)];
        if (special)
        {
            return op;
        }

        if (op != '/' && op != '%')
        {
            return op;
        }
    }
}
// Function for printing an integer in the specified number base
std::string PrintFormat(NumberBase n, int str)
{
    std::stringstream sstream;
    if (n == NumberBase::DEC)
    {
        sstream << str;
    }

    if (n == NumberBase::HEX)
    {
        if (str < 0)
        {
            sstream << '-' << std::hex << abs(str);
        }
        else
        {
            sstream << std::hex << str;
        }
    }
    if (n == NumberBase::BIN)
    {
        if (str)
        {
            if (str < 0)
            {
                sstream << '-';
            }
            sstream << std::bitset<32>(abs(str)).to_string().substr(31 - log2(abs(str)));
        }
        else
        {
            sstream << 0;
        }
    }
    std::string result = sstream.str();
    return result;
}
// Function for operator priority
static int GetOperatorPriority(int n)
{
    switch (n)
    {
    case '*':
        return 1;
    case '/':
        return 1;
    case '%':
        return 1;
    case '+':
        return 2;
    case '-':
        return 2;
    case '&':
        return 3;
    case '^':
        return 4;
    case '|':
        return 5;
    default:
        return 0;
    }
}

static int EvalOP(int n, int a, int b)
{
    switch (n)
    {
    case '*':
        return a * b;
    case '/':
        return a / b;
    case '%':
        return a % b;
    case '+':
        return a + b;
    case '-':
        return a - b;
    case '&':
        return a & b;
    case '^':
        return a ^ b;
    case '|':
        return a | b;
    default:
        return 0;
    }
}

struct Member
{
    int value;
    bool isOp;
};
// Template struct that implements a stack with a fixed size.
template <typename T>
struct FixedSizeStack
{
    FixedSizeStack(size_t size) { c.resize(size + 1); }
    FixedSizeStack(){};
    // resizes the stack to the given size
    void resize(size_t size) { c.resize(size + 1); }
    // clears the stack by setting all elements to zero and resetting the top index
    void clear()
    {
        memset(c.data(), 0, c.size() * sizeof(T));
        top_int = -1;
    }
    // reverses the order of elements in the stack
    void reverse() { std::reverse(c.begin(), c.begin() + top_int + 1); }
    // returns the size of the stack (the top index plus one)
    size_t size() { return top_int + 1; }
    // returns a reference to the top element of the stack
    T &top() { return c[top_int]; }
    // removes the top element from the stack
    void pop()
    {
        c[top_int] = {};
        top_int--;
    }
    // adds a new element to the top of the stack
    void push(T a) { c[++top_int] = a; }

private:
    int top_int = -1;
    std::vector<T> c;
};

class Parser
{
public:
    Parser(size_t size) : output(size), operators(size), number_stack(size) {}
    Parser() {}
    // Method to push a number onto the output stack
    void PushNumber(int n)
    {
        output.push({n, false});
    }
    // Method to push an operator onto the operator stack
    void PushOperator(char op)
    {
        int new_op_priority = GetOperatorPriority(op);
        if (operators.size())
        {
            if (new_op_priority >= GetOperatorPriority(operators.top()))
            {
                output.push({operators.top(), true});
                operators.pop();
                operators.push(op);
                return;
            }
        }
        operators.push(op);
    }
    // Method to move all operators from the operator stack to the output stack
    void Flush()
    {
        while (operators.size() != 0)
        {
            output.push({operators.top(), true});
            operators.pop();
        }
    }

    void clear()
    {
        output.clear();
        operators.clear();
        number_stack.clear();
    }
    // Method to compute the result of the expression stored in the output stack
    int Compute()
    {
        auto &reversed_stack = output;
        output.reverse();

        while (reversed_stack.size() != 0)
        {
            auto &top = reversed_stack.top();
            if (top.isOp)
            {
                int result = 0;
                int a = number_stack.top();
                number_stack.pop();
                int b = number_stack.top();
                number_stack.pop();
                if ((top.value == '/' || top.value == '%') && a == 0)
                {
                    return INT_MIN;
                }
                else
                {
                    result = EvalOP(top.value, b, a);
                }
                number_stack.push(result);
            }
            else
            {
                number_stack.push(top.value);
            }
            reversed_stack.pop();
        }

        return number_stack.top();
    }

    void ResizeStacks(size_t size)
    {
        output.resize(size);
        operators.resize(size);
        number_stack.resize(size);
    }

private:
    FixedSizeStack<Member> output;
    FixedSizeStack<char> operators;
    FixedSizeStack<int> number_stack;
};

class Expression
{
    Parser p;

public:
    inline static std::string operators = "+-*/%&|^"; // Valid operators

    Expression(size_t size) { p.ResizeStacks(size); }
    Expression() {}
    std::tuple<bool, int> eval(std::vector<int> &e)
    {
        int result = 0;
        p.clear();
        for (int i = 1; i < e.size() - 2; i++)
        {
            if (e[i] == INT_MIN)
            {
                break;
            }
            if (i % 2 != 0)
            {
                p.PushNumber(e[i]);
            }
            else
            {
                p.PushOperator(e[i]);
            }
        }
        p.Flush();
        result = p.Compute();
        return {result == e[e.size() - 1], result};
    }

    std::string ops() { return operators; };
};

struct Vec_Hash
{
    std::size_t operator()(std::vector<int> const &vec) const
    {
        std::size_t seed = vec.size();
        for (auto x : vec)
        {
            x = ((x >> 16) ^ x) * 0x45d9f3b;
            x = ((x >> 16) ^ x) * 0x45d9f3b;
            x = (x >> 16) ^ x;
            seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }
        return seed;
    }
};

// Solutions class
class Solutions
{
    // This vector will store the solutions found so far
    std::vector<std::unordered_set<std::vector<int>, Vec_Hash>> solutions;
    // This vector will store the contiguous column for a given row during the recursive search
    std::vector<int> contiguos_col;

    int lowest_number;
    int highest_number;
    // Private functions to generate / maintain solution sets
    Expression expr;
    // Recursive function that searches for solutions starting from a given row
    bool solve(std::vector<std::vector<int>> &board, int y)
    {
        int size = (board.size() - 2) / 2;
        bool result = false;
        // Iterate over the cells in the current row
        for (int x = 0; x < size; x++)
        {
            // Get a reference to the current cell and its value
            auto &game_cell = board[y * 2 + 1][x * 2 + 1];
            auto game_cell_val = board[y * 2 + 1][x * 2 + 1];
            // Try all possible numbers for the current cell
            for (int i = 0; i <= highest_number; i++)
            {
                // Evaluate the arithmetic expression for the current row
                auto [e, v] = expr.eval(board[y * 2 + 1]);
                // If the expression is valid
                if (e)
                {
                    // If last row is reached
                    if (y == size - 1)
                    {
                        // Compute the contiguous column for the current column
                        for (int i = 0; i < board.size(); i++)
                        {
                            contiguos_col[i] = board[i][x * 2 + 1];
                        }
                        auto [c_e, c_v] = expr.eval(contiguos_col);
                        // If the contiguous column expression is valid
                        if (c_e)
                        {
                            // Add the current row to the solution set for the current level
                            solutions[y].insert(board[y * 2 + 1]);
                            result = true;
                        }
                    }
                    // If last row not reached yet
                    else
                    {
                        // Search for solutions in the next row
                        if (solve(board, y + 1))
                        {
                            // Add the current row to the solution set for the current level
                            solutions[y].insert(board[y * 2 + 1]);
                            result = true;
                        }
                    }
                }
                // Try the next number for the current cell
                game_cell = i;
            }
        }
        return result;
    }

public:
    Solutions() = default;
    // Create solutions valid set from initial board
    // which only has operators and results

    void build(std::vector<std::vector<int>> &board)
    {
        int size = (board.size() - 2) / 2;
        expr = Expression(board.size());
        contiguos_col.resize(board.size());
        solutions.resize(size);
        solve(board, 0);
    }
    // This function generates a hint for the player by selecting a random cell
    // that is not already filled and fills it with a valid value
    bool hint(std::vector<std::vector<int>> &board, std::vector<std::vector<bool>> &valid)
    {
        std::vector<int> values;
        int size = (board.size() - 2) / 2;
        // looping over rows to find ones with no value
        for (int y = 0; y < size; y++)
        {
            bool notfull = false;
            for (int x = 0; x < size; x++)
            {
                notfull |= !valid[y][x];
            }
            if (notfull)
            {
                values.push_back(y);
            }
        }
        // if there are rows that are not full
        if (values.size())
        { // select random row
            int y = values[Randint(0, values.size() - 1)];
            // Create a pair to store the best column and its score
            std::pair<int, std::vector<int>> pair{-1, {}};
            // Loop over the columns in the selected row to find the one with the highest score
            for (auto &column : solutions[y])
            {
                std::pair<int, std::vector<int>> c_pair{0, column};

                for (int i = 0; i < size; i++)
                {
                    if (board[y * 2 + 1][i * 2 + 1] == column[i * 2 + 1] || board[y * 2 + 1][i * 2 + 1] == INT_MIN)
                    {
                        c_pair.first++;
                    }
                }

                if (pair.first < c_pair.first)
                {
                    pair = c_pair;
                }
            }
            auto &sol = pair.second;
            // Create a vector to store the columns that are not filled yet
            std::vector<int> x_values;
            int size = (board.size() - 2) / 2;
            // Loop over the columns in the selected row to find the ones that are not filled yet
            for (int x = 0; x < size; x++)
            {
                if (!valid[y][x])
                {
                    x_values.push_back(x);
                }
            }
            // Select a random column from the ones that are not filled yet
            int x = x_values[Randint(0, x_values.size() - 1)];
            // Fill the selected cell with the valid value from the best column
            board[y * 2 + 1][x * 2 + 1] = sol[x * 2 + 1];
            valid[y][x] = true;
            return true;
        }
        return false;
    }

    bool update(std::vector<std::vector<int>> &b, std::vector<std::vector<bool>> &vs, int row, int col)
    {
        int size = (b.size() - 2) / 2;
        // Loop through solutions for the current row
        for (auto &column : solutions[(row - 2) / 2])
        {
            // iterate over each solution for given row
            bool result = true;
            for (int i = 0; i < size; i++)
            {
                result &= (b[row][i * 2 + 1] == column[i * 2 + 1] || b[row][i * 2 + 1] == INT_MIN);
            }
            if (result)
            {
                return result;
            }
        }
        return false;
    }

    void SetConstraints(int low, int high)
    {
        lowest_number = low;
        highest_number = high;
    }
};

class Board
{
public:
    // Create, display initial user board and generate solutions set
    void Create(std::string &operators)
    {
        const int actual_size = size * 2 + 2;

        board.resize(actual_size);
        for (auto &row : board)
        {
            row.resize(actual_size);
            std::fill(row.begin(), row.end(), 32);
        }

        valid_cell.resize(size);
        for (auto &row : valid_cell)
        {
            row.resize(size);
        }

        // Operators Loop
        for (int y = 0; y <= size; y++)
        {
            for (int x = 0; x <= size; x++)
            {
                if (x && !y)
                {
                    board[y * 2][x * 2 - 1] = x;
                }
                if (y && !x)
                {
                    board[y * 2 - 1][x * 2] = y;
                }

                if (x && y)
                {
                    board[y * 2 - 1][x * 2] = GetOps(operators); // x
                    board[y * 2][x * 2 - 1] = GetOps(operators); // y
                }

                if (y && x == size)
                {
                    board[y * 2 - 1][x * 2] = '=';
                    board[y * 2 - 1][x * 2 + 1] = 0;
                }

                if (x && y == size)
                {
                    board[y * 2][x * 2 - 1] = '=';
                    board[y * 2 + 1][x * 2 - 1] = 0;
                }
            }
        }
        // iterate over each cell
        // this block of code removes 0's to prevent arithmetic errors if / or %
        for (int y = 0; y <= size; y++)
        {
            for (int x = 0; x <= size; x++)
            {
                auto &game_cell = board[y * 2 + 1][x * 2 + 1];
                int fix_lowest_number = lowest_number;
                if (!lowest_number)
                {
                    if (x)
                    {
                        auto &previous_cell = board[y * 2 + 1][x * 2];
                        if (previous_cell == '/' || previous_cell == '%')
                        {
                            fix_lowest_number = 1;
                        }
                    }

                    if (y)
                    {
                        auto &previous_cell = board[y * 2][x * 2 + 1];
                        if (previous_cell == '/' || previous_cell == '%')
                        {
                            fix_lowest_number = 1;
                        }
                    }
                }
                game_cell = fix_lowest_number;
            }
        }

        auto board_copy = board;

        // Solutions Loop
        for (int y = 0; y < size; y++)
        {
            std::unordered_map<int, std::pair<int, std::vector<int>>> occurences_x;
            for (int x = 0; x < size; x++)
            {

                auto &game_cell = board_copy[y * 2 + 1][x * 2 + 1];
                auto game_cell_val = board_copy[y * 2 + 1][x * 2 + 1];

                for (int i = game_cell_val + 1; i <= highest_number; i++)
                {
                    auto [e, v] = exp.eval(board_copy[y * 2 + 1]);
                    if (v == INT_MIN)
                    {
                    }

                    if (occurences_x.count(v) > 0)
                    {
                        occurences_x[v].first++;
                    }
                    else
                    {
                        occurences_x[v].first = 1;
                        occurences_x[v].second = board_copy[y * 2 + 1];
                    }

                    game_cell = i;
                }

                std::pair<int, std::pair<int, std::vector<int>>> pair = *occurences_x.begin();
                for (auto &occurence : occurences_x)
                {
                    if (occurence.second.first < pair.second.first)
                    {
                        pair = occurence;
                    }
                }

                board[y * 2 + 1][size * 2 + 1] = pair.first;
                board_copy[y * 2 + 1] = pair.second.second;
                board_copy[y * 2 + 1][size * 2 + 1] = pair.first;
            }
        }

        board = board_copy;

        for (int x = 0; x <= size; x++)
        {
            auto p = Build_exp(x * 2 + 1);
            auto [e, v] = exp.eval(p);
            board[size * 2 + 1][x * 2 + 1] = v;
            board_copy[size * 2 + 1][x * 2 + 1] = v;
        }

        board_copy = board;

        // Removes temporary solutions

        board[actual_size - 1][actual_size - 1] = ' ';

        solutions.build(board);

        for (int y = 0; y < size; y++)
        {
            for (int x = 0; x < size; x++)
            {
                auto &game_cell = board[y * 2 + 1][x * 2 + 1];

                game_cell = INT_MIN;
            }
        }
    }

    Board(int size, int lowest_number, int highest_number, bool show_cell_validity, std::string operators) : size(size), lowest_number(lowest_number), show_cell_validity(show_cell_validity), highest_number(highest_number), exp(size * 2 + 2)
    {
        solutions.SetConstraints(lowest_number, highest_number);
        // recording start time for final score calculation
        using namespace std::chrono;
        auto now = time_point_cast<seconds>(system_clock::now());
        using sys_milliseconds = decltype(now);
        start_time = now.time_since_epoch().count();
        Create(operators);
    }

    std::string ops() { return exp.ops(); }
    // Check value in range, check results for completed rows and cols
    // Update valid and potential in solutions
    std::tuple<bool, int, int> Set(int row, int column, int value)
    {
        // Check if value is in range and row/column indices are valid
        if (value > highest_number || value < lowest_number || row > size || row <= 0 || column > size || column <= 0)
        {
            return {false, row, column};
        }
        // Set value of cell on game board
        board[(row - 1) * 2 + 1][(column - 1) * 2 + 1] = value;
        // Update solutions and valid cell arrays
        auto result = solutions.update(board, valid_cell, (row - 1) * 2 + 1, (column - 1) * 2 + 1);
        valid_cell[row - 1][column - 1] = result;
        // Return tuple with boolean indicating whether cell was set successfully and the row and column indices
        return {result, row, column};
    }
    // Check if all cells are valid and game is finished
    bool Finished()
    {
        int result = true;
        for (auto &row : valid_cell)
        {
            for (auto colum : row)
            {
                result &= colum;
            }
        }
        // If all cells are valid, calculate score and print game board and score
        if (result)
        {
            using namespace std::chrono;
            auto now = time_point_cast<seconds>(system_clock::now());
            using sys_milliseconds = decltype(now);
            int end = now.time_since_epoch().count();
            int time = (end - start_time);
            if (!time)
            {
                score = 0;
            }
            else
            {
                score = (1000 / time) * (highest_number - lowest_number) * size + 10000 / (hint);
            }
            if (!show_cell_validity)
            {
                score *= 1.5;
            }
            std::cout << *this;
            std::cout << "\nYou won with a score of " << score << "! \n";
        }

        return result;
    }
    bool Hint()
    {
        hint++;
        return solutions.hint(board, valid_cell);
    }
    void SetOutputBase(NumberBase n) { OutputBase = n; }
    // Print board
    friend std::ostream &operator<<(std::ostream &os, const Board &b)
    {
        system("cls");
        const int actual_size = b.size * 2 + 2;
        for (int y = 0; y < actual_size; y++)
        {
            for (int x = 0; x < actual_size; x++)
            {
                auto &cell = b.board[y][x];
                if ((x % 2) && (y % 2))
                {
                    if (!(x >= actual_size - 1 && y >= actual_size - 1))
                    {
                        if (cell != INT_MIN)
                        {
                            if (!(x >= actual_size - 1 || y >= actual_size - 1))
                            {
                                if (b.valid_cell[(y - 1) / 2][(x - 1) / 2])
                                {
                                    if (b.show_cell_validity)
                                    {
                                        os << "\033[38;2;0;255;0m";
                                        os << PrintFormat(b.OutputBase, cell);
                                        os << "\033[38;5;255m";
                                    }
                                    else
                                    {
                                        os << PrintFormat(b.OutputBase, cell);
                                    }
                                }
                                else
                                {
                                    if (b.show_cell_validity)
                                    {
                                        os << "\033[38;2;255;0;0m";
                                        os << PrintFormat(b.OutputBase, cell);
                                        os << "\033[38;5;255m";
                                    }
                                    else
                                    {
                                        os << PrintFormat(b.OutputBase, cell);
                                    }
                                }
                            }
                            else
                            {
                                os << PrintFormat(b.OutputBase, cell);
                            }
                        }
                        else
                        {
                            if (!(x >= actual_size - 1 || y >= actual_size - 1))
                            {
                                os << '#';
                            }
                        }
                    }
                }
                else
                {
                    if ((!x || !y) && cell != ' ')
                    {
                        if (x != 2 && y != 2)
                        {
                            os << cell;
                        }
                    }
                    else
                    {

                        os << (char)cell;
                    }
                }

                os << " ";
            }
            os << std::endl;
        }

        return os;
    }

private:
    std::vector<std::vector<int>> board;       // Contains values, ops and results
    std::vector<std::vector<bool>> valid_cell; // Flag unused cells

    Expression exp;
    Solutions solutions;

    int lowest_number = 0;
    int highest_number = 0;

    int size = 0;
    int valid_cells = 0;
    int hint = 1;
    bool show_cell_validity = true;

    int score = 0;

    int start_time = 0;
    // Build row expression from column of board

    NumberBase OutputBase = NumberBase::DEC;

    std::vector<int> Build_exp(int col)
    {
        std::vector<int> contiguos_col;
        for (auto &cell_row : board)
        {
            contiguos_col.push_back(cell_row[col]);
        }

        return contiguos_col;
    }
};

template <typename T>
T upper(T txt)
{
    if constexpr (std::is_same<T, char>::value)
    {
        return std::toupper(txt);
    }
    return txt;
}
// check input
template <typename T, typename... Args>
void Input(std::string txt, T &var, Args... arg)
{
    while (true)
    {
        std::cin.clear();
        std::cout << txt;
        std::cin >> var;

        bool result = false;
        std::vector<bool> b{};
        ((b.push_back(upper(var) == arg)), ...); // fold expression
        for (auto a : b)
        {
            result |= a;
        }
        if (result || !b.size() && !std::cin.fail())
        {
            return;
        }
        std::cin.clear();
        std::cin.ignore(INT_MAX, '\n'); // ignore last input
    }
}

int main()
{
    while (true)
    {
        system("cls");
        std::cout << "Welcome to mathduko!\n\n";

        int size, low, high;
        char show_cell_validity;
        std::string operators;
        while (true)
        {
            Input("Please insert game board size: ", size);
            if (size > 0)
            {
                break;
            }
        }
        Input("Please insert lower number boundary: ", low);
        while (true)
        {
            Input("Please insert upper number boundary: ", high);
            if (high > low)
            {
                break;
            }
        }
        // Input("Should it show if a cell content is right[n, y]: ", show_cell_validity, 'Y', 'N');
        show_cell_validity = std::tolower(show_cell_validity);
        bool op_loop = true;

        while (op_loop)
        {
            Input("Insert operators you want to use (+-*/%&|^) or press D to use all: ", operators);
            if (std::toupper(operators[0]) == 'D')
            {
                operators = "+-*/%&|^";
                op_loop = false;
            }
            for (auto &charater : operators)
            {
                bool valid_character = false;
                for (auto &valid_char : Expression::operators)
                {
                    valid_character |= (charater == valid_char);
                }
                if (valid_character)
                {
                    op_loop = false;
                    break;
                }
            }
            std::cout << "\n";
        }

        Board game(size, low, high, show_cell_validity - 110, operators);
        while (!game.Finished())
        {
            std::cout << game << "\n";

            char mode;
            Input("[I: insert value; H: hint; D: display mode; Q: quit]\n", mode, 'I', 'H', 'D', 'Q');
            if (std::toupper(mode) == 'I')
            {
                int row, col, value;

                Input("Row: ", row);
                Input("Columns: ", col);
                Input("Value: ", value);
                game.Set(row, col, value);
            }

            if (std::toupper(mode) == 'H')
            {
                game.Hint();
            }

            if (std::toupper(mode) == 'D')
            {
                char d_mode;
                Input("[D: decimal; H: hexadecimal; B: binary]\n", d_mode, 'D', 'H', 'B');
                switch (std::toupper(d_mode))
                {
                case 'D':
                    game.SetOutputBase(NumberBase::DEC);
                    break;
                case 'H':
                    game.SetOutputBase(NumberBase::HEX);
                    break;
                case 'B':
                    game.SetOutputBase(NumberBase::BIN);
                    break;
                default:
                    break;
                }
            }

            if (std::toupper(mode) == 'Q')
            {
                break;
            }
        }

        char replay;
        Input("Do you want to play again[n, y]\n", replay, 'Y', 'N');
        if (replay == 'n')
        {
            break;
        }
        system("cls");
    }

    return 0;
}