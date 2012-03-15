#include <iostream>
#include "init.h"
#include "output.h"
#include "game.h"
using namespace std;


int main()
{
    int const SIZE(19);
    char goban[SIZE][SIZE];
    cout << "Taille du goban: " << SIZE << "x" << SIZE << "\n\n";

    init(goban, SIZE);
    loop(goban, SIZE);

    return 0;
}
