#include <iostream>
#include "init.h"
#include "output.h"
using namespace std;


int main()
{
    int const taille(19);
    char goban[taille][taille];
    cout << "Taille du goban: " << taille << "x" << taille << "\n\n";

    init(goban, taille);
    output(goban, taille);

    return 0;
}
