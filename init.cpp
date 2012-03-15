#include "init.h"

void spec_case(char goban[][19])
{
    goban[3][3] = '+';
    goban[9][3] = '+';
    goban[15][3] = '+';
    goban[3][9] = '+';
    goban[3][15] = '+';
    goban[9][9] = '+';
    goban[15][9] = '+';
    goban[9][15] = '+';
    goban[15][15] = '+';
}

void init(char goban[][19], int taille)
{
    for(int i = 0; i < taille; i++)
        for(int j = 0; j < taille; j++)
            goban[i][j] = '.';
    spec_case(goban);
}
