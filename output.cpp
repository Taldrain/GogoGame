#include <iostream>
#include "output.h"
using namespace std;

void print_letter()
{
    cout << "   ";
    for(char i = 65; i < 84; i++)
        cout << i << " ";
    cout << endl;
}

void print_number(int i)
{
        if (i < 9)
            cout << " " << (i+1) << " ";
        else
            cout << (i+1) << " ";
}

void output(char goban[][19], int taille)
{
    print_letter();

    for(int i = 0; i < taille; i++)
    {
        print_number(i);
        for(int j = 0; j < taille; j++)
            cout << goban[i][j] << " ";
        print_number(i);
        cout << endl;
    }

    print_letter();
}
