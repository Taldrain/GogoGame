#include <iostream>
#include <cstdlib>
#include "goban.h"
using namespace std;


Goban::Goban(bool u1, bool u2) :
    red1(u1), red2(u2)
{
    taille = 19;
    pass1 = pass2 = false;
    init_case();
}

Goban::Goban() :
    red1(true), red2(true)
{
    taille = 19;
    pass1 = pass2 = false;
    init_case();
}

void Goban::game()
{
    while(true)
    {
        print();
        if (red1)
            turn_h(true);
         else
             turn_m(true);
        print();

        if (red2)
            turn_h(false);
        else
            turn_m(true);
    }
}


void Goban::print()
{
    print_letter();
    for(int i=0; i<taille; i++)
    {
        print_number(i);
        for(int j=0; j<taille; j++)
            cout << goban[i][j] << " ";
        print_number(i);
        cout << endl;
    }
    print_letter();
}

void Goban::init_case()
{
    for(int i=0; i<taille; i++)
        for(int j=0; j<taille; j++)
            goban[i][j] = '.';
    spec_case();
}

void Goban::spec_case()
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

void Goban::print_letter()
{
    cout << "   ";
    for(char i=65; i<84; i++)
        cout << i << " ";
    cout << endl;
}

void Goban::print_number(int i)
{
    if (i<9)
        cout << " ";
    cout << ++i << " ";
}

void Goban::turn_h(bool black)
{
    char in_X;
    int in_Y;
    if (black)
    {
        cout << "Joueur 1, entrez les coordonnées de votre pion\n";
        cin >> in_X >> in_Y;
        if ((in_X == 'Z') && (in_Y == 0))
        {
            pass1 = true;
            cout << "Joueur 1 passe son tour";
        }
        else
        {
            goban[(in_Y) - 3][(in_X) - 27] = 'B';
            cout << "Nouveau pion aux coordonnées " << in_X << "x" << in_Y;
            if (pass2)
                pass2 = !pass2;
        }
    }
    else
    {
        cout << "Joueur 2, entrez les coordonnées de votre pion\n";
        cin >> in_X >> in_Y;
        if ((in_X == 'Z') && (in_Y == 0))
        {
            pass2 = true;
            cout << "Joueur 2 passe son tour";
        }
        else
        {
            goban[(in_Y) - 3][(in_X) - 27] = 'W';
            cout << "Nouveau pion aux coordonnées " << in_X << "x" << in_Y;
            if (pass1)
                pass1 = !pass1;
        }
    }
    cout << endl;
    check_end();
}

void Goban::turn_m(bool black)
{
    //TODO
}

void Goban::check_end()
{
    if ((pass1 == true) && (pass1 == pass2))
    {
        cout << "Les deux joueurs ont passé successivement, fin de la partie\n";
        exit(0);
    }
}
