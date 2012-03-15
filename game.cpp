#include <iostream>
#include "output.h"
#include "game.h"
using namespace std;


void loop(char goban[][19], int taille)
{
    char user1_X('0'), user2_X('0');
    int user1_Y(1), user2_Y(1);

    bool user1(true);

    while((user1_X != 'Z' ||  user2_X != 'Z') &&
        (user1_Y != 0 || user2_Y != 0))
    {
        output(goban, taille);
        cout << "\n\n";

        if (user1)
        {
            cout << "Joueur 1 entrez les coordonnees de votre pion:\n";
            cin >> user1_X >> user1_Y;
            if (user1_X != 'Z' && user1_Y != 0)
            {
                goban[(user1_Y)-3/* ??? */][(user1_X)-27/* pareil */] = 'N';
                cout << "\nNouveau pion aux coordonnees " << user1_X << "x";
                cout << user1_Y << endl;
            }
            else
                cout << "Le joueur 1 passe son tour\n";
        }
        else
        {
            cout << "Joueur 2 entrez les coordonnees de votre pion:\n";
            cin >> user2_X >> user2_Y;
            if (user2_X != 'Z' && user2_Y != 0)
            {
                goban[(user2_Y)-3][(user2_X)-27] = 'B';
                cout << "\nNouveau pion aux coordonnees " << user2_X << "x";
                cout << user2_Y << endl;
            }
            else
                cout << "Le joueur 2 passe son tour\n";
        }
        user1 = !user1;
    }
    cout << "Fin de la partie\n";
}

