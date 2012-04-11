#ifndef GOBAN_H_INCLUDED
#define GOBAN_H_INCLUDED

class Goban
{
    public:
        Goban(bool u1, bool u2);
        Goban();

        void game();

        void print();

    private:
        bool red1, red2;
        bool pass1, pass2;
        char goban[19][19];
        int taille;

        void init_case();
        void spec_case();

        void print_letter();
        void print_number(int i);

        //output n joueur, pass or not
        void turn_h(bool black);
        //tofix must by an other program
        void turn_m(bool black);

        void check_end();
};

#endif
