# This is a collection of blunders of GNU Go in Stefan Mertin's private
# 13x13 - Computer Go tournament in 2001 (see
# http://www.geocities.com/comp_go/13x13.html). I think they should be
# quite useful as tests. 13x13-positions are specific as problems, and
# they are quickly evaluated both by GNU Go and by us when maintaining.
# (And of course, these are games against some opponents we would like
# to beat :-) )
# I also added a few test cases where GNU Go played right in the game,
# but went wrong in a replay by a newer version (3.14 / 3.15).
# -Arend
# Most of the variations in the sgf-files are due to Stefan Mertin.

#CATEGORY=STRATEGY
#DESCRIPTION=M2 has the huge follow up of rescueing H3, not seen by GNU Go.
loadsgf games/mertin13x13/goliath-gnugo1.B+11.sgf 22
1 reg_genmove white
#? [M2]

# E10 is a little better but the difference might not be worth regressing
# for now. 
loadsgf games/mertin13x13/goliath-gnugo1.B+11.sgf 36
2 reg_genmove white
#? [E9|E10]

loadsgf games/mertin13x13/goliath-gnugo1.B+11.sgf 58
3 restricted_genmove white E2 E5
#? [E5]

#CATEGORY=OWL_TUNING
# Of course it is nice if you can get away with J11 as GNU Go did in the
# game, but I think L9 is urgent.
loadsgf games/mertin13x13/goliath-gnugo2.W+9.sgf 30
4 reg_genmove white
#? [L9]*

#CATEGORY=ENDGAME_TUNING
loadsgf games/mertin13x13/goliath-gnugo3.W+0.sgf 42
5 reg_genmove white
#? [N10|N9|M1]

#CATEGORY=BLUNDER
# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/goliath-gnugo3.W+0.sgf 44
6 reg_genmove white
#? [M8]

#CATEGORY=BLUNDER
loadsgf games/mertin13x13/goliath-gnugo3.W+0.sgf 48
7 reg_genmove white
#? [L7|M7|M6]*

#CATEGORY=ENDGAME_TUNING
loadsgf games/mertin13x13/goliath-gnugo3.W+0.sgf 54
8 restricted_genmove white K6 K7 J8 H8
#? [J8]*

#CATEGORY=BLUNDER
loadsgf games/mertin13x13/goliath-gnugo3.W+0.sgf 72
9 reg_genmove white
#? [C4]*

#CATEGORY=STRATEGY
loadsgf games/mertin13x13/gointellect-gnugo1.B+14.sgf 12
10 reg_genmove white
#? [C2]

#CATEGORY=TERRITORIAL_EVALUATION
loadsgf games/mertin13x13/gointellect-gnugo1.B+14.sgf 20
11 reg_genmove white
#? [L11|H7|G7|F6]*

#CATEGORY=TERRITORIAL_EVALUATION
loadsgf games/mertin13x13/gointellect-gnugo1.B+14.sgf 24
12 reg_genmove white
#? [G7|F7|G8]

##CATEGORY=STRATEGY
#loadsgf games/mertin13x13/gointellect-gnugo2.W+8.sgf 22
#13 reg_genmove white
##? [!G7]*

#CATEGORY=STRATEGY
# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/gointellect-gnugo2.W+8.sgf 28
14 reg_genmove white
#? [A10|E8|E6|H6]*

#CATEGORY=STRATEGY
# New failure (comparing 3.1.15 and 3.0.0)
# See also owl:262.
loadsgf games/mertin13x13/gointellect-gnugo2.W+8.sgf 32
15 reg_genmove white
#? [C7|B7|C6|B6|B5|C5]

#CATEGORY=STRATEGY
loadsgf games/mertin13x13/gointellect-gnugo3.B+1.sgf 24
16 reg_genmove white
#? [C6|C5]

# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/gointellect-gnugo3.B+1.sgf 32
17 reg_genmove white
#? [L5|L3]*

#CATEGORY=OWL_TUNING
# This got broke between 3.0.0 and 3.1.14.
loadsgf games/mertin13x13/gointellect-gnugo3.B+1.sgf 42
18 owl_attack L11
#? [0]

loadsgf games/mertin13x13/gointellect-gnugo3.B+1.sgf 48
19 restricted_genmove white M8 N3
#? [M8]

#CATEGORY=ENDGAME_TUNING
loadsgf games/mertin13x13/gointellect-gnugo3.B+1.sgf 60
20 reg_genmove white
#? [M8]

#CATEGORY=ENDGAME_TUNING
loadsgf games/mertin13x13/gointellect-gnugo3.B+1.sgf 66
21 reg_genmove white
#? [N8]

#CATEGORY=BLUNDER
loadsgf games/mertin13x13/gointellect-gnugo3.B+1.sgf 94
22 reg_genmove white
#? [C9]

# E12 is too small.
loadsgf games/mertin13x13/katsunari-gnugo3.B+14.sgf 16
23 reg_genmove white
#? [B7|M10|H2]*

# At least try to connect.
loadsgf games/mertin13x13/katsunari-gnugo3.B+14.sgf 34
24 reg_genmove white
#? [G5|H5|G4]*

loadsgf games/mertin13x13/katsunari-gnugo3.B+14.sgf 64
25 reg_genmove white
#? [J5]

loadsgf games/mertin13x13/katsunari-gnugo3.B+14.sgf 66
26 reg_genmove white
#? [J5]

#CATEGORY=FUSEKI_STRATEGY
# Too early for 3-3 invasion at L11 with both UL and LR corner open
loadsgf games/mertin13x13/katsunari-gnugo1.W+45.sgf 10
27 reg_genmove white
#? [C11|E3|E4]

loadsgf games/mertin13x13/katsunari-gnugo1.W+45.sgf 12
28 reg_genmove white
#? [L10]*

loadsgf games/mertin13x13/katsunari-gnugo1.W+45.sgf 20
29 reg_genmove white
#? [C8]

loadsgf games/mertin13x13/katsunari-gnugo1.W+45.sgf 32
30 reg_genmove white
#? [B7|J13|E2|J6]

loadsgf games/mertin13x13/katsunari-gnugo1.W+45.sgf 64
31 reg_genmove white
#? [M4|N4]

# A nice tesuji by GNU Go worth remembering!
loadsgf games/mertin13x13/katsunari-gnugo1.W+45.sgf 74
33 reg_genmove white
#? [E7]

# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/katsunari-gnugo1.W+45.sgf 82
34 reg_genmove white
#? [K13]

# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/katsunari-gnugo1.W+45.sgf 102
35 reg_genmove white
#? [D2]

loadsgf games/mertin13x13/katsunari-gnugo1.W+45.sgf 110
36 reg_genmove white
#? [C5]

#CATEGORY=STRATEGY
#SEVERITY=8
loadsgf games/mertin13x13/katsunari-gnugo2.W+4.sgf 12
37 reg_genmove white
#? [E10]*

loadsgf games/mertin13x13/katsunari-gnugo2.W+4.sgf 20
38 reg_genmove white
#? [G11]

#CATEGORY=SEMEAI
loadsgf games/mertin13x13/katsunari-gnugo2.W+4.sgf 44
39 reg_genmove white
#? [H4|J4]

# CATEGORY=FOLLOWUP
# The followup of K13 is undervalued.
# In fact, there is none at all (3.3.8)
loadsgf games/mertin13x13/katsunari-gnugo2.W+4.sgf 50
40 reg_genmove white
#? [K13]*

# C8 can be cut off. Stones added to make the testcase more clear cut.
loadsgf games/mertin13x13/katsunari-gnugo2.W+4.sgf 60
play white H10
play black D5
41 restricted_genmove white C8 D7
#? [D7]*

#CATEGORY=SEMEAI
loadsgf games/mertin13x13/katsunari-gnugo2.W+4.sgf 126
42 reg_genmove white
#? [N3|N2|L4|L3|L2|L1|M1|N1|M4]

#CATEGORY=ENDGAME_TUNING
# A correction in this shape would need subtle tuning, as leaving
# the gap at D7 (by playing D6) should often be bad.
loadsgf games/mertin13x13/gnugo-goliath1.W+11.sgf 25
43 reg_genmove black
#? [D6]*

#CATEGORY=ENDGAME_TUNING
# To get better evaluation for B6 one would need to
# 1) add the follow-up value of the monkey jump (very high here as
# it is sente)
# 2) move the influence barrier in the current situation around B6
# a little further down, and/or add a reverse followup value here
# Part of the overvaluation of M11 is due to the huge shape factor (+8.00)
# by pattern CC77, which does not make too much sense here.
loadsgf games/mertin13x13/gnugo-goliath1.W+11.sgf 27
44 reg_genmove black
#? [B6]

#CATEGORY=ENDGAME_TUNING
# B4 is really huge here, appr. 20 pts.
loadsgf games/mertin13x13/gnugo-goliath1.W+11.sgf 31
45 reg_genmove black
#? [B4]

#CATEGORY=ENDGAME_TUNING
# L3 is 2pt double sente.
loadsgf games/mertin13x13/gnugo-goliath1.W+11.sgf 85
46 reg_genmove black
#? [L3]

#CATEGORY=FUSEKI_STRATEGY
loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 7
47 restricted_genmove black K8 K9 G11
#? [K8|K9]

loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 11
48 reg_genmove black
#? [K7]

loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 13
49 reg_genmove black
#? [K7]

# Renumbered since there were 2 test 49's
loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 19
149 reg_genmove black
#? [L7]*

# Here N11 is generated as an "additional attack/defense move", although
# the black dragon is still dead after black connects at N11.
loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 27
50 owl_defend L10
#? [0]

loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 33
51 reg_genmove black
#? [L5]*

# GNU Go 3.1.15 wants to play F10 here (although 3.0.0 got H10)
loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 35
52 reg_genmove black
#? [H10]

#CATEGORY=BLUNDER
loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 61
53 reg_genmove black
#? [C6]

#CATEGORY=SEMEAI
# This is very difficult. There are some variations in the .sgf-file,
# most of them due to Stefan Mertin. Of course J13 (played by GNU Go)
# is an annoying blunder.
loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 67
54 reg_genmove black
#? [G10]*

loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 83
55 reg_genmove black
#? [F5]*

#CATEGORY=CONNECTION
loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 99
56 reg_genmove black
#? [H3]*

#CATEGORY=ENDGAME_TUNING
# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/gnugo-goliath2.W+38.sgf 111
57 reg_genmove black
#? [C5|D5]

#CATEGORY=OWL_TUNING
# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/gnugo-goliath3.B+16.sgf 55
58 owl_attack J2
#? [0]

# Moves like C8 are severly undervalued by GNU Go. /ab
loadsgf games/mertin13x13/gnugo-katsunari1.B+21.sgf 13
59 restricted_genmove black J11 C8
#? [C8]

loadsgf games/mertin13x13/gnugo-katsunari1.B+21.sgf 15
60 reg_genmove black
#? [K5|L5]

loadsgf games/mertin13x13/gnugo-katsunari1.B+21.sgf 75
61 reg_genmove black
#? [K8]

loadsgf games/mertin13x13/gnugo-katsunari2.B+66.sgf 13
62 reg_genmove black
#? [M5]*

# New failure (comparing 3.0.0 and 3.1.15)
loadsgf games/mertin13x13/gnugo-katsunari2.B+66.sgf 25
63 reg_genmove black
#? [K4]

#CATEGORY=OWL_TUNING
loadsgf games/mertin13x13/gnugo-katsunari2.B+66.sgf 33
64 reg_genmove black
#? [L2|M3|L4|M5]

# New failure (comparing 3.0.0 and 3.1.15)
loadsgf games/mertin13x13/gnugo-katsunari2.B+66.sgf 35
65 reg_genmove black
#? [H4]

loadsgf games/mertin13x13/gnugo-katsunari2.B+66.sgf 47
66 reg_genmove black
#? [G8|G7|H9|J9]*

#CATEGORY=ATARI_ATARI + ENDGAME_TUNING
# New failure (comparing 3.0.0 and 3.1.15)
# EB1404b produces nonsense here. 
loadsgf games/mertin13x13/gnugo-katsunari2.B+66.sgf 77
67 reg_genmove black
#? [B9]

loadsgf games/mertin13x13/gnugo-katsunari2.B+66.sgf 79
68 reg_genmove black
#? [A8]

#CATEGORY=TERRITORIAL_EVALUATION
# There is s.th. odd in the influence function in comparison between
# D4 and D5.
loadsgf games/mertin13x13/gnugo-katsunari2.B+66.sgf 89
69 restricted_genmove black D4 D5
#? [D5]

#CATEGORY=FUSEKI_STRATEGY
# I think that securing the corner is larger.
loadsgf games/mertin13x13/gnugo-gointellect1.W+28.sgf 11
70 reg_genmove black
#? [L10|L9|L11]

loadsgf games/mertin13x13/gnugo-gointellect1.W+28.sgf 19
71 reg_genmove black
#? [J11]

loadsgf games/mertin13x13/gnugo-gointellect1.W+28.sgf 29
72 reg_genmove black
#? [J10]

# New failure (comparing 3.1.15 with 3.0.0)
# The "attack last move played, although it seems bad" heuristic sometimes
# produces very bad choices.
loadsgf games/mertin13x13/gnugo-gointellect1.W+28.sgf 31
73 reg_genmove black
#? [J8]*

#CATEGORY=OWL_TUNING, SEMEAI
# The "attack last move played, although it seems bad" heuristic sometimes
# produces very bad choices. Also, owl is wrong here.
loadsgf games/mertin13x13/gnugo-gointellect1.W+28.sgf 33
74 reg_genmove black
#? [G10]

# D11 is a no-no. /ab
#CATEGORY=TERRITORIAL_EVALUATION
loadsgf games/mertin13x13/gnugo-gointellect2.W+2.sgf 13
75 restricted_genmove black D11 E12
#? [E12]

#CATEGORY=TERRITORIAL_EVALUATION
loadsgf games/mertin13x13/gnugo-gointellect2.W+2.sgf 17
76 reg_genmove black
#? [K6|L6|J3]

# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/gnugo-gointellect2.W+2.sgf 19
77 reg_genmove black
#? [L6|K5]*

loadsgf games/mertin13x13/gnugo-gointellect2.W+2.sgf 21
78 reg_genmove black
#? [L5]*

loadsgf games/mertin13x13/gnugo-gointellect2.W+2.sgf 23
79 reg_genmove black
#? [M5]

# M2 is better for eye shape here.
# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/gnugo-gointellect2.W+2.sgf 41
80 reg_genmove black
#? [M2]

#CATEGORY=ENDGAME_TUNING
loadsgf games/mertin13x13/gnugo-gointellect2.W+2.sgf 55
81 reg_genmove black
#? [M8]

# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 11
82 reg_genmove black
#? [K4]

loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 23
83 reg_genmove black
#? [L9]*

loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 25
84 reg_genmove black
#? [L9]*

loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 29
85 reg_genmove black
#? [D2]

#CATEGORY=OWL_TUNING
# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 45
86 owl_attack L5
#? [1 (H5|J4)]

#CATEGORY=TACTICAL_READING
# GNU Go claims that D6 defends the worms D9 and F9
loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 65
87 reg_genmove black
#? [E6]

#CATEGORY=BLUNDER
# New failure (comparing 3.1.15 with 3.0.0)
loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 69
88 restricted_genmove black A9 D6
#? [D6]

#CATEGORY=OWL_TUNING
# New failure (comparing 3.1.15 with 3.0.0)
# (I hope I read this out correctly. See variations in sgf-file.)
loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 83
89 owl_attack K2
#? [0]

#CATEGORY=ENDGAME_TUNING
loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 99
90 reg_genmove black
#? [A5]

#CATEGORY=ENDGAME_TUNING
loadsgf games/mertin13x13/gnugo-gointellect3.W+37.sgf 109
91 reg_genmove black
#? [K6]*
