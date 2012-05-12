
BLACK="/usr/games/gnugo --mode gtp --level 5"
WHITE="./vendor/mogor --13"
gogui -program "gogui-twogtp -black \"$BLACK\" -white \"$WHITE\" -size 13"
