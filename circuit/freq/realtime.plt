set terminal x11
set datafile separator whitespace
plot "output1" using 2:3 with lines,\
     "output2" using 2:3 with points pointtype 7
pause -1
reread
