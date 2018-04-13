set terminal x11
set datafile separator whitespace
plot "output" using 2:3 with lines
pause -1
reread
