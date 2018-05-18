set terminal x11
set datafile separator whitespace
plot "output" using 2:3 with lines
set terminal postscript eps color
set out "output.eps"
replot
set terminal x11
pause -1
reread
