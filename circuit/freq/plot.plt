set terminal x11
set datafile separator whitespace
plot "output2" using 1:4 with dots 
set terminal postscript eps color
set out "output.eps"
replot
set terminal x11
replot
pause -1
