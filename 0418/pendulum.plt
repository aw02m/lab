set terminal x11
set datafile separator whitespace
plot "output" using 2:3 with lines linecolor rgb "green"
set terminal png
set output "pendulum.png"
replot
set terminal x11
replot
pause -1
