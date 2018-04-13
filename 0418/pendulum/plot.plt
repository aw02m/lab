set terminal x11
plot "output" use 2:3 with lines
set terminal png
set output "output.png"
replot
set terminal x11
replot
pause -1
