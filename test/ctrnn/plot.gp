set grid
set output "plot.pdf"
set terminal pdf background rgb 'white'
set xlabel "time"
set ylabel "activation"

set autoscale y
unset key

plot "net-output.dat" using 1:2 with lines, \
     "net-output.dat" using 1:3 with lines

#pause 1
#reread