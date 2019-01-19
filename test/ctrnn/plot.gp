set grid
set output "plot.pdf"
set terminal pdf background rgb 'white'
set xlabel "time"
set ylabel "activation"

set autoscale y

plot "net-output_rdbeer.dat" using 1:2 with lines lw 2 lc rgb "black" title "RB-1", \
     "net-output_rdbeer.dat" using 1:3 with lines lw 2 lc rgb "gray" title "RB-2", \
     "net-output.dat" using 1:2 with lines lc rgb "green" title "RK4-1", \
     "net-output.dat" using 1:3 with lines lc rgb "skyblue" title "RK4-2" 

#pause 1
#reread