set grid
set output "plot.pdf"
set terminal pdf background rgb 'white'
set xlabel "time"
set ylabel "activation"

set autoscale y

     
plot "net-output_euler.dat" using 1:2 with lines lt rgb "red" title "FE-1", \
     "net-output_euler.dat" using 1:3 with lines lt rgb "pink" title "FE-2", \
     "net-output_rdbeer.dat" using 1:2 with lines lt rgb "black" title "RB-1", \
     "net-output_rdbeer.dat" using 1:3 with lines lt rgb "gray" title "RB-2", \
     "net-output.dat" using 1:2 with lines lt rgb "blue" title "RK4-1", \
     "net-output.dat" using 1:3 with lines lt rgb "skyblue" title "RK4-2" 

#pause 1
#reread