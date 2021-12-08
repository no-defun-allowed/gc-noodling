set title "To copy or to compact?"
set size ratio 0.66
set terminal svg enhanced background rgb 'white' linewidth 3
set datafile separator ','
set tics scale 0.5
set border 3
set xtics nomirror offset 0, 0.25
set ytics nomirror
set grid mytics ytics
set xlabel "Heap size" offset 0, 0.75
set ylabel "Cache misses" offset 2, 0
set yrange [0:2500]
set format y "%.1s %c"
set terminal svg size 1440,1080 font 'Open Sans, 30'
set key bmargin Left center horizontal samplen 1 reverse width -2
unset colorbox
plot 'data.csv' using 1:3 with lines title "Semispace" lt rgb "#993333", \
     '' using 1:5 with lines title "Lisp 2 compacting" lt rgb "#339933"