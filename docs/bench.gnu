set terminal pdf size 5,5
set output 'benchresults.pdf'

# set size 1, 2
# set size ratio 2

set boxwidth 0.5 absolute
set style fill   solid 1.00 border -1
set key outside right top vertical Left reverse enhanced autotitles columnhead nobox
set key invert samplen 4 spacing 1 width 0 height 0 
set style histogram rowstacked title  offset character 0, 0, 0
set datafile missing '-'
set style data histograms
set xtics border in scale 1,0.5 nomirror rotate by -45  offset character 0, 0, 0 
# set xtics 'test.dat' 1
set xlabel "Benchmark"
set ylabel "No. of operations"
set title "Benchmark Results"

# plot for [ROW=2:6] 'test.dat' using ROW, '' using 1:key(1)
# plot for [ROW=2:6] 'test.dat' using ROW:xticlabels(1)

plot for [ROW=2:12] 'benchdata.dat' using ROW:xticlabels(1)

# plot 'data.dat' using 2:xtic(1), for [i=3:10] '' using i

# plot for [COL=2:4:2] 'test.dat' using COL
# plot 'data.dat' using 2:10
