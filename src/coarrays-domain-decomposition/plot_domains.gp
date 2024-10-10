# plot_domains.gp --
# 	Plot the heatmap of the three domains
#	
#   Usage: 
#	  $ gnuplot -c plot_domains.gp <case_name>
#
set terminal pngcairo enhanced size 800,600

outfile = ARG1.'.png'
set output outfile

d1 = ARG1.'_1.out'
d2 = ARG1.'_2.out'
d3 = ARG1.'_3.out'

set title "Heat conduction in a L-shaped plate"
set xlabel "x"
set ylabel "y"

set cbrange [0:1]
set cblabel "Temperature"

set xrange [-1:61]
set yrange [-1:41]
set tics out

set size ratio -1

shft = -0.5

wh = 20

plot d1 matrix u ($1+shft):($2+shft):3 w image notitle,\
     d2 matrix u ($1+shft+wh):($2+shft):3 w image notitle,\
     d3 matrix u ($1+shft+wh):($2+shft+wh):3 w image notitle

