#!/usr/bin/gnuplot


# assumed columns in statistics.csv file:
# 1         2          3      4       5               6           7           8                9            10            11                    12                    13                14                       15
# CPU time  iteration  rules  states  initial states  beam width  beam index  candidate index  rule merges  state merges  initial-state merges  log likelihood delta  likelihood delta  log evaluation of merge  evaluation of merge


reset

set terminal pdf noenhanced size 29.7cm, 21.0cm
set output 'statistics.pdf'

set multiplot title GPVAL_PWD layout 3, 2 columnsfirst downwards

set datafile separator ','
set key autotitle columnhead

set key tmargin left

set ytics nomirror
set y2tics

set lmargin 10
set rmargin 10


### First column ###

delta_v(x) = (vD = x - old_v, old_v = x, vD)
old_v=0
plot 'statistics.csv' using 2:(delta_v($1)) with points pointtype 7 pointsize 0.5 title 'Δ CPU time',  \
     '' using 2:1 with points pointtype 7 pointsize 0.1 axes x1y2


plot '' using 2:3 with points pointtype 5 pointsize 0.1


plot '' using 2:4 with points pointtype 5 pointsize 0.1, '' using 2:5 with points pointtype 5 pointsize 0.1 axes x1y2


### Second column ###

accumulate(x) = (accumulationSum = accumulationSum + x)
accumulationSum = 0
# set yrange [-10 : 0]
plot '' using 2:12 with points pointtype 7 pointsize 0.5,  \
     '' using 2:(accumulate($12)) with points pointtype 7 pointsize 0.1 axes x1y2 title 'accumulated log₂ likelihood delta'


plot 'statistics.csv' using 2:7 with points linecolor 'blue' pointtype 5 pointsize 0.1
beam_index_min = GPVAL_Y_MIN
beam_index_max = GPVAL_Y_MAX


# http://psy.swansea.ac.uk/staff/carter/gnuplot/gnuplot_frequency.htm
bin_width = (beam_index_max - beam_index_min + 1) / 10;
bin_number(x) = floor(x/bin_width)
rounded(x) = bin_width * ( bin_number(x) + 0.5 )

plot '' using (rounded($7)):(1) smooth frequency with boxes fillstyle solid 0.25 title 'histogram',  \
     '' using 7:2 with points pointtype 5 pointsize 0.1 axes x1y2 title 'beam index (transposed)'


### End ###

unset multiplot