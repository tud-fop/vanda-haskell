#!/usr/bin/gnuplot


# assumed columns in statistics.csv file:
# 1         2          3      4       5               6            7           8           9                10           11            12                    13                     14                15                        16
# CPU time  iteration  rules  states  initial states  merge pairs  beam width  beam index  candidate index  rule merges  state merges  initial-state merges  log₂ likelihood delta  likelihood delta  log₂ evaluation of merge  evaluation of merge

# assumed columns in statistics-evaluations.csv
# 1          2               3                4                         5
# iteration  beam index low  beam index high  log₂ evaluation of merge  evaluation of merge


min(x, y) = x <= y ? x : y
max(x, y) = x >= y ? x : y


set datafile separator ','
set key autotitle columnhead


# we just want to set GPVAL_DATA_?_M??
set terminal dumb
print 'analyzing data'
plot 'statistics-evaluations.csv' using 1:3 with dots

width  = 0.1 * (GPVAL_DATA_X_MAX - GPVAL_DATA_X_MIN) + 21
height = 21
aspect = width / height
set terminal pdf noenhanced size (width)cm, (height)cm

set title GPVAL_PWD

set key off
set colorbox user origin (1 - 0.1 / aspect), 0.05 size (0.05 / aspect), 0.85
set rmargin at screen (1 - 0.2 / aspect)

set tics out
set x2tics
set y2tics

set xrange  [GPVAL_DATA_X_MIN - 0.5 : GPVAL_DATA_X_MAX + 0.5]
set x2range [GPVAL_DATA_X_MIN - 0.5 : GPVAL_DATA_X_MAX + 0.5]
set yrange  [GPVAL_DATA_Y_MIN - 0.5 : GPVAL_DATA_Y_MAX + 0.5]
set y2range [GPVAL_DATA_Y_MIN - 0.5 : GPVAL_DATA_Y_MAX + 0.5]
set cbrange [-20 : 0]  # range of palette


filename = 'statistics-evaluations'
set output filename.'.pdf'
print 'generating '.GPVAL_OUTPUT
plot filename.'.csv' using (0):(0):($1 - 0.5):($1 + 0.5):($2 - 0.5):($3 + 0.5):4 with boxxy fill solid noborder palette,  \
     'statistics.csv' using 2:8 with points linecolor 'blue' pointtype 4 pointsize 0.5,  \
     '' using 2:6 with histeps

filename = 'statistics-evaluations-ascending'
print 'generating '.filename.'.csv'
! LC_ALL=C sort -g -k 4,4    -t, statistics-evaluations.csv > @filename.csv
set output filename.'.pdf'
print 'generating '.GPVAL_OUTPUT
replot

filename = 'statistics-evaluations-descending'
print 'generating '.filename.'.csv'
! LC_ALL=C sort -g -k 4,4 -r -t, statistics-evaluations.csv > @filename.csv
set output filename.'.pdf'
print 'generating '.GPVAL_OUTPUT
replot


set output 'statistics-evaluations-both.pdf'
print 'generating '.GPVAL_OUTPUT
plot 'statistics-evaluations-descending.csv' using (0):(0):($1 - 0.5):1:($2 - 0.5):($3 + 0.5):4 with boxxy fill solid noborder palette,  \
     'statistics-evaluations-ascending.csv'  using (0):(0):1:($1 + 0.5):($2 - 0.5):($3 + 0.5):4 with boxxy fill solid noborder palette,  \
     'statistics.csv' using 2:8 with points linecolor 'blue' pointtype 4 pointsize 0.5,  \
     '' using 2:6 with histeps
