#!/usr/bin/gnuplot


# assumed columns in statistics.csv file:
# 1         2          3      4       5               6           7           8                9            10            11                    12                    13                14                       15
# CPU time  iteration  rules  states  initial states  beam width  beam index  candidate index  rule merges  state merges  initial-state merges  log likelihood delta  likelihood delta  log evaluation of merge  evaluation of merge

# assumed columns in statistics-evaluations.csv
# 1          2               3                4                         5
# iteration  beam index low  beam index high  log₂ evaluation of merge  evaluation of merge


reset

set datafile separator ','
set key autotitle columnhead


set terminal dumb

plot 'statistics-evaluations.csv' using 1:3 with dots
y_max = GPVAL_DATA_Y_MAX

min(x, y) = x <= y ? x : y
max(x, y) = x >= y ? x : y

set terminal pdf noenhanced size (0.1 * (GPVAL_DATA_X_MAX - GPVAL_DATA_X_MIN) + 21)cm, 21cm
set output 'statistics-beam.pdf'

set title GPVAL_PWD

set key off


plot 'statistics-evaluations.csv' using (0):(0):($1 - 0.5):($1 + 0.5):($2 - 0.5):(min($3 + 0.5, y_max)):(max(-20, min(0, $4))) with boxxy fill solid noborder palette,  \
     'statistics.csv' using 2:7 with points linecolor 'blue' pointtype 4 pointsize 0.5
