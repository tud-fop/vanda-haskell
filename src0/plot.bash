#!/bin/bash

# Copyright (c) 2010, Toni Dietze
# License: BSD-style

set -e -u -C
#set -u -C

po='/home/gdp/dietze/Documents/paper-tdbh/plots'

ps='filtered/Benchmark'

# ls "${ps[0]}" | read -a fs


#mkdir "${p}"

##############################################################################

cat >|plot.tmp <<EOF
set terminal latex
set size 0.7,1.0
#set key bottom right
set key off
#set y2tics
#set ytics nomirror
# set label 'test'

set logscale y

#set y2range [0:25]
set xrange [0:60]
set yrange [0.01:100]


set xlabel "{\\\\small length of input sentence in words}"


set output '${po}/tdbh.gnuplot.tex'
plot '${ps}TDBH_hg_400_0_binarized.csv' using (\$6 + 0.0):(\$4 - 0.38) title '0 splits, tdbh with binarized wta' with points 2 \
   , '${ps}TDBH_hg_400_0.csv'           using (\$6 + 0.1):(\$4 - 0.14) title '0 splits, tdbh' with points 4  \
   , '${ps}TDBH_hg_400_2_binarized.csv' using (\$6 + 0.2):(\$4 - 0.62) title '2 splits, tdbh with binarized wta' with points 5  \
   , '${ps}TDBH_hg_400_2.csv'           using (\$6 + 0.3):(\$4 - 0.25) title '2 splits, tdbh' with points 6  \
   , '${ps}TDBH_hg_400_3_binarized.csv' using (\$6 + 0.4):(\$4 - 0.67) title '3 splits, tdbh with binarized wta' with points 7  \
   , '${ps}TDBH_hg_400_3.csv'           using (\$6 + 0.5):(\$4 - 0.27) title '3 splits, tdbh' with points 10

set output '${po}/400_3.gnuplot.tex'
plot '${ps}TDBH_hg_400_3_binarized.csv' using (\$6 + 0.0):(\$4 - 0.67) title 'tdbh with binarized wta' with points 2 \
   , '${ps}TDBH_hg_400_3.csv'           using (\$6 + 0.1):(\$4 - 0.27) title 'tdbh' with points 4  \
   , '${ps}TDBHBin_hg_400_3_binarized.csv' using (\$6 + 0.2):(\$4 - 0.67) title 'tdbh-bin with binarized wta' with points 5  \
   , '${ps}TDBHBin_hg_400_3.csv'           using (\$6 + 0.3):(\$4 - 0.27) title 'tdbh-bin' with points 6  \
   , '${ps}Complete_hg_400_3_binarized.csv' using (\$6 + 0.4):(\$4 - 0.69) title 'complete with binarized wta' with points 7  \
   , '${ps}Complete_hg_400_3.csv'           using (\$6 + 0.5):(\$4 - 0.30) title 'complete' with points 10


set ylabel "{\\\\small\\\\rotatebox{90}{execution time in seconds}}"

set output '${po}/tdbh-bin.gnuplot.tex'
plot '${ps}TDBHBin_hg_400_0_binarized.csv' using (\$6 + 0.0):(\$4 - 0.39) title '0 splits, tdbh-bin with binarized wta' with points 2 \
   , '${ps}TDBHBin_hg_400_0.csv'           using (\$6 + 0.1):(\$4 - 0.14) title '0 splits, tdbh-bin' with points 4  \
   , '${ps}TDBHBin_hg_400_2_binarized.csv' using (\$6 + 0.2):(\$4 - 0.62) title '2 splits, tdbh-bin with binarized wta' with points 5  \
   , '${ps}TDBHBin_hg_400_2.csv'           using (\$6 + 0.3):(\$4 - 0.25) title '2 splits, tdbh-bin' with points 6  \
   , '${ps}TDBHBin_hg_400_3_binarized.csv' using (\$6 + 0.4):(\$4 - 0.67) title '3 splits, tdbh-bin with binarized wta' with points 7  \
   , '${ps}TDBHBin_hg_400_3.csv'           using (\$6 + 0.5):(\$4 - 0.27) title '3 splits, tdbh-bin' with points 10

set output '${po}/400_2.gnuplot.tex'
plot '${ps}TDBH_hg_400_2_binarized.csv' using (\$6 + 0.0):(\$4 - 0.62) title 'tdbh with binarized wta' with points 2 \
   , '${ps}TDBH_hg_400_2.csv'           using (\$6 + 0.1):(\$4 - 0.25) title 'tdbh' with points 4  \
   , '${ps}TDBHBin_hg_400_2_binarized.csv' using (\$6 + 0.2):(\$4 - 0.62) title 'tdbh-bin with binarized wta' with points 5  \
   , '${ps}TDBHBin_hg_400_2.csv'           using (\$6 + 0.3):(\$4 - 0.25) title 'tdbh-bin' with points 6  \
   , '${ps}Complete_hg_400_2_binarized.csv' using (\$6 + 0.4):(\$4 - 0.63) title 'complete with binarized wta' with points 7  \
   , '${ps}Complete_hg_400_2.csv'           using (\$6 + 0.5):(\$4 - 0.27) title 'complete' with points 10

# pause -1  "Hit return to continue"
EOF

gnuplot plot.tmp
