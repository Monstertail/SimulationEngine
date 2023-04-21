#!/bin/bash

set -e

output_directory="/home/jinwei/engine/SimulationEngine/results"
mkdir -p "$output_directory"

cd /home/jinwei/engine/SimulationEngine/

height=100
widths=(10 50 100 1000)
modes=(6 7 8 9 10)

for width in "${widths[@]}"; do
  for mode in "${modes[@]}"; do
    output_file="${output_directory}/result_height_${height}_width_${width}_mode_${mode}.txt"
    sbt -mem 100000 "project base;testOnly simulation.base.test.gameOLTest -Dheight=$height -Dwidth=$width -Dmode=$mode" > "$output_file"
  done
done
