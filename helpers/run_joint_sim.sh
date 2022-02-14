#!/bin/sh
# Iterate through rows of possible simulations

for i in $(seq 43800 100 43900)
do
  echo $i
  RScript run_joint_sim.R "communicate" $i 100
done
