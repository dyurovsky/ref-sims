#!/bin/sh
# Iterate through rows of possible simulations

for i in $(seq 500 100 700)
do
  echo $i
  RScript run_joint_sim.R $i
done
