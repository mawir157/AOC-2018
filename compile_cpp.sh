#!/bin/bash

for i in $(seq -f "%02g" 1 25)
do
  g++ -o $i.out jmt_main_$i.cpp -O2 -std=c++11
done

for i in $(seq -f "%02g" 1 25)
do
  echo "Day $i" 
  ./$i.out input_$i.txt
  rm $i.out
  echo
done
