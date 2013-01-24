#!/bin/sh

BENCHMARKS="Bench.SumFromTo1
Bench.SumFromTo2
Bench.SumSquare1
Bench.Nofib.Spectral.Constraints
Bench.Nofib.Spectral.Boyer
Bench.Nofib.Spectral.Lambda
Bench.Nofib.Spectral.Circsim
Bench.Fibon.Agum.Main"
for bench in ${BENCHMARKS}; do
    echo "=== ${bench}"
    ./lcvm -e bench ${bench} --stack=10m
done

BENCHMARKS2="Bench.Fibon.Agum.Main"
for bench in ${BENCHMARKS2}; do
    echo "=== ${bench}"
    ./lcvm -e test ${bench} --stack=10m
done
