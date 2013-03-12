#!/bin/sh

BENCHMARKS="Bench.SumFromTo1
Bench.SumFromTo2
Bench.SumSquare1
Bench.SumStream
Bench.Tak
Bench.WheelSieve2
Bench.Nofib.Spectral.Boyer
Bench.Nofib.Spectral.Constraints
Bench.Nofib.Spectral.Circsim
Bench.Nofib.Spectral.Lambda"

for bench in ${BENCHMARKS}; do
    bench_file=tests/`echo "${bench}" | sed 's;\.;/;g'`.hs
    ./lcc -o2 ${bench_file}
done

# Bench.Fibon.Agum.Main"
# BENCHMARKS="Bench.SumFromTo1"
for bench in ${BENCHMARKS}; do
    for run in 1 2 3 4 5; do
        echo "=== ${bench} === run ${run} ==="
        ./lcvm -e bench ${bench} --stack=10m
    done
done

# BENCHMARKS2="Bench.Fibon.Agum.Main"
# for bench in ${BENCHMARKS2}; do
#     echo "=== ${bench}"
#     ./lcvm -e test ${bench} --stack=10m
# done
