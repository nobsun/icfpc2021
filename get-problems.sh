#!/bin/sh

(
    set -e

    set -x
    cd data/problems

    for i in $(seq 1 132); do
        n=$(printf "%03d" ${i})
        if [ -r ${n}.json ]; then
            continue
        fi
        curl --output ${n}.json https://poses.live/problems/${i}/download
    done
)
