#!/bin/sh

usage() {
    cat <<EOF
Usage: $0 PROBLEM_NUMBER SOLUTION_JSON_FILE
EOF
}

if [ x"$1" = x ]; then
    usage
    exit 1
fi

pnum="$1"

if [ x"$2" = x ]; then
    usage
    exit 1
fi

json_path="$2"

key=$(cat ./.api-key)

set -x

curl -s -X POST -H "Authorization: Bearer $key" --data @${json_path} \
     https://poses.live/api/problems/${pnum}/solutions > ${json_path}.out
