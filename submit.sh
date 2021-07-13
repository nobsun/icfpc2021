#!/bin/sh

usage() {
    cat <<EOF
Usage: $0 PROBLEM_ID JSON_FILEPATH
EOF
}

if [ x"$1" = x ]; then
    usage
    exit 1
fi

prob_id="$1"

if [ x"$2" = x ]; then
    usage
    exit 1
fi

json_path="$2"

set -x

cp ${json_path} /home/icfpc2021/submit/post/${prob_id}."p$$"-$(date +'%H%M').json
