#!/bin/sh

# export ICFP2021_API_TOKEN=<<team-token>>

# login and create session
wget --keep-session-cookies --save-cookies=cookies.txt --post-data='login.email=team-sampou@googlegroups.com&login.password=sampou2021' https://poses.live/login

# down load pages
for i in $(seq 1 132)
do
    echo DOWNLOADING... ${i}.html
    
    wget --load-cookies cookies.txt https://poses.live/problems/$i
    mv ${i} ${i}.html
    
    mkdir -p ${i}
    
    echo COLLECT URL
    
    cat ${i}.html | grep -o '<a .*href=.*>' | sed -e 's/<a /\n<a /g' | sed -e 's/<a .*href=['"'"'"]//' -e 's/["'"'"'].*$//' -e '/^$/ d' | grep '/solutions/' > ${i}.urls
    cat ${i}.urls | sed "s@/solutions/\(.*\)@wget --header=\"Authorization: Bearer ${ICFP2021_API_TOKEN}\" --load-cookies cookies.txt https://poses.live/api/problems/${i}/solutions/\1; mv \1 ${i}/\1@" | sh
done

# 一次ファイルを破棄
rm login* *.html *.urls
