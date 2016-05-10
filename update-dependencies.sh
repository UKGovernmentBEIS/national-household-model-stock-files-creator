grep -ri library|cut -d':' -f2|tr -d ' '|grep -v \#|sort | uniq | sed -r -e 's/library\((.+)\)/install.packages("\1")/g' > install-dependencies.R
