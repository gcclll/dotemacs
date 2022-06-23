#!/bin/bash


if [ $# -lt 1 ]; then
  echo "提供 github user/reponame"
  exit
fi
s="$1"

arr=(${s//\// })
user=${arr[0]}
repo=${arr[1]}

host="https://github.com"

case "${2-}" in
    -l) host="https://gitlab.com" ;;
     *) echo "use default: $host"
esac

git submodule add -f $host/$user/$repo.git site-lisp/extensions/$repo
