#!/bin/bash


if [ $# -lt 1 ]; then
  echo "提供 github user/reponame"
  exit
fi
s="$1"

arr=(${s//\// })
user=${arr[0]}
repo=${arr[1]}
git submodule add https://github.com/$user/$repo.git site-lisp/extensions/$repo
