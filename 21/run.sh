#!/bin/bash

function compile
{
    ghc $1.hs -o $1.out -O2 -ferror-spans -threaded -rtsopts
}

function run
{
    prob=$1
    if [ $# -eq 1 ]
    then
        input=${prob::-2}
    fi
    if [ $# -eq 2 ]
    then
        input=$2
    fi
    cat $input | ./$1.out +RTS -M4096m -K8m -sstderr
}

compile $1 && run $1
