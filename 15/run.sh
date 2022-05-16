#!/bin/bash

function compile
{
    ghc $1.hs -o $1.out -O2 -ferror-spans -threaded -rtsopts
}

function run
{
    prob=$1
    input=${prob::-1}
    cat $input | ./$1.out +RTS -M4096m -K8m -sstderr
}

compile $1 && run $1
