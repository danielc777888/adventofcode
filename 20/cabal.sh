#!/bin/bash

function run
{
    prob=$1
    input=${prob::-2}
    cat $input | cabal run $1 +RTS -M4096m -K8m -sstderr
}

run $1
