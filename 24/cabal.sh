#!/bin/bash

function run
{
    cat $2 | cabal run $1 -- +RTS -M4096m -K8m -sstderr
}

run $1 $2
