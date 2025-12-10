#!/bin/bash

function run
{
    cat $1 | cabal run $1 -- +RTS -M4096m -K8m -sstderr
}

run $1
