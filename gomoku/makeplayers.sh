#!/bin/bash

cd src
stack ghc -- --make MakePlayers.hs > /dev/null 2>&1
stack ghc -- --make ShufflePlayers.hs > /dev/null 2>&1
./MakePlayers
./ShufflePlayers
rm MakePlayers
rm *.hi
rm *.o
cd ..