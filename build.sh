#!/bin/bash
ghc -c HaskellApp.hs -package unordered-containers
ghc -o App HaskellApp.o SDLApp.c -I"/usr/include/SDL2" -lSDL2 -no-hs-main -package unordered-containers
