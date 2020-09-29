#!/bin/bash
ghc -c Engine.hs HaskellApp.hs ForeignApp.hs -package unordered-containers -package random
ghc -o App Engine.o HaskellApp.o ForeignApp.o SDLApp.c -I"/usr/include/SDL2" -lSDL2 -no-hs-main -package unordered-containers -package random
