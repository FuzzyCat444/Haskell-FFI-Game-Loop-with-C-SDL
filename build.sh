#!/bin/bash
ghc -c Engine.hs Src/HaskellApp.hs ForeignApp.hs -package unordered-containers -package random
ghc -o App Engine.o Src/HaskellApp.o ForeignApp.o SDLApp.c -I"/usr/include/SDL2" -lSDL2 -lSDL2_mixer -no-hs-main -package unordered-containers -package random
