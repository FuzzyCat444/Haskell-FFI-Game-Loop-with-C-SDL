#!/bin/bash
ghc --make Engine.hs Src/*.hs ForeignApp.hs -package unordered-containers -package random
ghc -o App Engine.o Src/*.o ForeignApp.o SDLApp.c -I"/usr/include/SDL2" -lSDL2 -lSDL2_mixer -no-hs-main -package unordered-containers -package random
