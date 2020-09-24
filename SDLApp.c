#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>
#include "HaskellApp_stub.h"

int main(int argc, char**argv) 
{
    hs_init(&argc, &argv);
    
	if (SDL_Init(SDL_INIT_VIDEO) != 0)
	{
		printf("SDL_Init Error: %s\n", SDL_GetError());
		return 1;
	}
	
	char windowTitle[200];
	hs_windowTitleCallback((HsPtr) windowTitle);
	int windowSize[2];
	hs_windowSizeCallback((HsPtr) windowSize);
	SDL_Window *win = SDL_CreateWindow(windowTitle, 
	    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 
	    windowSize[0], windowSize[1], SDL_WINDOW_SHOWN);
	SDL_Renderer *ren = SDL_CreateRenderer(win, -1, 
	    SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	
	char **imageResources = (char**) hs_imageResourcesCallback();
	int textureCount = (int) strtol(imageResources[0], NULL, 10);
	SDL_Texture **textures = malloc(textureCount * sizeof(SDL_Texture*));
	for (int i = 0; i < textureCount; i++) 
	{
	    SDL_Surface *bmp = SDL_LoadBMP(imageResources[i + 1]);
	    if (bmp == NULL)
	    {
		    printf("SDL_LoadBMP Error: %s\n", SDL_GetError());
		    return 1;
	    }
	    SDL_Texture *tex = SDL_CreateTextureFromSurface(ren, bmp);
	    textures[i] = tex;
	}
	
	void *gameState = (void*) hs_initGameStateCallback();
	
	int maxSprites = (int) hs_maxSpritesCallback();
	int *spriteData = malloc((maxSprites * 12 + 1) * sizeof(int));
	int running = 1;
	int ticks = SDL_GetTicks();
	int delta = 0;
	while (running)
	{
	    delta += SDL_GetTicks() - ticks;
	    ticks = SDL_GetTicks();
	    SDL_Event e;
	    while (SDL_PollEvent(&e))
        {
            if (e.type == SDL_QUIT)
                running = 0;
        }
        
        while (delta >= 16)
        {
            hs_updateGameStateCallback((HsStablePtr) gameState);
            delta -= 16;
        }
        
        SDL_RenderClear(ren);
		hs_renderGameStateCallback((HsStablePtr) gameState, (HsPtr) spriteData);
		int numSprites = spriteData[0];
		for (int j = 0; j < numSprites * 13; j += 13)
		{
		    int spriteId = spriteData[j + 1];
		    SDL_Rect srcRect = { spriteData[j + 2], spriteData[j + 3], 
		                         spriteData[j + 4], spriteData[j + 5] };
            SDL_Rect dstRect = { spriteData[j + 6], spriteData[j + 7], 
		                         spriteData[j + 8], spriteData[j + 9] };
            SDL_Point origin = { spriteData[j + 10], spriteData[j + 11] };
            int angleNum = spriteData[j + 12];
            int angleDenom = spriteData[j + 13];
            SDL_Rect *srcRectPtr = srcRect.w == 0 ? NULL : &srcRect;
            SDL_Rect *dstRectPtr = dstRect.w == 0 ? NULL : &dstRect;
            SDL_RenderCopyEx(ren, textures[spriteId], 
                             srcRectPtr, dstRectPtr, 
                             (double) angleNum / angleDenom, &origin,
                             SDL_FLIP_NONE);
		}
		SDL_RenderPresent(ren);
	}
	
	hs_cleanupCallback((HsStablePtr) gameState);

    free(imageResources[0]);
    for (int i = 0; i < textureCount; i++)
    {
        free(imageResources[i + 1]);
        SDL_DestroyTexture(textures[i]);
    }
    
    free(spriteData);
    free(imageResources);
    free(textures);
	SDL_DestroyRenderer(ren);
	SDL_DestroyWindow(win);
	SDL_Quit();
	
    hs_exit();
	
	return 0;
}
