#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>
#include <SDL_mixer.h>
#include "ForeignApp_stub.h"

typedef enum
{
    EV_KEYDOWN = 0,
    EV_KEYUP = 1,
    EV_MOUSEDOWN = 2,
    EV_MOUSEUP = 3,
    EV_MOUSEMOVE = 4,
    EV_MOUSEWHEEL = 5,
    EV_NONE
} EventType;

typedef enum
{
    KEY_A = 0, KEY_B = 1, KEY_C = 2, KEY_D = 3, KEY_E = 4, KEY_F = 5, 
    KEY_G = 6, KEY_H = 7, KEY_I = 8, KEY_J = 9, KEY_K = 10, KEY_L = 11, 
    KEY_M = 12, KEY_N = 13, KEY_O = 14, KEY_P = 15, KEY_Q = 16, KEY_R = 17, 
    KEY_S = 18, KEY_T = 19, KEY_U = 20, KEY_V = 21, KEY_W = 22, KEY_X = 23, 
    KEY_Y = 24, KEY_Z = 25, KEY_COMMA = 26, KEY_PERIOD = 27, 
    KEY_FORWARDSLASH = 28, KEY_BACKSLASH = 29, KEY_MINUS = 30, KEY_EQUALS = 31, 
    KEY_LBRACKET = 32, KEY_RBRACKET = 33, KEY_SEMICOLON = 34, KEY_QUOTE = 35, 
    KEY_BACKQUOTE = 36, KEY_F1 = 37, KEY_F2 = 38, KEY_F3 = 39, 
    KEY_F4 = 40, KEY_F5 = 41, KEY_F6 = 42, KEY_F7 = 43, KEY_F8 = 44, 
    KEY_F9 = 45, KEY_F10 = 46, KEY_F11 = 47, KEY_F12 = 48, KEY_0 = 49, 
    KEY_1 = 50, KEY_2 = 51, KEY_3 = 52, KEY_4 = 53, KEY_5 = 54, KEY_6 = 55, 
    KEY_7 = 56, KEY_8 = 57, KEY_9 = 58, KEY_SPACE = 59, KEY_TAB = 60, 
    KEY_LSHIFT = 61, KEY_RSHIFT = 62, KEY_LCTRL = 63, KEY_RCTRL = 64, 
    KEY_LALT = 65, KEY_RALT = 66, KEY_DEL = 67, KEY_INS = 68, KEY_ESC = 69,
    KEY_LEFTARROW = 70, KEY_RIGHTARROW = 71, KEY_UPARROW = 72, 
    KEY_DOWNARROW = 73, KEY_ENTER = 74, KEY_BACKSPACE = 75, KEY_NONE
} KeyboardKey;

typedef enum
{
    BUTTON_LEFT = 0,
    BUTTON_MIDDLE = 1,
    BUTTON_RIGHT = 2,
    BUTTON_NONE
} MouseButton;

KeyboardKey getKeyFromSDLK(SDL_Keycode keycode);
MouseButton getMouseButtonFromSDLButton(Uint8 button);

int main(int argc, char**argv) 
{
    hs_init(&argc, &argv);
    
	if (SDL_Init(SDL_INIT_EVERYTHING) != 0)
	{
		printf("SDL_Init Error: %s\n", SDL_GetError());
		return 1;
	}
	
	if (Mix_OpenAudio(22050, MIX_DEFAULT_FORMAT, 2, 4096) != 0)
	{
	    printf("Could not initialize SDL_mixer (audio).\n");
	    return 1;
	}
	
	int frequency;
	Uint16 format;
	int channels;
	Mix_QuerySpec(&frequency, &format, &channels);
	
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
	
	char **soundResources = (char**) hs_soundResourcesCallback();
	int soundCount = (int) strtol(soundResources[0], NULL, 10);
	Mix_Chunk **sounds = malloc(soundCount * sizeof(Mix_Chunk*));
	for (int i = 0; i < soundCount; i++)
	{
	    Mix_Chunk *wav = Mix_LoadWAV(soundResources[i + 1]);
	    if (wav == NULL)
	    {
	        printf("Could not load sound file \"%s\".\n", soundResources[i + 1]);
	        return 1;
	    }
	    sounds[i] = wav;
	}
	
	char **musicResources = (char**) hs_musicResourcesCallback();
	int musicCount = (int) strtol(musicResources[0], NULL, 10);
	Mix_Music **musics = malloc(musicCount * sizeof(Mix_Music*));
	for (int i = 0; i < musicCount; i++)
	{
	    Mix_Music *mus = Mix_LoadMUS(musicResources[i + 1]);
	    if (mus == NULL)
	    {
	        printf("Could not load music file \"%s\".\n", musicResources[i + 1]);
	        return 1;
	    }
	    musics[i] = mus;
	}
	
	void *gameState = (void*) hs_initGameStateCallback();
	
	int maxSprites = (int) hs_maxSpritesCallback();
	int *spriteData = malloc((maxSprites * 13 + 1) * sizeof(int));
	int maxSounds = (int) hs_maxSoundsCallback();
	int *soundData = malloc((maxSounds * 3 + 1) * sizeof(int));
	
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
            EventType eventType = EV_NONE;
            KeyboardKey key = KEY_NONE;
            MouseButton button = BUTTON_NONE;
            double x, y, dx, dy;
            x = y = dx = dy = 0.0;
            double scroll = 0.0;
            
            switch (e.type)
            {
                case SDL_KEYDOWN:
                    eventType = EV_KEYDOWN;
                    key = getKeyFromSDLK(e.key.keysym.sym);
                    if (key == KEY_NONE)
                        continue;
                    break;
                case SDL_KEYUP:
                    eventType = EV_KEYUP;
                    key = getKeyFromSDLK(e.key.keysym.sym);
                    if (key == KEY_NONE)
                        continue;
                    break;
                case SDL_MOUSEBUTTONDOWN:
                    eventType = EV_MOUSEDOWN;
                    button = getMouseButtonFromSDLButton(e.button.button);
                    x = (double) e.button.x;
                    y = (double) e.button.y;
                    if (button == BUTTON_NONE)
                        continue;
                    break;
                case SDL_MOUSEBUTTONUP:
                    eventType = EV_MOUSEUP;
                    button = getMouseButtonFromSDLButton(e.button.button);
                    x = (double) e.button.x;
                    y = (double) e.button.y;
                    if (button == BUTTON_NONE)
                        continue;
                    break;
                case SDL_MOUSEMOTION:
                    eventType = EV_MOUSEMOVE;
                    x = (double) e.motion.x;
                    y = (double) e.motion.y;
                    dx = (double) e.motion.xrel;
                    dy = (double) e.motion.yrel;
                    break;
                case SDL_MOUSEWHEEL:
                    eventType = EV_MOUSEWHEEL;
                    scroll = (double) e.wheel.y;
                    break;
                default: break;
            }
            
            if (eventType == EV_NONE)
                continue;
            
            gameState = (void*) hs_eventCallback((HsStablePtr) gameState, (int) eventType, (int) key, (int) button, x, y, dx, dy, scroll);
        }
        
        while (delta >= 16)
        {
            gameState = (void*) hs_updateGameStateCallback((HsStablePtr) gameState);
            hs_playSoundsCallback((HsStablePtr) gameState, (HsPtr) soundData);
            for (int i = 0; i < soundData[0] * 3; i += 3) {
                int soundId = soundData[i + 1];
                if (soundId >= 0)
                {
                    int channel = Mix_PlayChannel(-1, sounds[soundId], 0);
                    double volume = (double) soundData[i + 2] / soundData[i + 3];
                    if (volume > 1.0) volume = 1.0;
                    if (volume < 0.0) volume = 0.0;
                    Mix_Volume(channel, (int) (volume * 128));
                }
                else
                {
                    printf("Invalid sound id.\n");
                }
            }
            int musicCommand[4];
            hs_musicCallback((HsStablePtr) gameState, (HsPtr) musicCommand);
            int loops = 0;
            switch (musicCommand[0])
            {
                case 0:
                    if (musicCommand[3]) {
                        loops = -1;
                    }
                    Mix_PlayMusic(musics[musicCommand[1]], loops);
                    Mix_VolumeMusic(musicCommand[2]);
                    break;
                case 1:
                    Mix_VolumeMusic(musicCommand[2]);
                    break;
                case 2:
                    Mix_PauseMusic();
                    break;
                case 3:
                    Mix_ResumeMusic();
                    break;
                case 4:
                    Mix_HaltMusic();
                    break;
                default: break;
            }
            
            hs_doIOCallback((HsStablePtr) gameState);
            hs_writeLogsCallback((HsStablePtr) gameState);
            if ((int) hs_shouldQuitCallback((HsStablePtr) gameState)) {
                running = 0;
            }
            delta -= 16;
        }
        
        SDL_RenderClear(ren);
		hs_renderGameStateCallback((HsStablePtr) gameState, (HsPtr) spriteData);
		int numSprites = spriteData[0];
		for (int j = 0; j < numSprites * 13; j += 13)
		{
		    int spriteId = spriteData[j + 1];
		    if (spriteId >= 0)
		    {
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
            else
            {
                printf("Invalid sprite id.\n");
            }
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
    
    free(soundResources[0]);
    for (int i = 0; i < soundCount; i++)
    {
        free(soundResources[i + 1]);
        Mix_FreeChunk(sounds[i]);
    }
    
    free(musicResources[0]);
    for (int i = 0; i < musicCount; i++)
    {
        free(musicResources[i + 1]);
        Mix_FreeMusic(musics[i]);
    }
    
    free(spriteData);
    free(imageResources);
    free(textures);
    
    free(soundData);
    free(soundResources);
    free(sounds);
    
    free(musicResources);
    free(musics);
	SDL_DestroyRenderer(ren);
	SDL_DestroyWindow(win);
	Mix_CloseAudio();
	SDL_Quit();
	
    hs_exit();
	
	return 0;
}

KeyboardKey getKeyFromSDLK(SDL_Keycode keycode)
{
    switch (keycode)
    {
        case SDLK_a:
            return KEY_A;
        case SDLK_b:
            return KEY_B;
        case SDLK_c:
            return KEY_C;
        case SDLK_d:
            return KEY_D;
        case SDLK_e:
            return KEY_E;
        case SDLK_f:
            return KEY_F;
        case SDLK_g:
            return KEY_G;
        case SDLK_h:
            return KEY_H;
        case SDLK_i:
            return KEY_I;
        case SDLK_j:
            return KEY_J;
        case SDLK_k:
            return KEY_K;
        case SDLK_l:
            return KEY_L;
        case SDLK_m:
            return KEY_M;
        case SDLK_n:
            return KEY_N;
        case SDLK_o:
            return KEY_O;
        case SDLK_p:
            return KEY_P;
        case SDLK_q:
            return KEY_Q;
        case SDLK_r:
            return KEY_R;
        case SDLK_s:
            return KEY_S;
        case SDLK_t:
            return KEY_T;
        case SDLK_u:
            return KEY_U;
        case SDLK_v:
            return KEY_V;
        case SDLK_w:
            return KEY_W;
        case SDLK_x:
            return KEY_X;
        case SDLK_y:
            return KEY_Y;
        case SDLK_z:
            return KEY_Z;
        case SDLK_COMMA:
            return KEY_COMMA;
        case SDLK_PERIOD:
            return KEY_PERIOD;
        case SDLK_SLASH:
            return KEY_FORWARDSLASH;
        case SDLK_BACKSLASH:
            return KEY_BACKSLASH;
        case SDLK_MINUS:
            return KEY_MINUS;
        case SDLK_EQUALS:
            return KEY_EQUALS;
        case SDLK_LEFTBRACKET:
            return KEY_LBRACKET;
        case SDLK_RIGHTBRACKET:
            return KEY_RBRACKET;
        case SDLK_SEMICOLON:
            return KEY_SEMICOLON;
        case SDLK_QUOTE:
            return KEY_QUOTE;
        case SDLK_BACKQUOTE:
            return KEY_BACKQUOTE;
        case SDLK_F1:
            return KEY_F1;
        case SDLK_F2:
            return KEY_F2;
        case SDLK_F3:
            return KEY_F3;
        case SDLK_F4:
            return KEY_F4;
        case SDLK_F5:
            return KEY_F5;
        case SDLK_F6:
            return KEY_F6;
        case SDLK_F7:
            return KEY_F7;
        case SDLK_F8:
            return KEY_F8;
        case SDLK_F9:
            return KEY_F9;
        case SDLK_F10:
            return KEY_F10;
        case SDLK_F11:
            return KEY_F11;
        case SDLK_F12:
            return KEY_F12;
        case SDLK_0:
            return KEY_0;
        case SDLK_1:
            return KEY_1;
        case SDLK_2:
            return KEY_2;
        case SDLK_3:
            return KEY_3;
        case SDLK_4:
            return KEY_4;
        case SDLK_5:
            return KEY_5;
        case SDLK_6:
            return KEY_6;
        case SDLK_7:
            return KEY_7;
        case SDLK_8:
            return KEY_8;
        case SDLK_9:
            return KEY_9;
        case SDLK_SPACE:
            return KEY_SPACE;
        case SDLK_TAB:
            return KEY_TAB;
        case SDLK_LSHIFT:
            return KEY_LSHIFT;
        case SDLK_RSHIFT:
            return KEY_RSHIFT;
        case SDLK_LCTRL:
            return KEY_LCTRL;
        case SDLK_RCTRL:
            return KEY_RCTRL;
        case SDLK_LALT:
            return KEY_LALT;
        case SDLK_RALT:
            return KEY_RALT;
        case SDLK_DELETE:
            return KEY_DEL;
        case SDLK_INSERT:
            return KEY_INS;
        case SDLK_ESCAPE:
            return KEY_ESC;
        case SDLK_LEFT:
            return KEY_LEFTARROW;
        case SDLK_RIGHT:
            return KEY_RIGHTARROW;
        case SDLK_UP:
            return KEY_UPARROW;
        case SDLK_DOWN:
            return KEY_DOWNARROW;
        case SDLK_RETURN:
            return KEY_ENTER;
        case SDLK_BACKSPACE:
            return KEY_BACKSPACE;
    }
    return KEY_NONE;
}

MouseButton getMouseButtonFromSDLButton(Uint8 button)
{
    switch (button)
    {
        case SDL_BUTTON_LEFT:
            return BUTTON_LEFT;
        case SDL_BUTTON_MIDDLE:
            return BUTTON_MIDDLE;
        case SDL_BUTTON_RIGHT:
            return BUTTON_RIGHT;
    }
    return BUTTON_NONE;
}
