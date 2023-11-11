
#include "fc8_io.h"

#include <stdbool.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SDL2/SDL.h"

static int MF = 10; // Magnification Factor

static struct SDL_Window   *window;
static struct SDL_Renderer *window_renderer;

void fc8_display_open(const char *title, int n, const int *mf)
{
    // Set magnification factor
    if (mf) MF = *mf;

    int istat = SDL_Init(SDL_INIT_VIDEO|SDL_INIT_AUDIO); // Maybe add INIT_TIMER for timer subsystem?
    if (istat < 0) {
        puts("Failed to initialize SDL");
        puts(SDL_GetError());
    }

    char* tmp = (char*) malloc((n+1) * sizeof(char) );
    strncpy(tmp, title, n * sizeof(char) );
    tmp[n] = '\0';

    window = SDL_CreateWindow(tmp,
                              SDL_WINDOWPOS_UNDEFINED,
                              SDL_WINDOWPOS_UNDEFINED,
                              64 * MF, 32 * MF,
                              0);

    free(tmp);

    //window_renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);
    window_renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);

    istat = SDL_RenderClear(window_renderer);
    if (istat < 0) puts(SDL_GetError());

    SDL_RenderPresent(window_renderer);
    if (istat < 0) puts(SDL_GetError());

}

void fc8_display_close(void)
{
    SDL_DestroyRenderer(window_renderer);
    SDL_DestroyWindow(window);    
    SDL_Quit();
}

static bool btest(int32_t var, int pos)
{
    return (var) & (1<<(pos));
}

void fc8_display_draw(const int32_t screen[64])
{
    SDL_RenderClear(window_renderer);
    SDL_SetRenderDrawColor(window_renderer,255,255,255,255); // WHITE

    for (int i = 0; i < 64; i++) {
        for (int j = 0; j < 32; j++) {
            const int k = j + (i < 32 ? 0 : 32);
            if (btest(screen[k], i % 32)) {
                SDL_Rect rect;
                rect.x = i * MF;
                rect.y = j * MF;
                rect.w = MF; rect.h = MF;
                SDL_RenderFillRect(window_renderer, &rect);
            }
        }
    }

    // Reset color to black
    SDL_SetRenderDrawColor(window_renderer, 0, 0, 0, 255);
    
    SDL_RenderPresent(window_renderer);
}

void fc8_display_clear(void)
{
    SDL_RenderClear(window_renderer);
    SDL_RenderPresent(window_renderer);
}


// Map SDL Scancode to CHIP8 keyboard
//
// The scancodes assumes a standard US QWERTY keyboard is used.
// Hence, no workarounds should be needed for other virtual layouts (e.g. QWERTZ or AZERTY)
//
static int findkey(SDL_Scancode xkey) {
    switch(xkey) {
    case SDL_SCANCODE_X: return 0x0; // X
    case SDL_SCANCODE_1: return 0x1; // 1
    case SDL_SCANCODE_2: return 0x2; // 2
    case SDL_SCANCODE_3: return 0x3; // 3
    case SDL_SCANCODE_Q: return 0x4; // Q
    case SDL_SCANCODE_W: return 0x5; // W
    case SDL_SCANCODE_E: return 0x6; // E
    case SDL_SCANCODE_A: return 0x7; // A
    case SDL_SCANCODE_S: return 0x8; // S
    case SDL_SCANCODE_D: return 0x9; // D
    case SDL_SCANCODE_Z: return 0xA; // Z
    case SDL_SCANCODE_C: return 0xB; // C
    case SDL_SCANCODE_4: return 0xC; // 4
    case SDL_SCANCODE_R: return 0xD; // R
    case SDL_SCANCODE_F: return 0xE; // F
    case SDL_SCANCODE_V: return 0xF; // V
    default: 
        return -1;
    }
}

// How to disable key repeat in SDL2:
//   https://stackoverflow.com/questions/22156815/how-to-disable-key-repeat-in-sdl2

void fc8_event_get(int *irep, int *xkey)
{

    SDL_Event event;
    int key = -1;

    int e = SDL_WaitEvent(&event);

    if (!e) {
        *irep = -1;
        return;
    }

    *irep = (int) event.type;

    switch (event.type) {
        case SDL_QUIT:
            *irep = -2;
            return;
        case SDL_KEYDOWN:

            // Filter out repeats
            if (event.key.repeat) break;

            printf("key pressed: %s\n",SDL_GetKeyName(event.key.keysym.sym));

            // Escape
            if (event.key.keysym.scancode == SDL_SCANCODE_ESCAPE) {
                *irep = -2;
                return;
            }

            // Reload
            if (event.key.keysym.scancode == SDL_SCANCODE_0) {
                *irep = -3;
                return;
            }

            key = findkey(event.key.keysym.scancode);
            if (key >= 0) {
                keypad.key[key] = true;
            }
            break;

        case SDL_KEYUP:
            
            // Filter out repeats
            if (event.key.repeat) break; 

            printf("key released: %s\n",SDL_GetKeyName(event.key.keysym.sym));

            key = findkey(event.key.keysym.scancode);
            if (key >= 0) {
                keypad.key[key] = false;
            }
            break;

        default:
            puts("unknown event");
            break;
    }

    *xkey = key;
}
