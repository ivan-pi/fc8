
#include <stdio.h>   // print
#include <stdlib.h>  // exit
#include <string.h>
#include <inttypes.h> // uint32_t
#include <assert.h>

#include <unistd.h> // usleep, sleep

#include <xcb/xcb.h>

/* Magnification factor */
#define M 8

#define WW (64*M)
#define WH (32*M)

static xcb_connection_t     *conn;
static int                  screen_num;
static xcb_screen_t         *screen;
static const xcb_setup_t    *setup;

static xcb_window_t window_id;
static xcb_gcontext_t context_id;

void fcw_open__(char *title) {

    conn = xcb_connect(NULL,&screen_num);
      // first argument is the X server display name,
      // if this is NULL, use the DISPLAY environment variable

    if (xcb_connection_has_error(conn)) {
        printf("Error opening display.\n");
        exit(1);
    }

    setup = xcb_get_setup(conn);
    screen = xcb_setup_roots_iterator(setup).data;

    /* Create window */
    window_id = xcb_generate_id(conn);

    uint32_t prop_name;
    uint32_t prop_value[2];
    prop_name = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
    prop_value[0] = screen->black_pixel;
    prop_value[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_BUTTON_PRESS | 
                    XCB_EVENT_MASK_KEY_PRESS | XCB_EVENT_MASK_KEY_RELEASE ;

    xcb_create_window(conn, screen->root_depth, 
        window_id, screen->root, 
        0, 0, 
        WW, WH, 1,
        XCB_WINDOW_CLASS_INPUT_OUTPUT, 
        screen->root_visual, prop_name, &prop_value);

    /* Set the title of the window */
    xcb_change_property (conn, XCB_PROP_MODE_REPLACE, window_id,
                       XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 8,
                       strlen(title), title);

    /* Create graphic context */
    context_id = xcb_generate_id(conn);

    uint32_t value_mask;
    uint32_t value_list[2];
    value_mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
    value_list[0] = screen->white_pixel;
    value_list[1] = 0;

    xcb_create_gc(conn, context_id, window_id, value_mask, value_list);

    /* Display the window */
    xcb_map_window(conn, window_id);
    xcb_flush(conn);

}

void fcw_clear_screen_() {
    xcb_clear_area(conn,0,window_id,0,0,WW,WH);
    xcb_flush(conn);
}

#define BTEST(var,pos) ((var) & (1<<(pos)))

void fcw_draw_screen_(int32_t screen[64]) {

    xcb_clear_area(conn,0,window_id,0,0,WW,WH);

    xcb_rectangle_t rect = {0, 0, M, M};

    for (int i = 0; i < 64; i++) {
        for (int j = 0; j < 32; j++) {
            const int k = j + (i < 32 ? 0 : 32);
            //printf("i,j,k = %d, %d, %d\n",i,j,k);
            if (BTEST(screen[k], i % 32)) {
                rect.x = i * M;
                rect.y = j * M;
                xcb_poly_fill_rectangle(conn,window_id,context_id,1,&rect);
            }
        }
    }

    xcb_flush(conn);
}

void fcw_flush_() {
    xcb_flush(conn);
}

void fcw_close_() {
    xcb_disconnect(conn);
}

// Mapping between XCB and CHIP-8 key bindings
static int32_t const keypad[16] = 
    {15,26,27,28,20,21,22,8,9,10,14,16,29,23,11,17};

/* The CHIP-8 keypad (hexadecimal digits)*/
static int key_state[16] = {
    0,0,0,0,
    0,0,0,0,
    0,0,0,0,
    0,0,0,0 };

void fcw_event_(int *ievent, int *istat) {

    xcb_generic_event_t *e;
    e = xcb_poll_for_event(conn);
    if (!e) {
        *istat = -1;
        return;
    }

    *ievent = e->response_type;
    int xkey;

    switch(e->response_type) {

      case XCB_KEY_PRESS:
        xkey = ((xcb_key_press_event_t*)e)->detail;
        printf("Keypress code: %d\n", xkey);
        for (int i = 0; i < 16; ++i) {
          if (keypad[i] == xkey) key_state[i] = 1;
        }
        break;
      case XCB_KEY_RELEASE:
        xkey = ((xcb_key_release_event_t*)e)->detail;
        printf("Key release code: %d\n", xkey);
        // Check if ESCAPE was pressed
        if (xkey == 61) {
            *istat = -2;
        } else {
            for (int i = 0; i < 16; ++i) {
                if (keypad[i] == xkey) key_state[i] = 0;
            }
        }
        break;
      case XCB_EXPOSE:
        xcb_flush(conn);
        printf("Window exposed.\n");
        break;
      default:
        /* Unknown event type, ignore it */
        printf("Unknown event: %d\n", e->response_type);
        break;
    }
    free(e);
}

int fcw_getkey_(int *key) {
    assert((0 <= *key) && (*key < 16));
    return key_state[*key];
}


int fcw_keypress_(int *key) {

    xcb_generic_event_t *e;
    int xkey;

    int result;

    e = xcb_poll_for_event(conn);
    if (!e) {
        result = 0;
        goto end;        
    }

    switch (e->response_type) {
    case XCB_KEY_PRESS:
        xkey = ((xcb_key_press_event_t*)e)->detail;
        printf("Keypress code: %d\n", xkey);
        if (xkey == 61) {
            result = 2;
            break;
        } else if (xkey == keypad[*key]) {
            result = 1;
            break;
        }
    default:
        printf("No keypress\n");
        result = 0;
    }
    free(e);
end:
    return result;
}

int fcw_keyrelease_(int *key) {

    xcb_generic_event_t *e;
    int xkey, result;

    e = xcb_poll_for_event(conn);
    if (!e) {
        result = 0;
        goto end;        
    }

    switch (e->response_type) {
    case XCB_KEY_RELEASE:
        xkey = ((xcb_key_release_event_t*)e)->detail;
        printf("Keyrelease code: %d\n", xkey);
        if (xkey == keypad[*key]) {
            result = 1;
            break;
        }
    default:
        result = 0;
    }
    free(e);
end:
    return result;
}