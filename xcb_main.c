
#include <stdio.h>   // print
#include <stdlib.h>  // exit
#include <string.h>
#include <inttypes.h> // uint32_t

#include <unistd.h> // usleep, sleep

#include <xcb/xcb.h>

const int magnification = 8;
const int WW = 64 * magnification;
const int WH = 32 * magnification;

const char *title = "CHIP-8 interpreter\n";

xcb_point_t *points;

int32_t keymap[16] = {15,26,27,28,20,21,22,8,9,10,14,16,29,23,11,17};
char *keysym = "0123456789ABCDEF";

void render_screen(
  xcb_connection_t *conn,
  xcb_drawable_t window,
  xcb_gcontext_t context_id,
  int *buffer) {

  int k = 0;
  xcb_rectangle_t rect = {0, 0, magnification, magnification};

  for (int i = 0; i < 64; i++) {
    for (int j = 0; j < 32; j++) {

      if (buffer[k++]) {
        rect.x = i * magnification;
        rect.y = j * magnification;
        xcb_poly_fill_rectangle(conn,window,context_id,1,&rect);
      }
    }
  }

}

void fillrand(int *screen) {
  int k = 0;
  for (int i = 0; i < 64; i++) {
    for (int j = 0; j < 32; j++) {
      screen[k++] = rand() % 2;
    }
  }
}

static int screen_buffer[2048];

int main(int argc, char const *argv[])
{
  srand(234);

  points = (xcb_point_t*)malloc(5 * sizeof(xcb_point_t));
  points[0].x = 11;
  points[0].y = 24;
  points[1].x = 30;
  points[1].y = 10;
  points[2].x = 49;
  points[2].y = 24;
  points[3].x = 42;
  points[3].y = 46;
  points[4].x = 18;
  points[4].y = 46;

  int screen_num;
  xcb_connection_t *conn = xcb_connect(NULL,&screen_num);
      // first argument is the X server display name,
      // if this is NULL, use the DISPLAY environment variable

  if (xcb_connection_has_error(conn)) {
    printf("Error opening display.\n");
    exit(1);
  }

  /* Obtain setup info and access the screen */
  const xcb_setup_t* setup = xcb_get_setup(conn);
  xcb_screen_t* screen = xcb_setup_roots_iterator(setup).data;

  printf("Screen dimensions: %d, %d\n", screen->width_in_pixels, screen->height_in_pixels);

  /* Create window */
  xcb_window_t window_id = xcb_generate_id(conn);
  uint32_t prop_name = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
  uint32_t prop_value[2];

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
                       strlen (title), title);

  /* Create graphic context */
  xcb_gcontext_t context_id;
  uint32_t            value_mask;
  uint32_t            value_list[2];

  context_id = xcb_generate_id(conn);
  value_mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  value_list[0] = screen->white_pixel;
  value_list[1] = 0;
  xcb_create_gc(conn, context_id, window_id, value_mask, value_list);

  /* Display the window */
  xcb_map_window(conn, window_id);
  xcb_flush(conn);

  xcb_generic_event_t *event;
  uint32_t            finished = 0;

  xcb_rectangle_t rect = {15, 65, 30, 20};

  while (!finished && (event = xcb_wait_for_event(conn))) {

    char keychar = (char) '*';
    int key;

    switch(event->response_type) {


      case XCB_KEY_PRESS:
        key = ((xcb_key_press_event_t*)event)->detail;
        for (int i = 0; i < 16; ++i) {
          if (keymap[i] == key)
              keychar = keysym[i];
        }
        printf("Keypress code: %d, %c\n", key, keychar);

        // If y (or A in CHIP-8 mapping) was pressed,
        // then clear screen
        if (key == keymap[10]) {
          xcb_clear_area(conn,0,window_id,0,0,WW,WH);
          xcb_flush(conn);
        }
        if (key == keymap[11]) {
          //xcb_poly_fill_rectangle(conn,window_id,context_id,1,&rect);
          //xcb_flush(conn);
          fillrand(screen_buffer);

          xcb_clear_area(conn,0,window_id,0,0,WW,WH);
          render_screen(conn,window_id,context_id,screen_buffer);
          xcb_flush(conn);
        }
        break;
      case XCB_KEY_RELEASE:
        key = ((xcb_key_release_event_t*)event)->detail;
        printf("Keyrelease code: %d\n", key);

        // Check if ESCAPE was pressed
        if (key == 61) finished = 1;
        break;
      case XCB_BUTTON_PRESS:
        printf("Button pressed: %u\n", ((xcb_button_press_event_t*)event)->detail);
        break;
      case XCB_EXPOSE:
        xcb_fill_poly(conn,window_id,context_id,XCB_POLY_SHAPE_CONVEX,
          XCB_COORD_MODE_ORIGIN, 5, points);
        xcb_flush(conn);
        printf("Window exposed.\n");
        break;
      default:
        /* Unknown event type, ignore it */
        printf("Unknown event: %d\n", event->response_type);
        break;
    }

    free(event);
  }

  /* Disconnect from X server */
  xcb_disconnect(conn);
    return 0;
}
