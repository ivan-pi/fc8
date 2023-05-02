
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#if defined(__APPLE__)
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#endif


const uint8_t ESC  = 27;
const uint8_t NUM0 = 48;


int findkey(unsigned char k) 
{
    switch (k) {
        case 120: return 0x0;
        case  49: return 0x1;
        case  50: return 0x2;
        case  51: return 0x3;
        case 113: return 0x4;
        case 119: return 0x5;
        case 101: return 0x6;
        case  97: return 0x7;
        case 115: return 0x8;
        case 100: return 0x9;
        case 121: return 0xA;
        case  99: return 0xB;
        case  52: return 0xC;
        case 114: return 0xD;
        case 102: return 0xE;
        case 118: return 0xF;
        default:
            return -1;
    }
}

void keypress(unsigned char k, int x, int y) {

    switch (k) {
        case 27: // ESC
            puts("Exiting program.");
            exit(0);
            break;
        default:
            printf("key %X was pressed\n",findkey(k));
    }
}

void keyrelease(unsigned char k, int x, int y) {
    printf("key %X was released\n", findkey(k));
}


// An alternative to glDrawPixels in OpenGL 3.0?
// https://stackoverflow.com/questions/12072799/an-alternative-to-gldrawpixels-in-opengl-3-0

void draw() {

    GLfloat vertices[] = {
        -1.0f, 1.0f,
        -1.0f,-1.0f,
        +1.0f,-1.0f,
        +1.0f,+1.0f
    };

    // do nothing ?
    //glClear(GL_COLOR_BUFFER_BIT);
    //glRectf(0.0f, 0.0f, 0.0f);

    //glDrawArraysInstanced(GL_TRIANGLE_FAN, 0, 4, 5);

    glutSwapBuffers();
}

extern const char *vShaderStr; // in blocks_vert.c

void load_shader() {

    unsigned int vShader;
    vShader = glCreateShader(GL_VERTEX_SHADER);

    glShaderSource(vShader,1,&vShaderStr,NULL);

    glCompileShader(vShader);

    GLint success = 0;
    char infoLog[512];


    glGetShaderiv(vShader,GL_COMPILE_STATUS,&success);

    if (!success)
    {
        glGetShaderInfoLog(vShader,512,NULL,infoLog);
        printf("ERROR: Shader compilation failed: %s\n",infoLog);
    }
}

// Show information about the current GL connection
// Only the OpenGL 2.1 information is shown
void show_gl_connection() {

// Functions called:
//      glGetString - https://registry.khronos.org/OpenGL-Refpages/gl4/html/glGetString.xhtml
//

    printf("GL_VENDOR:   %s\n", glGetString(GL_VENDOR));
    printf("GL_RENDERER: %s\n", glGetString(GL_RENDERER));
    printf("GL_VERSION:  %s\n", glGetString(GL_VERSION));
    printf("GL_SHADING_LANGUAGE_VERSION:  %s\n", 
        glGetString(GL_SHADING_LANGUAGE_VERSION));
                                          

}

int main(int argc, char **argv) {

    glutInitWindowPosition(0, 0);
    glutInitWindowSize(640, 320);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);

    // Setup OpenGL and glut
    glutInit(&argc, argv);          
    glutCreateWindow("My window");

    show_gl_info();
 
    glClearColor(0.0f,0.0f,0.0f,1.0f);


    // Load custom shader
    load_shader();
    return 0;

    glutDisplayFunc(draw);

    glutKeyboardFunc(keypress);
    glutKeyboardUpFunc(keyrelease);

    // Run the emulator
    glutMainLoop();  

    return 0;
}