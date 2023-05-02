
const char *vShaderStr =
    "#version 120\r\n"
    "attribute vec2 aPos;\n"
    "attribute int id;"
    "void main()\n"
    "{\n"
    "   int ix = id / 32;\n"
    "   int iy = id % 32;\n"
    "   float dx = 2.0f/64, dy = 2.0f/32;\n"
    "   vec2 pos = vec2(aPos.x * (dx/2), aPos.y * (dy/2));\n"
    "   vec2 offset = vec2(-1 + dx/2 + ix*dx, 1 - dy/2 - iy*dy);\n"
    "   gl_Position = vec4(pos + offset, 0.0f, 1.0f);\n"
    "}\0";
