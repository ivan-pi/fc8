* [MiniFB](https://github.com/emoon/minifb)
* [wxWidgets](https://www.wxwidgets.org/)
* [Qt GUI](https://doc.qt.io/qt-5/qtgui-index.html)
* [nuklear](https://github.com/vurtun/nuklear)
* [FLTK](https://www.fltk.org/)
* [SFML](https://www.sfml-dev.org/index.php)
* [ImGui](https://github.com/ocornut/imgui)
* [Winteracter](https://www.winteracter.com/)
* [AppGraphics](http://simplyfortran.com/features/appgraphics)
* [GLFW](https://www.glfw.org/)
* [CPW](https://mathies.com/cpw/about.html)
* [Allegro](https://sourceforge.net/projects/alleg/)
* [GLUT-like Windowing, GUI, and Media Control toolkits](https://www.opengl.org/resources/libraries/windowtoolkits/)
* [freeglut](https://freeglut.sourceforge.net/)
* [GLUT](https://www.opengl.org/resources/libraries/glut/spec3/spec3.html)
* [tiny file dialogs](https://sourceforge.net/projects/tinyfiledialogs/)
* [JAPI](https://userpages.uni-koblenz.de/~evol/japi/japi2/japi.html)
* [g2 graphical library](https://sourceforge.net/projects/g2gl/)
* [grwin](http://spdg1.sci.shizuoka.ac.jp/grwin/en/)
* [pilib](https://sourceforge.net/projects/pilib/)

Misc:

* http://fortran-gui.blogspot.com/
* https://wiki.tcl-lang.org/page/Combining+Fortran+and+Tcl+in+one+program
* https://wiki.tcl-lang.org/page/Virtual+Framebuffer
* http://protodesign-inc.com/sansgui.htm
* http://www.xeffort.com/index.html
* SilverFrost FORTRAN + .NET Core?
* [PureBasic](https://www.purebasic.com/)

## Audio

- [libao](https://xiph.org/ao/)
- [portaudio](http://www.portaudio.com/)
- [Win32 `Beep`](https://learn.microsoft.com/en-us/windows/win32/api/utilapiset/nf-utilapiset-beep)
- [OpenAL](https://www.openal.org/) 

## Other

- Software pipelining
- Just-in-time compilation
- Recompilation to other architectures
- constexpr emulator

## FLTK

From comp.lang.fortrna:
```
/*
Fortran callback demo (fortran_cb.cxx)

This demo program shows how you can call a Fortran
subroutine in a callback from a FLTK (c++) main program.

Compile and run using:
gfortran -c do_fortran.f90
g++ -o fortran_cb fortran_cb.cxx do_fortran.o `fltk-config --cxxflags --ldflags`
./fortran_cb

do_fortran.f90:
subroutine do_fortran ( num )
implicit none
integer :: num
num = num + 1
end
*/
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Input.H>
extern "C" {
void do_fortran_ (int *);
}
/*
This is the callback function. It calls the Fortran
subroutine and assigns a new value to the input widget.
*/
void button_cb (Fl_Widget *, void *i) {
int my_number; // number to increment
char buf[20];
Fl_Input *input = (Fl_Input *)i; // the input widget
my_number = atoi(input->value()); // get the current value
for (int i=0; i<500; i++) {
do_fortran_(&my_number); // increment my_number
}
sprintf (buf,"%d",my_number);
input->value(buf); // store the value
}

int main (int argc, char **argv) {
Fl_Window *window = new Fl_Window(200,140);
Fl_Button *button = new Fl_Button(20,20,160,50,"Count (+500)");
Fl_Input *input = new Fl_Input(80,100,60,20,"Value: ");
input->value("0");
window->end();
button->callback(button_cb,input); // attach the callback
window->show(argc, argv);
return Fl::run();
}
/* end of file */
``