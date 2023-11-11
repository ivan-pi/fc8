
#include "fc8_io.h"

#include <iostream>

#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>


// Magnification factor
static int MF = 10;

static sf::RenderWindow window;

static bool btest(int32_t var, int pos)
{
    return (var) & (1<<(pos));
}

// Map SFML Keyboard to CHIP8 keyboard (assumes QWERTZ layout is used)
static inline int findkey(sf::Keyboard::Key xkey) {
    switch(xkey) {
    case sf::Keyboard::X:    return 0x0; // X
    case sf::Keyboard::Num1: return 0x1; // 1
    case sf::Keyboard::Num2: return 0x2; // 2
    case sf::Keyboard::Num3: return 0x3; // 3
    case sf::Keyboard::Q:    return 0x4; // Q
    case sf::Keyboard::W:    return 0x5; // W
    case sf::Keyboard::E:    return 0x6; // E
    case sf::Keyboard::A:    return 0x7; // A
    case sf::Keyboard::S:    return 0x8; // S
    case sf::Keyboard::D:    return 0x9; // D
    case sf::Keyboard::Y:    return 0xA; // Y
    case sf::Keyboard::C:    return 0xB; // C
    case sf::Keyboard::Num4: return 0xC; // 4
    case sf::Keyboard::R:    return 0xD; // R
    case sf::Keyboard::F:    return 0xE; // F
    case sf::Keyboard::V:    return 0xF; // V
    default: 
        return -1;
    }
}

extern "C" {

void fc8_display_open(const char *title, int n, const int *mf)
{
    // Set magnification factor
    if (mf) MF = *mf;

    std::string str{title,static_cast<size_t>(n)};
    window.create(sf::VideoMode(64 * MF, 32 * MF), str);
    window.setVerticalSyncEnabled(true);
    window.setKeyRepeatEnabled(false);

    window.clear( sf::Color::Black );
    window.display();
}

void fc8_display_close(void)
{
    window.close();
}

void fc8_display_draw(const int32_t screen[64])
{
    window.clear( sf::Color::Black );
    for (int i = 0; i < 64; i++) {
        for (int j = 0; j < 32; j++) {
            const int k = j + (i < 32 ? 0 : 32);
            if (btest(screen[k], i % 32)) {
                sf::RectangleShape rectangle(sf::Vector2f(MF,MF));
                rectangle.setPosition(i * MF, j * MF);
                window.draw(rectangle);
            }
        }
    }

    window.display();
}

void fc8_display_clear(void)
{
    window.clear( sf::Color::Black );
    window.display();
}

void fc8_event_get(int *irep, int *xkey)
{

    sf::Event event;
    int key;

    bool e = window.waitEvent(event);

    if (!e) {
        *irep = -1;
        return;
    }

    *irep = event.type;

    switch (event.type) {
        case sf::Event::Closed:
            *irep = -2;
            return;
        case sf::Event::KeyPressed:
            std::cout << "key pressed: " << event.key.code << '\n';

            // Escape
            if (event.key.code == sf::Keyboard::Escape) {
                *irep = -2;
                return;
            }

            // Reload
            if (event.key.code == sf::Keyboard::Num0) {
                *irep = -3;
                return;
            }

            key = findkey(event.key.code);
            if (key >= 0) {
                keypad.key[key] = true;
            }
            break;

        case sf::Event::KeyReleased:

            std::cout << "key release: " << event.key.code << '\n';

            key = findkey(event.key.code);
            if (key >= 0) {
                keypad.key[key] = false;
            }
            break;

        default:
            std::cout << "unknown event\n";
            key = -1;
            break;
    }

    *xkey = key;
}

} // extern "C"

