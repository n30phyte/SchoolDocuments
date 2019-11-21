#include <array>

enum _signals { LOW = 0x0,
    HIGH };

enum _output_types { OUTPUT = 0x0,
    INPUT,
    INPUT_PULLUP };

std::array<int, 54> _PINS;
std::array<int, 54> _PIN_MODE;

void init() {};

int digitalRead(unsigned int _pin)
{
    return _PINS[_pin];
}

int digitalWrite(unsigned int _pin, unsigned int _value)
{
    _PINS[_pin] = _value;
}

int pinMode(unsigned int _pin, unsigned int _mode)
{
    _PIN_MODE[_pin] = _mode;
}
