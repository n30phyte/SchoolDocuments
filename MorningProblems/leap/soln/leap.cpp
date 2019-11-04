/* This is needed in order to access the input
   and output streams.

   If you get an error message telling you that
   cin or cout are not defined, you probably forgot
   to include this line. */
#include <iostream>

/* cin and cout are in the std namespace.
   If we did not include this line, we would 
   need to use std::cin, std::cout and std::endl. 
   By including this line, we can use them directly,
   because we have told the compiler to bring all 
   symbols in the std namespace into your scope. */
using namespace std;

/* Every C++ program run from the terminal needs a main function. This
   is how the compiler knows where to start. 

   We use "int main()" because the main function always returnsn an
   integer. When you return from main, you are ending your program.
   Sometimes, the return value from your main function may be used by
   another program. For your purposes, the value used doesn't matter,
   but "return 0" is common convention. */
int main()
{

    /*  In C++, every variable must have an explicit type (such as int, float, 
        or char). The type of a variable in C++ is statically declared, and 
        cannot be changed during a program! This means, for example, that this
        variable year cannot later be changed to hold a char type value (unless
        of course that value was cast to an int first). */
    int year;

    /*  cin will read a value up to the next whitespace character in the program.
        For example, if the input given was 
                1 2 3
        then this cin statement would store 1 in the variable year and the other 
        two values would remain in the stream */
    cin >> year;

    // Put your code here! You will probably need to use cout to solve this problem,
    // using cout << value.

    if (year % 400 == 0 || ((year % 4 == 0) && (year % 100 != 0))) {
        cout << "yes" << endl;
    } else {
        cout << "no" << endl;
    }

    // return 0 by convention
    return 0;
}
