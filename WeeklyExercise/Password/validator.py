# ---------------------------------------------------
# Name: Michael Kwok
# ID: 1548454
# CMPUT 274, Fall 2019
#
# Weekly Exercise 1: Password Validator and Generator
# ---------------------------------------------------

import string
import random

specialCharacters = set("!#$%&'()*+,./:;<=>?@[]^`{|}~")
lowercaseCharacters = set(string.ascii_lowercase)
uppercaseCharacters = set(string.ascii_uppercase)
digitCharacters = set(string.digits)


def validate(password):
    """ Analyzes an input password to determine if it is "Secure", "Insecure", or "Invalid" based on the assignment description criteria.

    Arguments:
        password (string): a string of characters

    Returns:
        result (string): either "Secure", "Insecure", or "Invalid".
    """

    invalid_characters = set(" -_")
    # Turn password into set of characters. Length and order doesn't matter for verification method,
    # just presence of characters.
    password_characters = set(password)

    # Check for invalid characters by using set intersection, and check for password length by comparison
    if (password_characters & invalid_characters) or (len(password) < 8):
        return "Invalid"

    # Check for presence of characters by using set intersection. If password is shorter than 8 characters, function
    # would have returned already, so a check is unnecessary here
    if (
        (password_characters & lowercaseCharacters)
        and (password_characters & uppercaseCharacters)
        and (password_characters & digitCharacters)
        and (password_characters & specialCharacters)
    ):
        return "Secure"

    return "Insecure"


def generate(n):
    """ Generates a password of length n which is guaranteed to be Secure according to the given criteria.

    Arguments:
        n (integer): the length of the password to generate, n >= 8.

    Returns:
        secure_password (string): a Secure password of length n. 
    """

    # Force n >= 8 to make sure passwords generated always secure
    if n < 8:
        n = 8

    # Character Set list to help with generating passwords
    charset = [
        list(specialCharacters),
        list(lowercaseCharacters),
        list(uppercaseCharacters),
        list(digitCharacters),
    ]

    # We need at least one each of the following
    # Uppercase character, lowercase character, digit and special characters.
    # This ensures that that requirement is fulfilled
    output = [
        random.choice(charset[0]),
        random.choice(charset[1]),
        random.choice(charset[2]),
        random.choice(charset[3]),
    ]

    # Flatten the charsets into one big list
    flattened = [character for sublist in charset for character in sublist]

    # Increase possible choices when random picks
    flattened = flattened * 6

    # Shuffle list to decrease predictability
    random.shuffle(flattened)

    # First 4 characters are already generated for us above, so we will generate the rest
    # in this loop.
    for x in range(n - 4):
        # Pick a random character from the charset.
        output.append(random.choice(flattened))

    # Shuffle the list of characters we got
    random.shuffle(output)

    return "".join(output)  # Convert list to string and return it.


if __name__ == "__main__":
    # Any code indented under this line will only be run
    # when the program is called directly from the terminal
    # using "python3 validator.py". This can be useful for
    # testing your implementations.

    pass
