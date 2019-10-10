# ---------------------------------------------------
# Name: Michael Kwok
# ID: 1548454
# CMPUT 274, Fall 2019
#
# Weekly Exercise 4: Deck of Cards
# ---------------------------------------------------

import argparse


class Deck:
    """
    Stores playing cards and related information
    """

    # Generate list of ranks, oredered from lowest to highest rank.
    validRanks = list(map(str, range(2, 10))) + ["T", "J", "Q", "K", "A"]
    # Generate list of suits
    validSuits = ["S", "D", "C", "H"]

    def __init__(self, cards):
        '''Initializes attribute for deck of cards

        Arguments:
            cards: list of two-character strings representing cards
        '''

        # Convert each card to an all caps interanl representation as it gets copied.
        self.__deck = list(map(lambda card: str(card).upper(), cards))
        # Filter out blank lines.
        self.__deck = list(filter(None, self.__deck))

    def deal(self):
        '''Deals one card from top of deck.

        Returns:
            Either the top card (first element in the deck list), or
            False if there are no cards left in the deck
        '''

        # Check if deck is empty
        if self.__deck:
            return self.__deck.pop(0)
        else:
            return False

    def validate(self):
        '''Checks whether the deck is a valid 52 card deck or not.

        Returns:
           (is_valid, msg): a tuple containing a Boolean value indicating whether
                            the deck is valid (True) or not (False), and a string
                            that is either empty (when deck is valid) or contains
                            information about why the deck is no valid
        '''

        # Possible error messages
        messages = ["", "Card {} is not a valid card",
                    "Incomplete deck", "Deck contains duplicate cards"]

        # Check each card to see if they're a valid rank or suit
        for card in self.__deck:
            if (card[0] not in self.validRanks) or (card[1] not in self.validSuits):
                return(False, messages[1].format(card))

        # Verify deck is 52 cards
        if len(self.__deck) < 52:
            return (False, messages[2])

        # Verify no duplicates
        # Sets remove duplicates. If after deduplication, there are less cards, there are duplicates.
        if (len(set(self.__deck)) != len(self.__deck)):
            return (False, messages[3])
        return (True, messages[0])

    def __str__(self):
        '''Creates custom string to represent deck object

        Returns:
           String representation of deck object
        '''
        return '-'.join(self.__deck)


def compareCards(playerCard, dealerCard):
    """Compares both cards to see which player wins

    Arguments:
        playerCard: string of player's card
        dealerCard: string of dealer's card

    Returns:
        The string with the result
    """

    # Use list indexing to set up rank scores
    rankScores = Deck.validRanks

    if playerCard[0] == dealerCard[0]:
        return "Tie!"
    elif rankScores.index(playerCard[0]) > rankScores.index(dealerCard[0]):
        return "Player wins!"
    elif rankScores.index(playerCard[0]) < rankScores.index(dealerCard[0]):
        return "Dealer wins!"


def playGame(deck):
    """Plays the game with the input deck. Does not return.

    Arguments:
        deck: The deck the game should be played with.
    """

    canDeal = True
    roundNumber = 1

    # Loops while new cards can be dealt.
    while canDeal:

        player_card = deck.deal()
        dealer_card = deck.deal()

        if player_card:
            print(
                f"Round {roundNumber}: {compareCards(player_card, dealer_card)}")
            roundNumber += 1

        canDeal = player_card


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description="Program to play a game of High Card Draw")
    parser.add_argument("filename")
    parser.parse_args()

    with open(parser.parse_args().filename, "r") as file:

        deck = Deck(file.read().splitlines())

        validationResult = deck.validate()

        if validationResult[0]:
            playGame(deck)
        else:
            print(validationResult[1])
