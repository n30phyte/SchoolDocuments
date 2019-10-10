validRanks = list(map(str, range(2, 10))) + ["T", "J", "Q", "K", "A"]
validSuits = ["S", "D", "C", "H"]

# Too Big
# validSuits = ["S", "D", "C", "H"] * 2
# Dupe
# validSuits = ["S", "D"] * 2
# Garbage
# validSuits = ["$", "5", "^", "]"]

print(*[rank + suit for suit in validSuits for rank in validRanks], sep='\n')