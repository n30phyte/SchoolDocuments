num_matches = int(input())


matchResults = []

for i in range(num_matches):
    roundResult = 0
    for gameRound in input().split():
        # P > R, R > S, S > P
        # let P be 0 , R be 1, and S be 2
        if gameRound == "PR":
            roundResult += 1
        elif gameRound == "RP":
            roundResult -= 1
        elif gameRound == "RS":
            roundResult += 1
        elif gameRound == "SR":
            roundResult -= 1
        elif gameRound == "SP":
            roundResult += 1
        elif gameRound == "PS":
            roundResult -= 1

    if(roundResult < 0):
        matchResults.append("Bob")
    elif(roundResult == 0):
        matchResults.append("Tie")
    else:
        matchResults.append("Alice")
    
if matchResults.count("Bob") > matchResults.count("Alice"):
    
    print("Bob " + str(matchResults.count("Bob")))
else:
    print("Alice " + str(matchResults.count("Alice")))
