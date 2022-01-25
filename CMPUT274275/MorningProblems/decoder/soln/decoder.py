n = int(input())

output = ""

dictionary = {}

for i in range(n):
    binary, word = input().split()
    dictionary[binary] = word

binarysequence = input()

windowSize = 0
currentIndex = 0
while True:
    if binarysequence[currentIndex : currentIndex + windowSize] in dictionary.keys():
        output += dictionary[binarysequence[currentIndex : currentIndex + windowSize]] + " "
        currentIndex = currentIndex + windowSize
        windowSize = 0
    else:
        windowSize += 1

    if windowSize >= (len(binarysequence) - 1):
        break

print(output.rstrip())
