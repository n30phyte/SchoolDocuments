sentence = input().lower()

vowels = ['a', 'e', 'i', 'o', 'u']

count = 0

for letter in sentence:
    if letter in vowels:
        count += 1

print(count)