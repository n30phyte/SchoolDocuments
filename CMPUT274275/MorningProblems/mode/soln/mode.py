input_list = input().split()
input_set = set(input_list)
outputs = list()

highest_value = 0

for word in input_set:
    if highest_value <= input_list.count(word):
        highest_value = input_list.count(word)

for word in input_set:
    if input_list.count(word) == highest_value:
        outputs.append(word)

outputs.sort()

for word in outputs:
    print(word)