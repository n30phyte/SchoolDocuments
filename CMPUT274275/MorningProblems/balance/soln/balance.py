# Read the input
a, b, c = map(int, input().split())

# Solve the problem

# Calculate average
average = (a + b + c) // 3

# Output the result

map(lambda money: print(money), [a-average, b-average, c-average])