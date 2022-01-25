# Get the input
leftlane = input().split()
rightlane = input().split()

output = list()

leftmax = len(leftlane)
rightmax = len(rightlane)

for car in range(max([leftmax, rightmax])):
    if car < leftmax:
        output.append(leftlane[car])
    if car < rightmax:
        output.append(rightlane[car])

print(*output)
# Print the result