m, n = map(int, input().split())

output = 0

for i in range(n):
    AlreadyIn, Maximum = map(int, input().split())
    if (AlreadyIn + m) <= Maximum:
        output += 1

print(output)
