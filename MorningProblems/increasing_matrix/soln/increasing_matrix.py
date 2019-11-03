n, m = map(int, input().split())
# You can access the j-th element in the i-th row of matrix A using a[i][j].
# Works the same for B. Just make sure 0 <= j < n and 0 <= i < m.
a = [[int(val) for val in input().split()] for _ in range(n)]
b = [[int(val) for val in input().split()] for _ in range(n)]

# Good luck! I believe in you! :)
possible = True
for i in range(n):
    for j in range(m):
        aij = a[i][j]
        bij = b[i][j]
        if a[i][j] > b[i][j]:
            a[i][j], b[i][j] = b[i][j], a[i][j]

# Check column
for j in range(m):
    if not possible:
        break
    for i in range(n-1):
        if a[i][j] >= a[i+1][j] or b[i][j] >= b[i+1][j]:
            possible = False
            break

# Check row
for i in range(n):
    if not possible:
        break
    for j in range(m-1):
        if a[i][j] >= a[i][j+1] or b[i][j] >= b[i][j+1]:
            possible = False
            break

if possible:
    print("Possible")
else:
    print("Impossible")
