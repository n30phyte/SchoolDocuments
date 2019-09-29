import math

n = int(input())

x, y, r = [], [], []
# x, y, r are empty lists

for i in range(n):
    x_in, y_in, r_in = map(float, input().split())
    x.append(x_in)
    y.append(y_in)
    r.append(r_in)


m = int(input())

output = [None] * m

for j in range(m):
    x_in, y_in = map(float, input().split())

    for circle in range(n):
        distance = math.sqrt((x[circle] - x_in) ** 2 + (y[circle] - y_in) ** 2)
        if distance <= r[circle]:
            output[j] = "Large"

for word in output:
    if word == None:
        print("Small")
    else:
        print(word)
