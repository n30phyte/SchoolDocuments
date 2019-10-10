# read the input
n, m = map(int, input().split())
bitSequence = input()

# solve the problem
for i in range(m):
    s, e = map(int, input().split())
    bits = set(bitSequence[s-1: e])
    if len(bits) == 2:
        print("both")
    elif "1" in bits:
        print("one")
    else:
        print("zero")

# output the answer
