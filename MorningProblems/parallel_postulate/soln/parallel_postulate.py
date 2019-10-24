# read in the input
x1, y1, x2, y2 = map(int, input().split())
x3, y3, x4, y4 = map(int, input().split())
try:
    m1 = (y2 - y1) / (x2 - x1)
    m2 = (y4 - y3) / (x4 - x3)
except:
    print("parallel")
else:
    if m1 == m2:
        print("parallel")
    else:
        print("not parallel")