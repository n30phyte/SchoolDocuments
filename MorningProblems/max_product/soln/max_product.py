n = int(input())

vector_one = list(map(int, input().split()))
vector_two = list(map(int, input().split()))

vector_one.sort()
vector_two.sort()

running_sum = 0

for index in range(n):
    running_sum += vector_one[index] * vector_two[index]

print(running_sum)