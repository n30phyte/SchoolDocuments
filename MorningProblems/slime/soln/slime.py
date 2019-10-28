slimes = int(input())

slime_list = list()

slime_list.append(1)
slimes -= 1

for i in range(slimes):
    slime_list.append(1)
    slimes -= 1
    try:
        while(slime_list[-1] == slime_list[-2]):
            slime_list[-2] = slime_list[-2] + 1
            slime_list.pop()
    except:
        pass

print(*slime_list)