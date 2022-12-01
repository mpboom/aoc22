with open('input.txt') as fp:
    data = list(map(lambda x: sum(int(y) for y in x.split('\n')), fp.read().strip().split('\n\n')))


# Part I
print(max(data))

# Part II
print(sum(sorted(data, reverse=True)[:3]))
