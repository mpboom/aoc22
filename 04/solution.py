with open('input.txt') as fp:
    data = [
        tuple(set(range(int(y.split('-')[0]), int(y.split('-')[1]) + 1)) for y in x.split(','))
        for x in fp.read().strip().splitlines()
    ]


# Part I
print(sum(1 for a, b in data if a | b in (a, b)))

# Part II
print(sum(1 for a, b in data if len(a & b)))
