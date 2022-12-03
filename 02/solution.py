RESULTS = {
    (1, 1): 3,
    (2, 1): 1,
    (3, 1): 2,
    (1, 3): 2,
    (2, 3): 3,
    (3, 3): 1,
}


with open('input.txt') as fp:
    data = [
        (ord(a) - 64, ord(b) - 87) for a, b in map(
            lambda x: tuple(x.split(' ')),
            [x for x in fp.read().strip().split('\n')],
        )
    ]


def get_score(data):
    return sum([b + (3 if a == b else (6 if b == (a % 3) + 1 else 0)) for a, b in data])


# Part I
print(get_score(data))

## Part II
print(get_score(map(lambda x: (x[0], x[0]) if x[1] == 2 else (x[0], RESULTS[x]), data)))
