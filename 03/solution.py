with open('input.txt') as fp:
    rugsacks = fp.read().strip().split('\n')


def get_score(rugsacks):
    return sum(
        ord(x) - 38 if x.isupper() else ord(x) - 96
        for x in [next(iter(set(a) & set(b) & set(c or a + b))) for a, b, c in rugsacks]
    )


# Part I
print(get_score((x[:len(x) // 2], x[len(x) // 2:], None) for x in rugsacks))

# Part II
print(
    get_score(
        (rugsacks[i], rugsacks[i + 1], rugsacks[i + 2]) for i in range(len(rugsacks)) if not i % 3
    )
)
