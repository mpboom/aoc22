from collections import defaultdict
from operator import mul, add
import math


with open('input.txt') as fp:
    data = fp.read().strip().split('\n\n')


def solve(iterations):
    inspections = defaultdict(int)
    monkeys = [
        (
            list(map(lambda x: {m: int(x) for m in range(8)}, a.split(': ')[-1].split(', '))),
            add if '+' in b else mul,
            None if b.endswith('old') else int(b.split(' ')[-1]),
            int(c.split(' ')[-1]),
            int(d.split(' ')[-1]),
            int(e.split(' ')[-1]),
        )
        for _, a, b, c, d, e in map(lambda x: x.split('\n'), data)
    ]
    for _ in range(iterations):
        for (i, monkey) in enumerate(monkeys):
            items, op, val, div, true, false = monkeys[i]
            inspections[i] += len(items)
            for level in items:
                new_level = {}
                for monkey, old_val in level.items():
                    old_val = old_val % monkeys[monkey][3] if iterations > 20 else old_val
                    new_level[monkey] = op(val or old_val, old_val) \
                                        if iterations > 20 \
                                        else math.floor(op(val or old_val, old_val) / 3)
                monkeys[true if not new_level[i] % div else false][0].append(new_level)
            items.clear()
    return mul(*list(sorted(inspections.values(), reverse=True))[:2])

# Part I
print(solve(20))

# Part II
print(solve(10000))
