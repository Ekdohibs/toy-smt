import sys
from random import randrange, choice, sample

N = 25
M = 500
K = 3

values = [randrange(int(N ** 0.5)) for i in range(N)]

print("p cnf %d %d" % (N, M))
for i in range(M):
    l = []
    #u = -1
    u = randrange(K)
    for c in range(K):
        a, b = sample(list(range(1, N + 1)), 2)
        if c == u:
            s = ["<>", "="][values[a - 1] == values[b - 1]]
        else:
            s = choice(["=", "<>"])
        l.append("%d%s%d" % (a, s, b))
    print(" ".join(l))
