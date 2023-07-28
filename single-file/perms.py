#!/usr/bin/env python

# ============================================================
# Helpers
# ============================================================


def printl(list):
    for i in list:
        print(i, end=" ")
    print()


def zamien(lista, i, j):
    tmp = lista[i]
    lista[i] = lista[j]
    lista[j] = tmp


def reset(lista, i):
    j = len(lista)-1
    while i < j:
        zamien(lista, i, j)
        i += 1
        j -= 1


# ============================================================
# Permutations of n-element set
# ============================================================


def permsr(n, s=True):
    lista = [i for i in range(n)]
    if s:
        lista = set(lista)
        __perms1s(lista)
    else:
        __perms1l(lista)


def __perms1l(lista, perm=[]):
    if len(lista) == 0:
        printl(perm)
        return
    for i in range(len(lista)):
        __perms1l(lista[:i]+lista[i+1:], perm+[lista[i]])


def __perms1s(s, perm=[]):
    if len(s) == 0:
        printl(perm)
        return
    for i in s:
        s.pop()
        __perms1s(s.copy(), perm+[i])
        s.add(i)


def permsi(n):
    lista = [i for i in range(n)]

    while len(lista) != 0:
        printl(lista)
        lista = nperm(lista)


def nperm(lista):
    lm = len(lista)-1
    i = lm

    while i > 0 and lista[i-1] > lista[i]:
        i -= 1

    i -= 1
    if i < 0:
        return []
    j = lm

    while lista[j] < lista[i]:
        j -= 1

    zamien(lista, i, j)
    reset(lista, i+1)
    return lista


# ============================================================
# All subsets of set
# ============================================================

def wkombr(n, i=1, list=[]):
    if i > n:
        printl(list)
    else:
        wkombr(n, i+1, list)
        wkombr(n, i+1, list+[i])


def wkombi(n):
    max = 2**n
    for i in range(max):
        imask(i, n)


def imask(m, n):
    for i in range(1, n+1):
        if m & 1:
            print(i, end="")
        m >>= 1
    print()

# ============================================================
# Combinations without repetition
# ============================================================


def kzn1(n, k, i=0, list=[]):
    if len(list) == k:
        printl(list)
    else:
        for j in range(i, n):
            kzn1(n, k, j+1, list+[j])


def kzn2(n, k):
    list = [i for i in range(k)]
    printl(list)

    while nkzn(n, k, list):
        printl(list)


def nkzn(n, k, list):
    i = k-1
    if list[i] < n-1:
        list[i] += 1
        return True

    while i > 0 and list[i-1] == list[i]-1:
        i -= 1
    i -= 1
    if i < 0:
        return False

    list[i] += 1
    for j in range(i+1, k):
        list[j] = list[j-1]+1
    return True


# ============================================================
# Combinations with repetition
# ============================================================


def pkzn1(n, k, i=0, list=[], p=0):
    if i == k:
        printl(list)
        return
    for j in range(p, n):
        pkzn1(n, k, i+1, list+[j], j)


def pkzn2(n, k):
    list = [0 for i in range(k)]
    printl(list)
    while npkzn(n-1, k, list):
        printl(list)


def npkzn(n, k, list):
    i = k-1
    while i >= 0 and list[i] >= n:
        i -= 1

    if i < 0:
        return False
    list[i] += 1
    i += 1

    while i < k:
        list[i] = list[i-1]
        i += 1
    return True

# ============================================================
# Long awaited __name__ == "__main__"
# ============================================================


if __name__ == "__main__":
    tokens = input().split()
    n = int(tokens[0])

    if len(tokens) > 1:
        k = int(tokens[1])
    else:
        k = int(input().split()[0])

    pkzn1(n, k)
