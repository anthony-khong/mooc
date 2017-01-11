# python3
import copy

SWAPS = []

def one_to_zero_indexing(fn):
    def wrapped(ix):
        return fn(ix + 1) - 1
    return wrapped

@one_to_zero_indexing
def left(ix):
    return 2 * ix

@one_to_zero_indexing
def right(ix):
    return 2*ix + 1

def build_heap(integers):
    integers = copy.copy(integers)
    n_sift_downs = int(len(integers) / 2) + 1
    for i in range(n_sift_downs):
        integers = sift_down(integers, n_sift_downs - i - 1)
    return integers

def sift_down(integers, ix):
    candidate_ixs = [ix, left(ix), right(ix)]
    min_ix = select_min_ix(integers, candidate_ixs)
    if ix == min_ix:
        return integers
    else:
        integers = swap_ixs(integers, ix, min_ix)
        return sift_down(integers, min_ix)

def select_min_ix(integers, candidate_ixs):
    n = len(integers)
    ixs = [ix for ix in candidate_ixs if ix < n]
    values = [integers[ix] for ix in ixs]
    return sorted(zip(values, ixs))[0][1]

def swap_ixs(integers, ix, swap_ix):
    SWAPS.append((ix, swap_ix))
    integers[swap_ix], integers[ix] = integers[ix], integers[swap_ix]
    return integers

if __name__ == '__main__':
    _ = input()
    integers = [int(s) for s in input().split()]
    # integers = [14, 3, 18, 12, 12, 16, 14, 0, 0, 10, 8, 1, 7, 16, 7]
    # integers = []
    integers = build_heap(integers)

    print(len(SWAPS))
    for ix, swap_ix in SWAPS:
        print('{} {}'.format(ix, swap_ix))
    # print(integers)

'''
def assert_is_heap(integers):
    n = len(integers)
    for ix, integer in enumerate(integers):
        for child in [left, right]:
            if child(ix) < n:
                assert integer <= integers[child(ix)]

if __name__ == '__main__':
    import numpy as np
    for i in xrange(10000):
        n = np.random.randint(0, i + 1)
        integers = np.random.randint(0, 10, n)
        xs = build_heap(integers)
        assert_is_heap(xs)
'''
