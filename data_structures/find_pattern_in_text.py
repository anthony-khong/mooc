# python3

from random import randint

BIG_PRIME = 100000007

def rabin_karp(text, pattern):
    x = randint(1, BIG_PRIME - 1)
    results = []
    phash = poly_hash(pattern, x)
    hashes = precompute_hashes(text, len(pattern), x)
    for i, hvalue in enumerate(hashes):
        if phash != hvalue:
            continue
        if text[i:i+len(pattern)] == pattern:
            results.append(i)
    return results

def precompute_hashes(text, len_pattern, x):
    adj_factor = get_adj_factor(len_pattern, x)
    init_str = text[-len_pattern:]
    hashes = [poly_hash(init_str, x)]
    for i in range(len(text) - len_pattern):
        in_char = text[-(i+len_pattern+1)]
        out_char = text[-(i+1)]
        new_hash = (x*hashes[-1] + ord(in_char)
                    - adj_factor*ord(out_char)) % BIG_PRIME
        hashes.append(new_hash)
    return reversed(hashes)

def get_adj_factor(len_pattern, x):
    adj_factor = 1
    for _ in range(len_pattern):
        adj_factor = (adj_factor*x) % BIG_PRIME
    return adj_factor

def poly_hash(string, x):
    # sum_polies = sum(ord(s) * (x**i) for i, s in enumerate(string))
    # return sum_polies % BIG_PRIME
    hvalue = 0
    for s in reversed(string):
        hvalue = (hvalue*x + ord(s)) % BIG_PRIME
    return hvalue

if __name__ == '__main__':
    pattern = input()
    text = input()
    results = rabin_karp(text, pattern)
    print(' '.join(str(x) for x in results))
