# python3

def poly_hash(string, m):
    sum_polies = sum(ord(s) * (263**i) for i, s in enumerate(string))
    return (sum_polies % 1000000007) % m

def add(table, query, n_buckets):
    hvalue = poly_hash(query, n_buckets)
    chain = table[hvalue]
    if query not in chain:
        chain.append(query)
    return table

def delete(table, query, n_buckets):
    hvalue = poly_hash(query, n_buckets)
    chain = table[hvalue]
    if query in chain:
        chain.remove(query)
    return table

def find(table, query, n_buckets):
    hvalue = poly_hash(query, n_buckets)
    stdout = 'yes' if query in table[hvalue] else 'no'
    print(stdout)
    return table

def check(table, query, *args):
    hvalue = int(query)
    chain = table[hvalue]
    print(' '.join(reversed(chain)))
    return table

if __name__ == '__main__':
    parse_fns = {'add': add, 'del': delete, 'find': find, 'check': check}
    n_buckets = int(input())
    table = [[] for _ in range(n_buckets)]

    n_queries = int(input())
    for _ in range(n_queries):
        request, query = input().split(' ')
        table = parse_fns[request](table, query, n_buckets)

'''
5
12
add world
add HellO
check 4
find World
find world
del world
check 4
del HellO
add luck
add GooD
check 2
del good

4
8
add test
add test
find test
del test
find test
find Test
add Test
find Test
'''

