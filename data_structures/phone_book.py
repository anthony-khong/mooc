# python3

if __name__ == '__main__':
    n = int(input())

    phone_book = {}
    for _ in range(n):
        query = input().split(' ')
        if query[0] == 'add':
            phone_book[query[1]] = query[2]
        elif query[0] == 'find':
            print(phone_book.get(query[1], 'not found'))
        else:
            if query[1] in phone_book:
                phone_book.pop(query[1])

'''
12
add 911 police
add 76213 Mom
add 17239 Bob
find 76213
find 910
find 911
del 910
del 911
find 911
find 76213
add 76213 daddy
find 76213

8
find 3839442
add 123456 me
add 0 granny
find 0
find 123456
del 0
del 0
find 0
'''
