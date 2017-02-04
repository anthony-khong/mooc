# python3

if __name__ == '__main__':
    string = input()
    n_queries = int(input())
    for _ in range(n_queries):
        i, j, k = [int(x) for x in input().split()]
        left, mid, right = string[:i], string[i:(j+1)], string[(j+1):]
        string = left + right
        left, right = string[:k], string[k:]
        string = left + mid + right
    print(string)

