# python3

class DisjointSet(object):
    def __init__(self, size):
        self.size = size
        self.parents = []
        self.ranks = []

        for i in range(size):
            self.make_set(i)

    def make_set(self, i):
        self.parents.append(i)
        self.ranks.append(0)

    def find(self, i):
        if self.parents[i] != i:
            self.parents[i] = self.find(self.parents[i])
        return self.parents[i]

    def union(self, i, j):
        par_i, par_j = self.find(i), self.find(j)
        rank_i, rank_j = self.ranks[par_i], self.ranks[par_j]
        if par_i == par_j:
            pass
        elif rank_i > rank_j:
            self.parents[par_j] = par_i
        else:
            self.parents[par_i] = par_j
            if rank_i == rank_j:
                self.ranks[par_j] = rank_j + 1

def read_integers():
    return [int(x) for x in input().split()]

if __name__ == '__main__':
   n_tables, n_merges = read_integers()
   n_rows = read_integers()
   merges = [read_integers() for _ in range(n_merges)]

   ds = DisjointSet(n_tables)
   max_rows = max(n_rows)
   for i, j in merges:
       i, j = i - 1, j - 1
       par_i, par_j = ds.find(i), ds.find(j)
       if par_i != par_j:
           ds.union(i, j)
           new_parent = ds.find(i)
           merged_n_rows = n_rows[par_i] + n_rows[par_j]
           n_rows[new_parent] = merged_n_rows
           if merged_n_rows > max_rows:
               max_rows = merged_n_rows
       print(max_rows)
