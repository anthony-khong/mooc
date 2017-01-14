# python3
from queue import PriorityQueue

if __name__ == '__main__':
    (n_threads, _) = tuple([int(s) for s in input().split()])
    job_times = [int(s) for s in input().split()]

    queue = PriorityQueue()
    for i in range(n_threads):
        queue.put((0, i))

    for job_time in job_times:
        (finish_time, thread_id)  = queue.get()
        print('{} {}'.format(thread_id, finish_time))
        queue.put((finish_time + job_time, thread_id))
