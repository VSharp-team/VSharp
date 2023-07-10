from time import perf_counter

EPOCH_TIMES = []
MAP_TIMES = []


def timeit(func):
    def wrapper(*args, **kwargs):
        start = perf_counter()
        rv = func(*args, **kwargs)
        end = perf_counter()
        EPOCH_TIMES.append(end - start)
        MAP_TIMES.append(end - start)
        return rv

    return wrapper
