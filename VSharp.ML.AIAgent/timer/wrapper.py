from functools import wraps
from time import time

EPOCH_TIMES = []
MAP_TIMES = []


def timeit(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time()
        rv = func(*args, **kwargs)
        end = time()
        EPOCH_TIMES.append(end - start)
        MAP_TIMES.append(end - start)
        return rv

    return wrapper
