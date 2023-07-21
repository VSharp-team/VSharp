from math import floor


def prepare_number(f, bit_num) -> int:
    rv = f
    while floor(rv) != rv:
        rv *= 10
    if rv < 0:
        rv = 2**bit_num + rv
    return int(rv)


def convert_to_big_int(*nums, bit_num=32):
    res = ""

    for num in nums:
        num_prepared = prepare_number(num, bit_num=bit_num)
        num_padded = str(num_prepared).zfill(bit_num)
        res += num_padded
    return int(res)
