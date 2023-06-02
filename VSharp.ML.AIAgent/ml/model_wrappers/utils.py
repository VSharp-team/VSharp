import random
import string

from config import Config


def gen_name() -> str:
    return "".join(
        random.choices(string.ascii_uppercase + string.digits, k=Config.NAME_LEN)
    )
