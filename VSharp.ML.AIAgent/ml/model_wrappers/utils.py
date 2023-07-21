import random
import string

from config import FeatureConfig


def gen_name() -> str:
    return "".join(
        random.choices(string.ascii_uppercase + string.digits, k=FeatureConfig.NAME_LEN)
    )
