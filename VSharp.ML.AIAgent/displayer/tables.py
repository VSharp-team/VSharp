from collections import defaultdict, namedtuple

import pandas as pd
from logger.setup import table_logger
from ml.mutation_gen import GameMapsModelResults, MutableResult

from config import Config

MutableNameResultMapping: tuple[str, MutableResult] = namedtuple(
    "MutableNameResultMapping", ["name", "result"]
)

logger = table_logger()


def get_sample_val(d: dict):
    if not d:
        raise RuntimeError("Dict is empty!")

    sample_key = next(iter(d))
    return d[sample_key]


def display_pivot_table(input_map_models_mappings: GameMapsModelResults):
    formatted_mappings: defaultdict[str, list[tuple[str, float]]] = defaultdict(list)

    for map_obj, mutable_result_mapping_list in input_map_models_mappings.items():
        formatted_mappings[map_obj.NameOfObjectToCover] = list(
            map(
                lambda mutable_result_mapping: MutableNameResultMapping(
                    mutable_result_mapping.mutable.name(),
                    mutable_result_mapping.mutable_result.printable(
                        Config.VERBOSE_TABLES
                    ),
                ),
                mutable_result_mapping_list,
            )
        )

    models_list_sample = get_sample_val(formatted_mappings)
    indices = [name_result_mapping.name for name_result_mapping in models_list_sample]
    df = pd.DataFrame(formatted_mappings, index=indices)
    df.sort_index(inplace=True)

    for col in df:
        df[col] = df[col].map(
            lambda mutable_name_result_mapping: mutable_name_result_mapping.result
        )
    logger.info(df.to_markdown(tablefmt="psql"))
