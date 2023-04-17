import logging
from collections import defaultdict, namedtuple

import pandas as pd

from config import Config
from ml.mutation.classes import ModelResultsOnGameMaps, MutableResult
from ml.mutation.utils import invert_mapping_mrgm_gmmr

MutableNameResultMapping: tuple[str, MutableResult] = namedtuple(
    "MutableNameResultMapping", ["name", "result"]
)


def get_sample_val(d: dict):
    if not d:
        raise RuntimeError("Dict is empty!")

    sample_key = next(iter(d))
    return d[sample_key]


def display_pivot_table(model_map_results_mapping: ModelResultsOnGameMaps):
    map_results_with_models = invert_mapping_mrgm_gmmr(model_map_results_mapping)

    formatted_mappings: defaultdict[str, list[tuple[str, float]]] = defaultdict(list)

    for map_obj, mutable_result_mapping_list in map_results_with_models.items():
        formatted_mappings[map_obj.Id] = list(
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

    maps_indexes = dict(
        {(map_obj.Id, map_obj.MapName) for map_obj in map_results_with_models.keys()}
    )

    for col in df:
        df[col] = df[col].map(
            lambda mutable_name_result_mapping: mutable_name_result_mapping.result
        )
    df.rename(columns=lambda map_id: maps_indexes[map_id], inplace=True)
    logging.info("\n" + df.to_markdown(tablefmt="psql"))
