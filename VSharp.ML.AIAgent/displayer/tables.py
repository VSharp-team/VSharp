from collections import defaultdict

import pandas as pd

from config import Config
from displayer.common import Name2ResultViewModel
from displayer.gen_stats import get_av_coverage
from selection.classes import ModelResultsOnGameMaps, Mutable2Result
from selection.utils import invert_mapping_mrgm_gmmr


def get_sample_val(d: dict):
    if not d:
        raise RuntimeError("Dict is empty!")

    sample_key = next(iter(d))
    return d[sample_key]


def create_pivot_table(model_map_results_mapping: ModelResultsOnGameMaps) -> str:
    av_coverages = []
    intervals = []

    for map_obj, map2result_mapping_list in model_map_results_mapping.items():
        stats = get_av_coverage(map2result_mapping_list)
        av_coverages.append(float(stats.av_coverage))
        intervals.append(stats.interval.pretty())

    map_results_with_models = invert_mapping_mrgm_gmmr(model_map_results_mapping)

    name_results_dict: defaultdict[int, list[Name2ResultViewModel]] = defaultdict(list)

    for map_obj, mutable2result_list in map_results_with_models.items():
        for mutable2result in mutable2result_list:
            name_results_dict[map_obj.Id].append(convert_to_view_model(mutable2result))

    mutable_names = get_model_names_in_order(name_results_dict)
    df = pd.DataFrame(name_results_dict, index=mutable_names)
    for col in df:
        df[col] = df[col].map(lambda name2result_vm: name2result_vm.pretty_result)

    maps_indexes = dict(
        {(map_obj.Id, map_obj.MapName) for map_obj in map_results_with_models.keys()}
    )

    df.rename(columns=lambda map_id: maps_indexes[map_id], inplace=True)
    df["av. coverage %"] = av_coverages

    df.sort_values(by=["av. coverage %"], inplace=True)
    df.drop(["av. coverage %"], axis=1)

    return df.to_markdown(tablefmt="psql")


def convert_to_view_model(
    m2r_mapping: Mutable2Result,
) -> Name2ResultViewModel:
    return Name2ResultViewModel(
        model_name=m2r_mapping.mutable.name(),
        pretty_result=m2r_mapping.game_result.printable(Config.VERBOSE_TABLES),
    )


def get_model_names_in_order(
    name_results_dict: defaultdict[str, list[Name2ResultViewModel]]
) -> list[str]:
    models_list_sample = get_sample_val(name_results_dict)
    names = [name2result.model_name for name2result in models_list_sample]
    return names
