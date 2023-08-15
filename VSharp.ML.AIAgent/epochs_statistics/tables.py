from collections import defaultdict
from pathlib import Path

import pandas as pd

from common.classes import Agent2Result, AgentResultsOnGameMaps
from common.strings import (
    AV_COVERAGE_COL_NAME,
    COV_DEVIATION_COL_NAME,
    EUC_DIST2FULL_COV_COL_NAME,
    MEDIAN_COVERAGE_COL_NAME,
)
from common.utils import invert_mapping_mrgm_gmmr
from config import FeatureConfig
from epochs_statistics.common import Interval, Name2ResultViewModel
from epochs_statistics.gen_stats import compute_euc_dist_to_full_coverage


def get_sample_val(d: dict):
    if not d:
        raise RuntimeError("Dict is empty!")

    sample_key = next(iter(d))
    return d[sample_key]


def create_stats(
    model_map_results_mapping: AgentResultsOnGameMaps,
) -> tuple[list[float], list[float], list[float], list[Interval]]:
    euc_dists2full_cov = []
    avs = []
    medians = []
    intervals = []

    for _, map2result_mapping_list in model_map_results_mapping.items():
        stats = compute_euc_dist_to_full_coverage(map2result_mapping_list)
        euc_dists2full_cov.append(float(stats.euc_dist2_full_cov))
        avs.append(float(stats.average_cov))
        medians.append(float(stats.median_cov))
        intervals.append(stats.interval.pretty())

    return euc_dists2full_cov, avs, medians, intervals


def create_pivot_table(
    model_map_results_mapping: AgentResultsOnGameMaps, sort: bool = True
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    map_results_with_models = invert_mapping_mrgm_gmmr(model_map_results_mapping)
    euc_dists2full_cov, avs, medians, intervals = create_stats(
        model_map_results_mapping
    )

    name_results_dict: defaultdict[int, list[Name2ResultViewModel]] = defaultdict(list)
    epoch_percents_dict: defaultdict[int, list[Name2ResultViewModel]] = defaultdict(
        list
    )

    for map_obj, mutable2result_list in map_results_with_models.items():
        for mutable2result in mutable2result_list:
            name_results_dict[map_obj.Id].append(convert_to_view_model(mutable2result))
            epoch_percents_dict[map_obj.Id].append(
                str(
                    (
                        mutable2result.game_result.actual_coverage_percent,
                        mutable2result.game_result.tests_count,
                        mutable2result.game_result.errors_count,
                        mutable2result.game_result.steps_count,
                    )
                )
            )

    mutable_names = get_model_names_in_order(name_results_dict)
    df = pd.DataFrame(name_results_dict, index=mutable_names)
    epochs_percent_df = pd.DataFrame(epoch_percents_dict, index=mutable_names)
    for col in df:
        df[col] = df[col].map(lambda name2result_vm: name2result_vm.pretty_result)

    maps_indexes = dict(
        {(map_obj.Id, map_obj.MapName) for map_obj in map_results_with_models.keys()}
    )

    df.rename(columns=lambda map_id: maps_indexes[map_id], inplace=True)
    epochs_percent_df.rename(columns=lambda map_id: maps_indexes[map_id], inplace=True)
    df[EUC_DIST2FULL_COV_COL_NAME] = euc_dists2full_cov
    df[AV_COVERAGE_COL_NAME] = avs
    df[MEDIAN_COVERAGE_COL_NAME] = medians
    df[COV_DEVIATION_COL_NAME] = intervals
    if sort:
        df.sort_values(by=[EUC_DIST2FULL_COV_COL_NAME], inplace=True)

    stats_df = df[
        [
            EUC_DIST2FULL_COV_COL_NAME,
            AV_COVERAGE_COL_NAME,
            MEDIAN_COVERAGE_COL_NAME,
            COV_DEVIATION_COL_NAME,
        ]
    ].copy()
    df.drop([EUC_DIST2FULL_COV_COL_NAME], axis=1, inplace=True)
    df.drop([AV_COVERAGE_COL_NAME], axis=1, inplace=True)
    df.drop([MEDIAN_COVERAGE_COL_NAME], axis=1, inplace=True)
    df.drop([COV_DEVIATION_COL_NAME], axis=1, inplace=True)
    return df, stats_df, epochs_percent_df


def table_to_string(table: pd.DataFrame):
    return table.to_markdown(tablefmt="psql")


def table_to_csv(table: pd.DataFrame, path: Path):
    table.to_csv(path_or_buf=path)


def convert_to_view_model(
    m2r_mapping: Agent2Result,
) -> Name2ResultViewModel:
    return Name2ResultViewModel(
        model_name=m2r_mapping.agent.name(),
        pretty_result=m2r_mapping.game_result.printable(
            verbose=FeatureConfig.VERBOSE_TABLES
        ),
    )


def get_model_names_in_order(
    name_results_dict: defaultdict[str, list[Name2ResultViewModel]]
) -> list[str]:
    models_list_sample = get_sample_val(name_results_dict)
    names = [name2result.model_name for name2result in models_list_sample]
    return names
