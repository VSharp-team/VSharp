from collections import defaultdict

import pandas as pd
from logger.setup import table_logger
from ml.mutation_gen import GameMapsModelResults
from tabulate import tabulate

logger = table_logger()


def display_pivot_table(r: GameMapsModelResults):
    new_r = defaultdict(list)

    for map_obj, mutable_result_mapping_list in r.items():
        for mutable_result_mapping in mutable_result_mapping_list:
            mutable_name = mutable_result_mapping.mutable.name()
            mutable_coverage = mutable_result_mapping.mutable_result.coverage_percent

            new_r[map_obj.NameOfObjectToCover].append(
                (mutable_name, round(mutable_coverage, 2))
            )

    df = pd.DataFrame(new_r)
    for col in df:
        df[col] = df[col].sort_values(
            ignore_index=True, key=lambda col: col.map(lambda x: x[1]), ascending=False
        )
    logger.info(tabulate(df, headers="keys", tablefmt="psql"))
