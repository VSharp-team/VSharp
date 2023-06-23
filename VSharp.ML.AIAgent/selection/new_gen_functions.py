from typing import Callable

from ml.model_wrappers.protocols import Predictor
from selection.classes import AgentResultsOnGameMaps


def keeps_duplicates(
    new_gen_func,
) -> Callable[[AgentResultsOnGameMaps], list[Predictor]]:
    """
    allows to create multiple instances of the same model
    """

    def wrapper(mr: AgentResultsOnGameMaps) -> list[Predictor]:
        selected_models = new_gen_func(mr)
        to_copy: tuple[Predictor, int] = []

        for unique_model in set(selected_models):
            if selected_models.count(unique_model) > 1:
                to_copy.append((unique_model, selected_models.count(unique_model) - 1))

        res: list[Predictor] = []

        for model, copy_count in to_copy:
            last_copied = model
            for _ in range(copy_count):
                new_model = last_copied.copy(last_copied.name() + "*")
                res.append(new_model)
                last_copied = new_model

        res += list(set(selected_models))

        assert len(res) == len(selected_models)
        return res

    return wrapper
