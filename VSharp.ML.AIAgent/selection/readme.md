# How to use selection module:

This module is to help configure function for creating new generations:

```new_gen_func := (ModelResultsOnGameMaps) -> list[Mutable]```

Recomended pipeline for creating such functions is:

## 1. Select models with chosen strategies (+scorers for them if necessary):

```python
my_scorer = scorer.decart_scorer
k_best_models = selectors.select_k_best(
    with_scorer=my_scorer,
    model_result_with_maps=mr,
    k=3,
)

# any custom key function (list[MapResultMapping]) -> T:Comparable
my_other_derived_scorer = lambda mr: scorer.minkowski_scorer(mr, 0.5)
tournament_best_models = selectors.tournament_selection(
    model_result_with_maps=mr,
    desired_population=3,
    n_comparisons=4,
    scorer=my_other_derived_scorer,
)
...
```

## 2. Mutate or/and average selected models with chosen wrapper:

```wrapper = GeneticLearner```

```python
averaged_k_best_models = GeneticLearner.average(k_best_models)

five_best_averaged_models_mutated = [
    GeneticLearner.mutate(
        averaged_k_best_models,
        mutation_volume=0.25,
        mutation_freq=0.1,
    ) for _ in range(5)
]

tournament_average = GeneticLearner.average(tournament_best_models)
tournament_average_mutated = GeneticLearner.mutate(
    tournament_average,
    mutation_volume=0.25,
    mutation_freq=0.1,
)
...
```

## 3. Return assembled generation

```python
assembled = [
    *k_best_models,
    *five_best_averaged_models_mutated,
    tournament_average,
    tournament_average_mutated,
    ...
]

return assembled
```