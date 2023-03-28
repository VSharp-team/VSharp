from .classes import MutableResultMapping


def sort_by_reward_asc_steps_desc(
    mutable_mapping: MutableResultMapping,
):
    # sort by <MoveRewardReward, -StepsCount (less is better)>
    result = mutable_mapping.mutable_result
    return (
        result.move_reward.ForCoverage,
        result.move_reward.ForVisitedInstructions,
        -result.steps_count,
    )
