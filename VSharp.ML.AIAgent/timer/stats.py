import statistics


def compute_statistics(inference_times_array: list[float], base_z_score=3):
    mean_inference = statistics.mean(inference_times_array)
    std = statistics.stdev(inference_times_array)

    def z_score_filter(point) -> bool:
        return abs((point - mean_inference) / std) < base_z_score

    inference_times_array = list(filter(z_score_filter, inference_times_array))

    mean_inference_no_outliers = statistics.mean(inference_times_array) * 1000
    std_no_outliers = statistics.stdev(inference_times_array) * 1000

    return mean_inference_no_outliers, std_no_outliers
