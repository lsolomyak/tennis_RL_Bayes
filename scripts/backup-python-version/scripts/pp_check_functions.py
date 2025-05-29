import pandas as pd
import numpy as np
import os
import sys
import seaborn as sns
import matplotlib.pyplot as plt
import json
from pathlib import Path


def load_stan(file):

    # Method 1: Basic reading of a JSON file
    with open(file, "r") as f:
        stan = json.load(f)
    return stan


def filter_data(data):
    data[data["Speed_KMH"].notnull() & (data["Speed_KMH"] > 0)]
    data[data["Speed_KMH"] < 250]
    return data


def get_data():
    """
    Get data from the csv files
    :return: dataframes for 2011-2015 and 2016-2022
    """
    # Load the data
    base_path = Path(__file__).parent.parent.parent.parent
    data_path = base_path / "data" / "processed"

    print(f"Script location: {Path(__file__)}")
    print(f"Base path: {base_path}")
    print(f"Looking for files in: {data_path}")
    print(f"Files that exist: {list(data_path.glob('*')) if data_path.exists() else 'Directory does not exist'}")
   
    csv_2011 = pd.read_parquet(data_path / "csv_2011.parquet")
    csv_2016 = pd.read_parquet(data_path / "csv_2016.parquet")

    csv_2011 = filter_data(csv_2011)
    csv_2016 = filter_data(csv_2016)
    
    sum_2011 = pd.read_parquet(data_path / "full_summary_2011.parquet")
    sum_2016 = pd.read_parquet(data_path / "full_summary_2016.parquet")

    sum_2011.rename(columns={"Unnamed: 0": "parameter"}, inplace=True)
    sum_2016.rename(columns={"Unnamed: 0": "parameter"}, inplace=True)

    stan_2011 = load_stan(
        file="/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2011_2015_stan_2011_uniform_effort.json"
    )
    stan_2016 = load_stan(
        file="/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2016_2022_stan_2016_uniform_effort.json"
    )

    return csv_2011, csv_2016, sum_2011, sum_2016, stan_2011, stan_2016


def get_cumulative_surprise(sum_2011, n_rows):
    sum_2011 = sum_2011.rename(columns={"Unnamed: 0": "parameter"})
    # find the mean-rows
    idx_s = sum_2011["parameter"].str.contains(r"cumulative_surprise_server\[")
    idx_p1 = sum_2011["parameter"].str.contains(r"cumulative_surprise_player1\[")
    idx_p2 = sum_2011["parameter"].str.contains(r"cumulative_surprise_player2\[")

    v_s = sum_2011.loc[idx_s, "Mean"].to_numpy()
    v_p1 = sum_2011.loc[idx_p1, "Mean"].to_numpy()
    v_p2 = sum_2011.loc[idx_p2, "Mean"].to_numpy()
    v_ns = np.where(v_s == v_p1, v_p2, v_p1)

    return pd.DataFrame(
        {
            "cumulative_surprise_server": v_s,
            "cumulative_surprise_non_server": v_ns,
            "cumulative_surprise_player1": v_p1,
            "cumulative_surprise_player2": v_p2,
            "index": np.arange(n_rows),
        }
    )


def add_cumulative(sum_2011, csv_2011):
    surprise_df = get_cumulative_surprise(sum_2011, len(csv_2011))
    cols = [
        "cumulative_surprise_server",
        "cumulative_surprise_non_server",
        "cumulative_surprise_player1",
        "cumulative_surprise_player2",
    ]
    # lookup by 0-based index
    lookup = surprise_df.set_index("index")[cols]
    row_nums = pd.Series(np.arange(len(csv_2011)), index=csv_2011.index)
    for c in cols:
        csv_2011[c] = row_nums.map(lookup[c])
    return csv_2011


def extract_point_params(summary_data):
    params = {
        "alpha": summary_data.loc[summary_data["parameter"] == "alpha", "Mean"].iloc[0],
        "beta_player": summary_data.loc[
            summary_data["parameter"].str.contains(r"^beta_player\["), "Mean"
        ].to_numpy(),
        "beta_match": summary_data.loc[
            summary_data["parameter"].str.contains(r"^beta_match\["), "Mean"
        ].to_numpy(),
        "beta_player_int_dev": summary_data.loc[
            summary_data["parameter"].str.contains(r"^beta_player_int_dev\["), "Mean"
        ].to_numpy(),
        "beta_surprise_player1": summary_data.loc[
            summary_data["parameter"].str.contains(r"^beta_surprise_player1\["), "Mean"
        ].to_numpy(),
        "beta_surprise_player2": summary_data.loc[
            summary_data["parameter"].str.contains(r"^beta_surprise_player2\["), "Mean"
        ].to_numpy(),
    }
    return params


def compute_win_prob(data, params, stan):
    """
    Compute win probabilities
    """

    # Match indexing - equivalent to as.numeric(as.factor(data$match_id)) in R
    # This creates sequential indices starting from 1, then we subtract 1 for Python 0-indexing
    unique_matches = np.unique(data["match_id"])
    match_id_to_index = {match_id: idx for idx, match_id in enumerate(unique_matches)}

    match_indices, _ = pd.factorize(data["match_id"])

    # Player indices (subtract 1 for 0-based indexing)
    server_indices = data["Point_Server_id"].values - 1
    non_server_indices = data["Point_non_Server_id"].values - 1

    # Get progress variable (check which column exists)
    if "p1_control_uniform" in data.columns:
        progress_var = data["p1_control_uniform"].values

    else:
        print("Warning: No progress variable found, using zeros")
        progress_var = np.zeros(len(data))

    # Compute linear predictor
    linear_pred = (
        params["alpha"]
        + params["beta_player"][server_indices]
        - params["beta_player"][non_server_indices]
        + params["beta_match"][match_indices] * data["player1_served"].values
        + (
            params["beta_player_int_dev"][server_indices]
            - params["beta_player_int_dev"][non_server_indices]
        )
        * progress_var
    )

    # Add surprise effects if available
    if "cumulative_surprise_player1" in data.columns:
        linear_pred += (
            params["beta_surprise_player1"] * data["cumulative_surprise_player1"].values
            + params["beta_surprise_player2"]
            * data["cumulative_surprise_player2"].values
        )

    # Convert to probability using logistic function
    win_prob = 1 / (1 + np.exp(-linear_pred))
    print(win_prob.mean(), win_prob.std())
    return win_prob


def create_point_calibration_data(data, n_bins=20):
    """
    Create calibration data for point predictions.
    """
    # Work with clean data from the start
    clean_data = data[["win_prob", "served_and_scored"]].dropna()

    # Check if we have enough data
    if len(clean_data) < 5:
        return None

    # Create bins more efficiently
    predicted_probs = clean_data["win_prob"]
    breaks = np.linspace(predicted_probs.min(), predicted_probs.max(), n_bins + 1)
    clean_data = clean_data.copy()  # Avoid SettingWithCopyWarning
    clean_data["bins"] = pd.cut(predicted_probs, bins=breaks, include_lowest=True)

    # Group and aggregate in one step
    grouped = (
        clean_data.groupby("bins")
        .agg({"win_prob": "mean", "served_and_scored": ["mean", "count"]})
        .reset_index()
    )

    # Flatten column names
    grouped.columns = ["bins", "predicted_prob", "actual_prob", "n_points"]

    # Extract bin edges
    grouped["bin_min"] = grouped["bins"].apply(lambda x: x.left)
    grouped["bin_max"] = grouped["bins"].apply(lambda x: x.right)

    # Calculate standard error
    grouped["se"] = np.sqrt(
        grouped["actual_prob"] * (1 - grouped["actual_prob"])
    ) / np.sqrt(grouped["n_points"])
    grouped["ci_lower"] = np.maximum(0, grouped["actual_prob"] - 1.96 * grouped["se"])
    grouped["ci_upper"] = np.minimum(1, grouped["actual_prob"] + 1.96 * grouped["se"])
    # Return only the columns we need
    return grouped[
        [
            "bin_min",
            "bin_max",
            "predicted_prob",
            "actual_prob",
            "n_points",
            "se",
            "ci_lower",
            "ci_upper",
        ]
    ]


# Check if we have enough data
def create_combined_point_plot(data):
    # Filter data
    filtered_data = data[
        (data["n_points"] > 500)
        & (data["actual_prob"] * 100 >= 50)
        & (data["actual_prob"] * 100 <= 70)
        & (data["predicted_prob"] * 100 > 52)
        & (data["predicted_prob"] * 100 <= 70)
        & (data["ci_lower"] * 100 >= 47)
        & (data["ci_upper"] * 100 <= 70)
    ]

    # Define colors
    colors = {"2011-2015": "#2171B5", "2016-2022": "#C1876B"}

    fig, ax = plt.subplots(figsize=(10, 10))
    sns.set_style("white")
    # Plot points colored by period
    sns.scatterplot(
        data=filtered_data,
        x="predicted_prob",
        y="actual_prob",
        hue="period",  # assuming your period column is named 'period'
        size="n_points",
        sizes=(500, 1000),
        alpha=0.8,
        palette=colors,
    )

    # Add error bars
    for period in filtered_data["period"].unique():
        period_data = filtered_data[filtered_data["period"] == period]
        plt.errorbar(
            period_data["predicted_prob"],
            period_data["actual_prob"],
            yerr=[
                period_data["actual_prob"] - period_data["ci_lower"],
                period_data["ci_upper"] - period_data["actual_prob"],
            ],
            fmt="none",
            ecolor=colors[period],
            capsize=3,
            alpha=0.8,
        )

    # Perfect prediction line
    plt.plot([0.5, 0.7], [0.5, 0.7], color="black", linestyle="--", alpha=1)

    # Formatting
    plt.xlim(0.5, 0.7)
    plt.ylim(0.5, 0.7)
    plt.xlabel("Predicted server's p(win point)", fontsize=20)
    plt.ylabel("Actual server's p(win point)", fontsize=20)

    # Convert to percentage labels
    plt.gca().xaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f"{x*100:.0f}%"))
    plt.gca().yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f"{x*100:.0f}%"))

    plt.xticks(np.arange(0.50, 0.71, 0.05), fontsize=20)
    plt.yticks(np.arange(0.50, 0.71, 0.05), fontsize=20)

    # Clear any existing legend first
    if ax.get_legend():
        ax.get_legend().remove()

    # Create proxy artists for legend
    blue_circle = plt.Line2D(
        [0],
        [0],
        marker="o",
        color="w",
        markerfacecolor="#2171B5",
        markersize=24,
        markeredgecolor="none",
        linestyle="",
    )
    orange_circle = plt.Line2D(
        [0],
        [0],
        marker="o",
        color="w",
        markerfacecolor="#C1876B",
        markersize=24,
        markeredgecolor="none",
        linestyle="",
    )

    ax.legend(
        [blue_circle, orange_circle],
        ["2011-2015", "2016-2022"],
        fontsize=24,
        loc="upper left",
        numpoints=1,
    )
    plt.tight_layout()
    plt.show()


def extract_serve_params(summary_data):
    # Get match parameters and determine number of matches
    match_param_ind = summary_data["parameter"].str.contains(
        r"^beta_serve_speed_match\["
    )
    match_params = summary_data.loc[match_param_ind, "Mean"].to_numpy()
    n_matches = int(len(match_params) / 2)

    # Create params dictionary
    params = {
        # Scalar parameters
        "alpha_serve_speed": summary_data.loc[
            summary_data["parameter"] == "alpha_serve_speed", "Mean"
        ].iloc[0],
        "alpha_serve_speed_raw": summary_data.loc[
            summary_data["parameter"] == "alpha_serve_speed_raw", "Mean"
        ].iloc[0],
        "alpha_serve_speed_int": summary_data.loc[
            summary_data["parameter"] == "alpha_serve_speed_int", "Mean"
        ].iloc[0],
        "beta_serve_speed_surprise": summary_data.loc[
            summary_data["parameter"] == "beta_serve_speed_surprise", "Mean"
        ].iloc[0],
        "beta_serve_speed_surprise_int": summary_data.loc[
            summary_data["parameter"] == "beta_serve_speed_surprise_int", "Mean"
        ].iloc[0],
        "sigma_serve_speed": summary_data.loc[
            summary_data["parameter"] == "sigma_serve_speed", "Mean"
        ].iloc[0],
        # Vector parameters
        "beta_serve_speed_player": summary_data.loc[
            summary_data["parameter"].str.contains(r"^beta_serve_speed_player\["),
            "Mean",
        ].to_numpy(),
        "beta_serve_speed_player_int_dev": summary_data.loc[
            summary_data["parameter"].str.contains(
                r"^beta_serve_speed_player_int_dev\["
            ),
            "Mean",
        ].to_numpy(),
        "beta_serve_number_speed_plus_1": summary_data.loc[
            summary_data["parameter"].str.contains(
                r"^beta_serve_number_speed_plus_1\["
            ),
            "Mean",
        ].to_numpy(),
        "beta_serve_speed_surprise_server": summary_data.loc[
            summary_data["parameter"].str.contains(
                r"^beta_serve_speed_surprise_server\["
            ),
            "Mean",
        ].to_numpy(),
        # Matrix parameter
        "beta_serve_speed_match": match_params.reshape(n_matches, 2, order="C"),
    }

    return params


def compute_serve_speed(data, params, stan_data):

    serve_indices = np.array(stan_data.get("is_serve_speed_indices", []))
    serve_indices = serve_indices - 1  # Convert to 0-based indexing

    # Get all unique player IDs and create mapping to parameter indices
    player_ids_in_data = data.loc[serve_indices, "Point_Server_id"].values
    player_name = np.where(
        data.loc[serve_indices, "player1_served"] == 1,
        data.loc[serve_indices, "player1"].values,
        data.loc[serve_indices, "player2"].values,
    )

    unique_player_ids = np.sort(np.unique(player_ids_in_data))

    # Create mapping: player_id -> parameter_index
    player_id_to_param_idx = {
        player_id: idx for idx, player_id in enumerate(unique_player_ids)
    }

    # Convert player IDs to parameter indices
    param_indices = np.array(
        [player_id_to_param_idx[pid] for pid in player_ids_in_data]
    )

    # Calculate standardization parameters
    speed_mean = data.loc[serve_indices, "Speed_KMH"].mean()
    speed_sd = data.loc[serve_indices, "Speed_KMH"].std()

    # Match indexing
    match_ids = data.loc[serve_indices, "match_id"].values
    unique_matches = np.sort(np.unique(match_ids))
    match_index = np.searchsorted(unique_matches, match_ids)

    # Get other variables
    progress_vals = (
        data.loc[serve_indices, "p1_control_uniform"].values
        if "p1_control_uniform" in data.columns
        else data.loc[serve_indices, "contr"].values
    )
    serve_numbers = data.loc[serve_indices, "ServeNumber"].values
    point_server = data.loc[serve_indices, "PointServer"].values

    # Compute predictions using parameter indices instead of player IDs
    predicted_speeds = (
        params["alpha_serve_speed"]
        + (
            params["alpha_serve_speed_int"]
            + params["beta_serve_speed_player_int_dev"][param_indices]
        )
        * progress_vals
        + params["beta_serve_speed_player"][param_indices]
        + (params["beta_serve_number_speed_plus_1"][param_indices] - 1)
        * (serve_numbers - 1.5)
        + params["beta_serve_speed_match"][match_index, 0] * (2 - point_server)
        + params["beta_serve_speed_match"][match_index, 1] * (point_server - 1)
    )

    # Add surprise effects if available
    if "cumulative_surprise_server" in data.columns:
        predicted_speeds += (
            params["beta_serve_speed_surprise_server"]
            * data.loc[serve_indices, "cumulative_surprise_server"].values
        )

    predicted_speeds_kmh = predicted_speeds * speed_sd + speed_mean

    return {
        "speeds": predicted_speeds_kmh,
        "indices": serve_indices,
        "player_ids": player_ids_in_data,
        "player_names": player_name,
    }


def create_serve_calibration_data(predicted_speeds, actual_speeds, n_bins=20):
    if sum(~np.isnan(predicted_speeds)) < 5:
        return None  # Return None if not enough data

    # Remove NaN values for calculations
    not_na = ~np.isnan(predicted_speeds)
    predicted_speeds_kmh = predicted_speeds[not_na]
    actual_speeds_filtered = actual_speeds[not_na]

    # Create bin breaks
    breaks = np.linspace(
        np.min(predicted_speeds_kmh), np.max(predicted_speeds_kmh), n_bins + 1
    )

    # Create bins using pandas cut
    bins = pd.cut(predicted_speeds_kmh, bins=breaks, include_lowest=True)

    # Instead of manually iterating, use groupby which is more like tapply
    temp_df = pd.DataFrame(
        {
            "predicted": predicted_speeds_kmh,
            "actual": actual_speeds_filtered,
            "bins": bins,
        }
    )

    # Group by bins and calculate statistics
    grouped = temp_df.groupby("bins")

    # Create DataFrame with results
    df = pd.DataFrame(
        {
            "bin_min": breaks[:-1],
            "bin_max": breaks[1:],
            "predicted_speed": grouped["predicted"].mean().values,
            "actual_speed": grouped["actual"].mean().values,
            "n_points": grouped.size().values,
        }
    )

    # Remove rows with NA predicted speeds (equivalent to R's df[!is.na(df$predicted_speed), ])
    df = df.dropna(subset=["predicted_speed"])

    # Calculate standard error and confidence intervals
    # Get standard deviations for each bin
    std_devs = grouped["actual"].std().values

    # Handle potential length mismatch due to empty bins
    if len(std_devs) == len(df):
        df["se"] = std_devs / np.sqrt(df["n_points"])
    else:
        # Map the standard deviations to the correct bins
        se_values = []
        for bin_name in grouped.groups.keys():
            if bin_name in grouped["actual"].std().index:
                se_values.append(
                    grouped["actual"].std()[bin_name]
                    / np.sqrt(len(grouped.groups[bin_name]))
                )
            else:
                se_values.append(np.nan)
        df["se"] = se_values

    # Add confidence intervals
    df["ci_lower"] = df["actual_speed"] - 1.96 * df["se"]
    df["ci_upper"] = df["actual_speed"] + 1.96 * df["se"]

    return df


def create_player_calibration_data_serve(data, predictions, player_id, player_name):
    """
    Create player-specific calibration data.

    Parameters:
    data (pd.DataFrame): Tennis match data
    predictions (dict): Prediction results
    player_id (int): Player ID
    player_name (str): Player name

    Returns:
    pd.DataFrame: Player-specific calibration data
    """
    serve_indices = predictions["indices"]
    predicted_speeds_all = predictions["speeds"]
    player_ids_all = predictions.get(
        "player_ids", data.loc[serve_indices, "Point_Server_id"].values
    )

    # Find which predictions correspond to this player
    player_prediction_mask = player_ids_all == player_id

    if np.sum(player_prediction_mask) < 100:
        print(
            f"Not enough serves for {player_name} (player_id {player_id}): {np.sum(player_prediction_mask)} serves"
        )
        return None

    # Get predicted speeds for this player
    predicted_speeds = predicted_speeds_all[player_prediction_mask]

    # Get corresponding serve indices for this player
    player_serve_indices = serve_indices[player_prediction_mask]

    # Get actual speeds for these serves
    actual_speeds = data.loc[player_serve_indices, "Speed_KMH"].values
    serve_numbers = data.loc[player_serve_indices, "ServeNumber"].values

    # Remove invalid data
    valid_mask = ~(
        pd.isna(predicted_speeds) | pd.isna(actual_speeds) | (actual_speeds <= 0)
    )
    predicted_speeds = predicted_speeds[valid_mask]
    actual_speeds = actual_speeds[valid_mask]
    serve_numbers = serve_numbers[valid_mask]

    if len(predicted_speeds) < 100:
        print(
            f"Not enough valid serves for {player_name} after cleaning: {len(predicted_speeds)} serves"
        )
        return None

    print(f"{player_name}: {len(predicted_speeds)} valid serves")

    # Create bins for each serve number
    bin_stats_list = []
    n_bins = 10

    for serve_num in [1, 2]:
        serve_mask = serve_numbers == serve_num
        if np.sum(serve_mask) < 20:
            continue

        pred_speeds_serve = predicted_speeds[serve_mask]
        actual_speeds_serve = actual_speeds[serve_mask]

        # Create quantile-based breaks
        breaks = np.linspace(
            np.percentile(pred_speeds_serve, 1),
            np.percentile(pred_speeds_serve, 99),
            n_bins + 1,
        )
        bins = pd.cut(pred_speeds_serve, bins=breaks, include_lowest=True)

        # Calculate statistics
        for bin_label in bins.categories:
            mask = bins == bin_label
            if np.sum(mask) == 0:
                continue

            bin_stat = {
                "bin_min": bin_label.left,
                "bin_max": bin_label.right,
                "predicted_speed": np.mean(pred_speeds_serve[mask]),
                "actual_speed": np.mean(actual_speeds_serve[mask]),
                "n_points": np.sum(mask),
                "serve_number": serve_num,
                "player_name": player_name,
                "se": np.std(actual_speeds_serve[mask]) / np.sqrt(np.sum(mask)),
            }
            bin_stat["ci_lower"] = bin_stat["actual_speed"] - 1.96 * bin_stat["se"]
            bin_stat["ci_upper"] = bin_stat["actual_speed"] + 1.96 * bin_stat["se"]
            bin_stats_list.append(bin_stat)

    return pd.DataFrame(bin_stats_list) if bin_stats_list else None


def create_overlay_plot(data1, data2, title1, title2):
    fig, ax = plt.subplots(figsize=(6, 6))

    # Plot 1 (blue) - all data points
    scatter1 = ax.scatter(
        data1["predicted_speed"],
        data1["actual_speed"],
        s=np.interp(
            data1["n_points"],
            [data1["n_points"].min(), data1["n_points"].max()],
            [50, 400],
        ),
        c="#2171B5",
        alpha=0.7,
    )

    # Plot 2 (orange) - all data points
    scatter2 = ax.scatter(
        data2["predicted_speed"],
        data2["actual_speed"],
        s=np.interp(
            data2["n_points"],
            [data2["n_points"].min(), data2["n_points"].max()],
            [50, 400],
        ),
        c="#FFB140",
        alpha=0.7,
    )

    # Error bars only for filtered data (n_points > 200)
    filtered_data1 = data1[data1["n_points"] > 200]
    filtered_data2 = data2[data2["n_points"] > 200]

    if len(filtered_data1) > 0:
        ax.errorbar(
            x=filtered_data1["predicted_speed"],
            y=filtered_data1["actual_speed"],
            yerr=[
                filtered_data1["actual_speed"] - filtered_data1["ci_lower"],
                filtered_data1["ci_upper"] - filtered_data1["actual_speed"],
            ],
            fmt="none",
            ecolor="#2171B5",
            alpha=0.8,
            elinewidth=1,
        )

    if len(filtered_data2) > 0:
        ax.errorbar(
            x=filtered_data2["predicted_speed"],
            y=filtered_data2["actual_speed"],
            yerr=[
                filtered_data2["actual_speed"] - filtered_data2["ci_lower"],
                filtered_data2["ci_upper"] - filtered_data2["actual_speed"],
            ],
            fmt="none",
            ecolor="#FFB140",
            alpha=0.8,
            elinewidth=1,
        )

    # Clear any existing legend first
    if ax.get_legend():
        ax.get_legend().remove()

    # Create proxy artists for legend
    blue_circle = plt.Line2D(
        [0],
        [0],
        marker="o",
        color="w",
        markerfacecolor="#2171B5",
        markersize=8,
        markeredgecolor="none",
        linestyle="",
    )
    orange_circle = plt.Line2D(
        [0],
        [0],
        marker="o",
        color="w",
        markerfacecolor="#FFB140",
        markersize=8,
        markeredgecolor="none",
        linestyle="",
    )

    ax.legend(
        [blue_circle, orange_circle],
        [title1, title2],
        fontsize=12,
        loc="upper left",
        numpoints=1,
    )

    # Styling
    ax.set_xlabel("Predicted Speed (km/h)", fontsize=20)
    ax.set_ylabel("Actual Speed (km/h)", fontsize=20)
    ax.set_title("Serve Speed", fontsize=30)
    ax.set_xlim(140, 200)
    ax.set_ylim(140, 200)
    ax.set_xticks(range(140, 201, 20))
    ax.set_yticks(range(140, 201, 20))
    ax.axline((0, 0), slope=1, linestyle="--", color="gray", alpha=0.7)
    ax.set_aspect("equal")
    ax.set_facecolor("white")
    fig.patch.set_facecolor("white")
    ax.grid(False)

    plt.tight_layout()
    plt.show()
    return ax


def create_player_calibration_plot(data, player_name, figsize=(8, 8)):
    """
    Create player-specific calibration plot.

    Parameters:
    data (pd.DataFrame): Player calibration data
    player_name (str): Player name
    figsize (tuple): Figure size

    Returns:
    matplotlib.figure.Figure: The created figure
    """
    data_filtered = data[
        (data["player_name"] == player_name) & (data["n_points"] >= 20)
    ].copy()

    if len(data_filtered) == 0:
        return None

    fig, ax = plt.subplots(figsize=figsize)

    # Perfect calibration line
    ax.plot([140, 200], [140, 200], "--", color="gray", alpha=0.7, linewidth=2)
    markers = {1: "o", 2: "o"}  # Both circles, but use fillstyle parameter
    fillstyles = {1: "full", 2: "none"}  # Filled vs empty

    # Plot for each dataset and serve number combination
    colors = {"2011-2015": "#2171B5", "2016-2022": "#FFB140"}
    #  colors=np.where(initial==1, colors['2011-2015'], colors['2016-2022'])

    for dataset in data_filtered["dataset"].unique():
        for serve_num in data_filtered["serve_number"].unique():
            subset = data_filtered[
                (data_filtered["dataset"] == dataset)
                & (data_filtered["serve_number"] == serve_num)
            ]

            if len(subset) == 0:
                continue

            # Determine fill
            facecolor = colors[dataset] if serve_num == 1 else "white"
            edgecolor = colors[dataset]

            ax.scatter(
                subset["predicted_speed"],
                subset["actual_speed"],
                s=subset["n_points"] / 5,
                c=facecolor,
                marker=markers[serve_num],
                alpha=0.8,
                edgecolors=edgecolor,
                linewidth=2,
            )

            ax.errorbar(
                subset["predicted_speed"],
                subset["actual_speed"],
                yerr=[
                    subset["actual_speed"] - subset["ci_lower"],
                    subset["ci_upper"] - subset["actual_speed"],
                ],
                fmt="none",
                color=edgecolor,
                alpha=0.8,
                capsize=2,
            )

    # Formatting
    ax.set_xlim(140, 200)
    ax.set_ylim(140, 200)
    ax.set_xticks(range(140, 201, 20))
    ax.set_yticks(range(140, 201, 20))
    ax.set_xlabel("Predicted Serve Speed (km/h)", fontsize=14)
    ax.set_ylabel("Actual Serve Speed (km/h)", fontsize=14)
    ax.set_title(player_name, fontsize=18, pad=15)
    ax.set_facecolor("white")
    fig.patch.set_facecolor("white")
    ax.grid(False)

    plt.tight_layout()
    ax.set_aspect("equal")

    plt.tight_layout()
    return fig
# pp_Serve.py
