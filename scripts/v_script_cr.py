import os
import numpy as np
import pandas as pd
from cmdstanpy import CmdStanModel
import logging
import cmdstanpy
import json
import shutil
import re 

# Set up logging
logging.basicConfig(level=logging.DEBUG, format='%(asctime)s - %(levelname)s - %(message)s')

# Print version information
logging.info(f"CmdStanPy version: {cmdstanpy.__version__}")
logging.info(f"CmdStan version: {cmdstanpy.cmdstan_version()}")

# Print relevant environment variables
logging.info("Relevant environment variables:")
for var in ['CMDSTAN', 'TBB_CXX', 'STAN_THREADS']:
    logging.info(f"{var}: {os.environ.get(var, 'Not set')}")

# Load your data
current_dir = os.getcwd()
data_dir = os.path.join(current_dir, 'data')
with open(os.path.join(data_dir, "data_2016_2022_stan_2016_uniform_effort.json"), "r") as f:
    data = json.load(f)


# Add this right after loading the JSON data and before creating stan_data
# Convert specific problematic arrays to float64 numpy arrays
# Convert the data into a dictionary suitable for cmdstanpy
# Convert the data into a dictionary suitable for cmdstanpy
#stan_data = {key: np.array(value) if isinstance(value, list) else value for key, value in data.items()}
# In your Python code, change this:
#stan_data = {key: np.array(value) if isinstance(value, list) else value for key, value in data.items()}

# To this (adding special handling for scalar values meant to be integers):
stan_data = {}
for key, value in data.items():
    if key in ['n_score', 'n_matches', 'n_players', 'n_players_enough', 'n_serve_speed', 
               'n_serve_speed_matches', 'n_players_with_serve_speed', 
               'n_players_with_serve_speed_enough'] and isinstance(value, list) and len(value) == 1:
        stan_data[key] = value[0]  # Convert 1-element list to scalar
    else:
        stan_data[key] = np.array(value) if isinstance(value, list) else value
# Log info about the data
for key, value in stan_data.items():
    if isinstance(value, np.ndarray):
        logging.info(f"{key}: shape={value.shape}, min={np.min(value)}, max={np.max(value)}")
    else:
        logging.info(f"{key}: {value}")
# Convert the data into a dictionary suitable for cmdstanpy
# Log info about the data

# Initialize and run Stan model
model_name = 'control_effort_uniform_2016'
model_path = os.path.join(current_dir, 'stan_models', f'{model_name}.stan')
StanModel = CmdStanModel(stan_file=model_path)

# Create model-specific output directory
#model_output_dir = os.path.join('./stan_results', model_name,'april17_2011_sd_uniform')
model_output_dir = '/sci/labs/eran.eldar/levi.solomyak/stan_results/sd_2016_take_2unchanged_mean_centered'
os.makedirs(model_output_dir, exist_ok=True)

logging.info("Starting sampling process...")

StanFit = StanModel.sample(
    data=stan_data,
    iter_warmup=1000,
    iter_sampling=500,
    chains=10,
    parallel_chains=10,
    max_treedepth=8,
    adapt_delta=0.85,
    threads_per_chain=1,
    show_progress=False,
    show_console=False,
    save_warmup=False,
    output_dir=model_output_dir
)
logging.info("Sampling completed successfully.")


# Get full summary
try:
    full_summary = StanFit.summary()
    logging.info(f"Full model summary shape: {full_summary.shape}")
    full_summary.to_csv(os.path.join(model_output_dir, 'full_summary.csv'), index=True)
    logging.info(f"Full summary saved to {os.path.join(model_output_dir, 'full_summary.csv')}")

except:
    logging.info(f"failed to get summary")

# Save full summary


# List of parameters of interest (non-subject/match-specif
logging.info("Checking diagnostics...")
diagnostics = StanFit.diagnose()
logging.info(f"Diagnostics: {diagnostics}")
