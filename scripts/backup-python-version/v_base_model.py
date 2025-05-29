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
with open(os.path.join(data_dir, "data_2016_2022.json"), "r") as f:
    data = json.load(f)

# Convert the data into a dictionary suitable for cmdstanpy
stan_data = {key: np.array(value) if isinstance(value, list) else value for key, value in data.items()}

# Log info about the data
for key, value in stan_data.items():
    if isinstance(value, np.ndarray):
        logging.info(f"{key}: shape={value.shape}, min={np.min(value)}, max={np.max(value)}")
    else:
        logging.info(f"{key}: {value}")

# Initialize and run Stan model
model_name = 'base_model'
model_path = os.path.join(current_dir, 'stan_models', f'{model_name}.stan')
StanModel = CmdStanModel(stan_file=model_path)

# Create model-specific output directory
model_output_dir = os.path.join('./stan_results', model_name)
os.makedirs(model_output_dir, exist_ok=True)

logging.info("Starting sampling process...")

StanFit = StanModel.sample(
    data=stan_data,
    iter_warmup=1110,
    iter_sampling=500,
    chains=10,
    parallel_chains=10,
    max_treedepth=8,
    adapt_delta=0.85,
    threads_per_chain=1,
    show_progress=True,
    show_console=True,
    output_dir=model_output_dir
)
logging.info("Sampling completed successfully.")
os.makedirs(model_output_dir, exist_ok=True)
logging.info("Checking diagnostics...")
diagnostics = StanFit.diagnose()
logging.info(f"Diagnostics: {diagnostics}")


# Print the contents of the console output file
console_files = [f for f in os.listdir(model_output_dir) if f.endswith('.txt')]
if console_files:
    with open(os.path.join(model_output_dir, console_files[0]), 'r') as f:
        console_output = f.read()
    logging.info(f"Console output:\n{console_output}")
else:
    logging.warning("No console output file found.")

# Get full summary
try:
    full_summary = StanFit.summary()
    logging.info(f"Full model summary shape: {full_summary.shape}")
    full_summary.to_csv(os.path.join(model_output_dir, 'full_summary.csv'), index=True)
    logging.info(f"Full summary saved to {os.path.join(model_output_dir, 'full_summary.csv')}")

except:
    logging.info(f"failed to get summary")


# Add after fitting








