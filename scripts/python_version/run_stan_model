#!/bin/bash
#SBATCH --job-name=2016uniform
#SBATCH --partition=glacier
#SBATCH --output=april_30%j.log
#SBATCH --error=april_30%j.err
#SBATCH --time=5-5
#SBATCH --ntasks=1 
#SBATCH --cpus-per-task=10 
#SBATCH --mem=520G 
#SBATCH --mail-type=BEGIN,END,FAIL 
#SBATCH --mail-user=levi.solomyak@mail.huji.ac.il 

# Activate your virtual environment 
source /sci/labs/eran.eldar/levi.solomyak/tennis_env/bin/activate


# Set CMDSTAN environment variable
export CMDSTAN=/sci/home/levi.solomyak/.cmdstan/cmdstan-2.35.0
echo "CMDSTAN is set to: $CMDSTAN"

# Print Python path
echo "Python path:"
python -c "import sys; print('\n'.join(sys.path))"

# Check for required packages
echo "Checking for required packages..."
python -c "import numpy; print('numpy version:', numpy.__version__)"
python -c "import pandas; print('pandas version:', pandas.__version__)"
python -c "import cmdstanpy; print('cmdstanpy version:', cmdstanpy.__version__)"


/sci/labs/eran.eldar/levi.solomyak/tennis_env/bin/python  /sci/home/levi.solomyak/tennis/v_script_cr.py
