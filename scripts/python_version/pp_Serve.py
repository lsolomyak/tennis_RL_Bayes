

import pandas as pd
import numpy as np
from pathlib import Path

import os
import sys
import seaborn as sns
import matplotlib.pyplot as plt


sys.path.append(os.path.join(os.getcwd(), 'scripts'))

from pp_check_functions import *


# note- all functions are drawn from the pp_check_functions.py script

# ### Loading the data 

# In[1]:


sum_2016[sum_2016['parameter'].str.contains('beta_serve_speed_surprise_int_player\[5]', regex=True, na=False)]#RetryClaude can make mistakes. Please double-check responses.


# In[3]:


csv_2011, csv_2016, sum_2011, sum_2016,stan_2011,stan_2016=get_data()


# #### Next we add the per-trial cumulative surprise fits from the model 

# In[4]:


# next we add cumulative surprise 
csv_2011 = add_cumulative(sum_2011,csv_2011)
csv_2016= add_cumulative(sum_2016,csv_2016)


# We also need to extract all the parameters from the summary csv's - the full samples are way too big to analyze here

# In[ ]:


def run_post_point(sum,data,stan):
   # data = add_cumulative(sum,data)

    point_params=extract_point_params(sum)
    data['win_prob'] = compute_win_prob(data, point_params, stan)
    point_cal_data=create_point_calibration_data(data,n_bins=20)
    return point_cal_data

point_cal_data_2011=run_post_point(sum_2011,csv_2011,stan_2011)
point_cal_data_2016=run_post_point(sum_2016,csv_2016,stan_2016)

point_cal_data_2011['period'] = '2011-2015'
point_cal_data_2016['period'] = '2016-2022'
combined_data = pd.concat([point_cal_data_2011,point_cal_data_2016])
create_combined_point_plot(combined_data)


# In[34]:


# start with group level 
def run_post_serve(sum,data,stan):
    serve_params= extract_serve_params(sum)
    print('extracted parameters succesfully')
    predictions = compute_serve_speed(data, serve_params, stan)
    print('made predictions')

    point_cal_data = create_serve_calibration_data(predictions['speeds'], data.loc[predictions['indices'],'Speed_KMH'], n_bins=20)
    print('prepare data for plot')

    return point_cal_data, predictions
cal_2011, predictions_2011=run_post_serve(sum_2011,csv_2011,stan_2011)
cal_2016, predictions_2016=run_post_serve(sum_2016,csv_2016,stan_2016)
overlay_plot = create_overlay_plot(cal_2011, cal_2016, "2011-2015", "2016-2022")


# ### Now evaluating player specific plots

# In[40]:


predictions_2016=predictions_2015


# In[48]:


from pathlib import Path
output_dir=Path(os.getcwd())
Path(output_dir.parent.parent / 'vis_python')


# In[50]:


# Create player calibration data
player_cal_all = []
players_2016 = {
    "Novak Djokovic":1,
    "Serena Williams":2,
    "Rafael Nadal":4,
    "Roger Federer":6
}
players_2011 = {
    "Novak Djokovic":1,
    "Serena Williams":4,

    "Rafael Nadal":8,
    "Roger Federer":3
}
save_plots=True
output_dir=Path(os.getcwd())
Path(output_dir.parent.parent / 'vis_python')
print('plots ar saved but can be adjusted to false')
for player_name, player_id in players_2011.items():
    
    cal_data = create_player_calibration_data(
        csv_2011, predictions_2011, player_id, player_name
    )
    if cal_data is not None:
        cal_data['dataset'] = '2011-2015'
        player_cal_all.append(cal_data)



for player_name, player_id in players_2016.items():
    
    cal_data = create_player_calibration_data(
        csv_2016, predictions_2016, player_id, player_name
    )
    if cal_data is not None:
        cal_data['dataset'] = '2016-2022'
        player_cal_all.append(cal_data)
# Combine all player calibration data
if player_cal_all:
    player_df = pd.concat(player_cal_all, ignore_index=True)
    print(f"Created calibration data for {len(player_cal_all)} player-dataset combinations")
    
    # Create player plots
    for player_name in ['Novak Djokovic', 'Rafael Nadal', 'Roger Federer']:
        fig = create_player_calibration_plot(player_df, player_name)
        if fig is not None:
            if save_plots:
                safe_name = player_name.replace(' ', '_').lower()
                fig.savefig(f"{output_dir}/player_{safe_name}.png", dpi=300, bbox_inches='tight')
            plt.show()
else:
    print("No player calibration data created")

