#!/usr/bin/env python3
"""
set_data_prep_clean.py

Modularized data preparation for tennis match analysis, including generation of Stan data list.
Usage:
    python set_data_prep_clean.py --input data.csv \
        --out_df cleaned.csv --out_players players.csv --out_summary summary.json \
        [--out_stan stan_data.json]
"""



def set_data(n_players, progress_type, progress_effect, years):
    import argparse
    import json
    import pandas as pd
    import numpy as np
    import os
    import sys
    import warnings
    """
    Set data for tennis match analysis.
    
    Args:
        n_players (int): Number of players to include.
        progress_type (str): Type of progress to analyze.
        progress_effect (str): Effect of progress to analyze.
        years (list): List of years to include in the analysis.
    
    Returns:
        dict: Data for Stan model.
    """
    # Load and preprocess data
    match_data = pd.read_csv("/Users/levisolomyak/Desktop/02Phd/tennis_project/data/match_data.csv")
    match_data = match_data.dropna(subset=['served_and_scored','PointServer'])
    match_data = match_data[match_data['year'].isin(years)]
    match_data= match_data[(match_data['player1']!="") & (match_data['player2']!="")]
   # first_vals=match_data.groupby('match_id')['points_remaining_overall1'].transform('first

    # +
    # n_players = 1000
    # progress_type='game'
    # progress_effect='int_abs'
    # years = [2011, 2012, 2013, 2014, 2015]
    print(f'Max number of players: {n_players}')
    print(f'Progress type: {progress_type}')
    print(f'Progress effect: {progress_effect}')
    print(f'Years: {years}')

    match_data = pd.read_csv("/Users/levisolomyak/Desktop/02Phd/tennis_project/data/match_data.csv")
    match_data = match_data.dropna(subset=['served_and_scored','PointServer'])
    match_data = match_data[match_data['year'].isin(years)]
    match_data= match_data[(match_data['player1']!="") & (match_data['player2']!="")]
    first_vals=match_data.groupby('match_id')['points_remaining_overall1'].transform('first')
    match_data['male']=np.select(
        [first_vals==72,first_vals==48],
            [True,False],
            default=np.nan)



    # -

    test=(
        match_data[(match_data['player1']!='') & (match_data['player2']!='')]
        .copy()
        .assign(ID=lambda df:df.groupby('player1').ngroup()+1)
        [['player1','ID']]
    )
    test


    # +
    matches_played1= match_data.groupby(['player1'])['match_id'].nunique().reset_index(name='n_matches')
    matches_played2 =match_data.groupby(['player2'])['match_id'].nunique().reset_index(name='n_matches')
    matches_played1.rename(columns={'player1':'player2'},inplace=True)
    temp=pd.concat([matches_played1,matches_played2],axis=0,ignore_index=True)
    match_player_all=temp.groupby('player2',as_index=False)['n_matches'].sum().rename({'n_matches':'all_matches'})
    player_n_matches=match_player_all.sort_values('n_matches',ascending=False)
    players_to_keep=player_n_matches.iloc[0:n_players,][['player2']]
    one_gamers=player_n_matches[player_n_matches['n_matches']==1]

    players_to_keep=players_to_keep['player2'].tolist()
    test=match_data.groupby('match_id').filter(
        lambda g: (g['player1'].isin(players_to_keep) & g['player2'].isin(players_to_keep)).all())
    test.shape


    # +
    while one_gamers.shape[0]>0:
        player_n_matches['numeric_id']=np.arange(1,player_n_matches.shape[0]+1)
        print(f'number of one timers',one_gamers.shape[0])
        print(f'total observations before', match_data.shape[0])
    
        matches_one_gamers=match_data[(match_data['player1'].isin(one_gamers['player2'])) | (match_data['player2'].isin(one_gamers['player2']))]
        match_data=match_data[~((match_data['player1'].isin(one_gamers['player2'])) | (match_data['player2'].isin(one_gamers['player2'])))]
        print(f'total observations after', match_data.shape[0])
        
        matches_played1= match_data.groupby(['player1'])['match_id'].nunique().reset_index(name='n_matches')
        matches_played2 =match_data.groupby(['player2'])['match_id'].nunique().reset_index(name='n_matches')
        matches_played1.rename(columns={'player1':'player2'},inplace=True)
    
        temp=pd.concat([matches_played1,matches_played2],axis=0,ignore_index=True)
        match_player_all=temp.groupby('player2',as_index=False)['n_matches'].sum().rename({'n_matches':'all_matches'})
        
        player_n_matches=match_player_all.sort_values('n_matches',ascending=False)
    #  players_to_keep=player_n_matches.iloc[0:n_players,][['player2']]
        one_gamers=player_n_matches[player_n_matches['n_matches']==1]
        
    print(f'total observations =',match_data.shape[0])

    # +

    common_elements=set(match_data['player1']) & set(match_data['player2'])
    print(len(list(common_elements)))
    one=match_data[['player1']]
    two=match_data[['player2']].rename(columns={'player2':'player1'})

    player_n_matches['numeric_id']=np.arange(1,len(player_n_matches)+1)
    player_n_matches.rename(columns={'player2':'player1'},inplace=True)
    all_players=pd.concat([one,two],axis=0,ignore_index=True)
    all_players=all_players.merge(player_n_matches,on='player1',how='left')

    # -

    all_players_numeric =all_players.groupby('player1',as_index=False)['numeric_id'].first().rename(columns={'numeric_id':'numeric_id1'})
    all_players_numeric
    match_data=match_data.merge(all_players_numeric,on='player1',how='left')
    print(f'match data rows', match_data.shape[0])


    # +
    all2=all_players
    all2.rename(columns={'player1':'player2','numeric_id':'numeric_id2'},inplace=True)
    all2 = all2[['player2', 'numeric_id2']]
    match_data['match_id']=match_data['match_id'].astype('category')
    match_data['player2']=match_data['player2'].astype('category')

    all2['player2']=all2['player2'].astype('category')

    #match_data=match_data.merge(all2,on='player2',how='left')
    # Create a mapping from player2 → numeric_id2
    id_map = dict(zip(all2['player2'], all2['numeric_id2']))

    # Assign using map (no merge)
    match_data['numeric_id2'] = match_data['player2'].map(id_map)

    # -

    djokovic_index=match_data[match_data['player1']=="Novak Djokovic"]['numeric_id1']
    nadal_index=match_data[match_data['player1']=="Rafael Nadal"]['numeric_id1']
    federer_index=match_data[match_data['player1']=="Roger Federer"]['numeric_id1']
    print(f'djok',djokovic_index )

    logistic_reg_data = match_data[[
        'served_and_scored', 'scored1', 
        'Speed_KMH', 'ServeNumber', 'PointServer', 'P1Ace', 'P2Ace',
        'P1BreakPointMissed', 'P1BreakPointWon', 'numeric_id1', 'numeric_id2',
        'player1', 'player2', 'match_id', 'GameNo', 'SetNo',
        'points_remaining_game1', 'points_remaining_game2',
        'points_remaining_set1', 'points_remaining_set2',
        'points_remaining_overall1', 'points_remaining_overall2',
        'tie_breaker', 'sets_to_win1', 'sets_to_win2', 'rally_new',
        'P1Score', 'P2Score', 'male', 'P1GamesWon', 'P2GamesWon'
    ]]


    # +
    n_players=pd.concat([logistic_reg_data['numeric_id1'],logistic_reg_data['numeric_id2']]).nunique()
    # logistic_reg_data['scored2']=(logistic_reg_data['scored1']==0).astype('int')
    # logistic_reg_data['Point_non_Server']=(logistic_reg_data['PointServer']==0).astype('int')
    # logistic_reg_data['Point_Server_score']=np.where(logistic_reg_data['PointServer']==1,logistic_reg_data['scored1'],logistic_reg_data['scored2'])
    # logistic_reg_data['Point_non_Server_score']=np.where(logistic_reg_data['Point_non_Server']==1,logistic_reg_data['scored1'],logistic_reg_data['scored2'])

    logistic_reg_data=logistic_reg_data.assign(
    scored2=lambda d: d.scored1.eq(0).astype(int),
    Point_non_Server=lambda d: d.PointServer.eq(0).astype(int),
    Point_Server_score= lambda d: d.scored1.where(d.PointServer.eq(1),d.scored2),
    Point_non_Server_score =lambda d: d.scored1.where(d.Point_non_Server.eq(1),d.scored2),
    score_difference= lambda d: d.scored1-d.scored2,
    cumulative_score_difference=lambda d: d.groupby(['match_id','GameNo'])['score_difference'].cumsum()
    )




    # +
    srv = logistic_reg_data['PointServer']
    s1  = logistic_reg_data['scored1']
    s2  = logistic_reg_data['scored2']

    conditions = [
        (srv == 1) & (s1 == 1),
        (srv == 2) & (s2 == 1),
        (srv == 1) & (s1 == 0),
        (srv == 2) & (s2 == 0)
    ]
    choices = [1, 1, 0, 0]

    logistic_reg_data['served_and_scored'] = np.select(conditions, choices, default=np.nan)

    n_players=logistic_reg_data[['numeric_id1','numeric_id2']].stack().nunique()
    logistic_reg_data['PointServer']=np.where(logistic_reg_data['PointServer']==2,-1,1)
    logistic_reg_data=logistic_reg_data.assign(
    Point_Server_id=lambda df: df.numeric_id1.where(df.PointServer==1,df.numeric_id2),
    Point_non_Server_id=lambda df: df.numeric_id1.where(df.Point_non_Server==1,df.numeric_id2)
    )

    # +
    tmp=logistic_reg_data.drop(columns=['scored1','match_id'])
    divs={
    'points_remaining_game1': 4,
    'points_remaining_game2': 4,
    'points_remaining_set1': 24,
    'points_remaining_set2': 24,
    'points_remaining_overall1': 72,
    'points_remaining_overall2': 72,
    }
    print(tmp['points_remaining_game1'])
    for orig, d in divs.items():
        print(f"points_to_{orig.split('_')[-1]}")
        logistic_reg_data[f"points_to_{orig.split('_')[-1]}"]= (tmp[orig]/d).to_list()



    # +
    labels = logistic_reg_data['match_id'].astype(str)+ "Set" + logistic_reg_data['SetNo'].astype(str) + "_Game" + logistic_reg_data['GameNo'].astype(str)+ "_"
    logistic_reg_data=logistic_reg_data.assign(
        game_id= lambda df: pd.factorize(labels)[0]+1,
        id= lambda df: df.index+1,
        server_label=lambda df:  df.match_id.astype(str)+"server_id"+df.Point_Server_id.astype(str),
        server_id_game_id=lambda df: pd.factorize(df['server_label'])[0]+1,

        player1_label=lambda df:  df.match_id.astype(str)+"player1id"+df.numeric_id1.astype(str),
        player1_id_match_id=lambda df: pd.factorize(df['player1_label'])[0]+1,

        player1_served=lambda df: np.where(df['PointServer']==1,1,-1)
        # +1 or -1 based on serving 

    ).drop(columns='server_label')

    flip = np.where(
        logistic_reg_data['player1_served']
        != logistic_reg_data['player1_served'].shift(1),
        -1,
        1
    )
    flip[0] = 1
    logistic_reg_data['sign_served'] = pd.Series(flip).cumprod()


    # -

    num_played_p1=logistic_reg_data.groupby(['player1'])['match_id'].nunique()
    num_played_p2=logistic_reg_data.groupby(['player2'])['match_id'].nunique()


    total_matches= (logistic_reg_data
                    .melt(id_vars='match_id',
                        value_vars=['numeric_id1','numeric_id2'],
                        value_name='player')
                        .drop_duplicates()
                        .groupby('player')['match_id']
                        .nunique()
                        .rename('matches_played')
                        .reset_index()
                        .sort_values(by='matches_played',ascending=False)
                    
                    )
    total_matches_enough=total_matches[total_matches['matches_played']>=6]
    is_player_enough = np.isin(
        np.arange(n_players),
        total_matches_enough['player'].to_numpy() - 1
    ).astype(int)


    # +
    import numpy as np

    logistic_reg_data = logistic_reg_data.assign(
        progress_1minus2=lambda df: np.where(
            'progress_type' == 'match',
            (df['points_to_overall2'] - df['points_to_overall1'])
            / (df['points_to_overall2'] + df['points_to_overall1']),
            (df['points_to_game2']  - df['points_to_game1'])
            / (df['points_to_game2']  + df['points_to_game1'])
        ),
        progress_1minus2_abs=lambda df: df['progress_1minus2'].abs()
    )


    # +
    logistic_reg_data['is_serve_speed']=(logistic_reg_data['Speed_KMH']>0) & (logistic_reg_data['ServeNumber']>0)
    is_serve_speed_player_s=(logistic_reg_data
    .loc[(logistic_reg_data['is_serve_speed'])]
    .groupby(['Point_Server_id','ServeNumber'])['match_id']
    .nunique()
    .reset_index(name='matches_with_serve')
    .pivot(
        index='Point_Server_id',
        columns='ServeNumber',
        values='matches_with_serve'
        )
    .reset_index()
    .rename_axis(columns=None)  # drop the “ServeNumber” name on the columns axis
    .rename(columns={1.0:'first',2.0:'second'})
    )

    is_serve_speed_player_enough=is_serve_speed_player_s.query('first > 5 and second >5')
    is_serve_speed_player = np.isin(
        np.arange(n_players),
        is_serve_speed_player_enough['Point_Server_id'].to_numpy() - 1
    ).astype(int)
    is_serve_speed_indices = logistic_reg_data.index[logistic_reg_data['is_serve_speed']]
    n_serve_speed_matches=logistic_reg_data.loc[logistic_reg_data['is_serve_speed'],'match_id'].nunique()
    logistic_reg_data['match_id']=pd.factorize(logistic_reg_data['match_id'])[0]+1
    serve_speed_match_id=logistic_reg_data.loc[logistic_reg_data['is_serve_speed'],'match_id']
    common_elements = np.intersect1d(
        logistic_reg_data['numeric_id1'],
        logistic_reg_data['numeric_id2']
    )
    num_identical=len(common_elements)

    n_players_with_serve_speed=is_serve_speed_player_s.shape[0]
    n_players_with_serve_speed_enough =is_serve_speed_player_enough.shape[0]
    n_players_enough = is_player_enough.sum()


    # -

    players_enough_indices=is_serve_speed_player_enough['Point_Server_id'].to_numpy() - 1
    players_enough_indices



    # +
    stan_data = {
        'n_score': len(logistic_reg_data),
        'y': logistic_reg_data['served_and_scored'].values,
        'serve_speed':logistic_reg_data.iloc[is_serve_speed_indices]['Speed_KMH'].transform(lambda x: (x-x.mean())/x.std()),
        'ServeNumber': logistic_reg_data['ServeNumber'].values,
        'Point_Server_id': logistic_reg_data['Point_Server_id'].values,
        'Point_non_Server_id': logistic_reg_data['Point_non_Server_id'].values,
        'P1Ace': logistic_reg_data['P1Ace'].values,
        'sign': 2 * logistic_reg_data['served_and_scored'] - 1,
        'id1': logistic_reg_data['numeric_id1'].values,
        'id2': logistic_reg_data['numeric_id2'].values,
        'match_id': logistic_reg_data['match_id'].values,
        'n_matches': len(logistic_reg_data['match_id'].unique()),
        'serve_speed_match_id': logistic_reg_data['match_id'].values,
        'n_serve_speed_matches': len(logistic_reg_data['match_id'].unique()),
        'n_players': n_players,
        'n_players_enough': n_players_enough,
        'player1_served': logistic_reg_data['player1_served'].values,
        'is_serve_speed_indices': is_serve_speed_indices,
        'n_players_with_serve_speed': n_players_with_serve_speed,
        'n_players_with_serve_speed_enough': n_players_with_serve_speed_enough,
        'PointServer': logistic_reg_data['PointServer'].values,
        'n_serve_speed': len(is_serve_speed_indices),
        'n_player_matches': len(logistic_reg_data['match_id'].unique()),
        'players_enough_indices': players_enough_indices,
        'players_with_serve_speed_enough_indices': players_enough_indices,
        'progress_1minus2': logistic_reg_data['progress_1minus2'].values,
        'progress_1minus2_abs': logistic_reg_data['progress_1minus2_abs'].values}


    return stan_data


