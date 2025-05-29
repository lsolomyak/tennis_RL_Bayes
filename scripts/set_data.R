library(dplyr)
library(tidyr)

set_data = function(n_players = 10000, progress_type, progress_effect, years){
  print(paste0("Max num players = ", n_players))
  print(paste0("Progress type = ", progress_type))
  print(paste0("Progress effect = ", progress_effect))
  print(paste0("Years = ", paste(years, collapse = ",")))
  
  match_data = read_csv("/Users/levisolomyak/Desktop/02Phd/tennis_project/data/match_data.csv")
  
  match_data=match_data %>% dplyr::filter(!is.na(served_and_scored))
  
  match_data=match_data %>% dplyr::filter(!is.na(PointServer))

  match_data=match_data %>% dplyr::filter(year %in% years)
  
  # match_data=match_data %>% dplyr::group_by(match_id,GameNo ) %>% dplyr::filter(n() > 10) %>% ungroup()
  
  match_data %>% dplyr::filter(player1 == "", player2 =="") %>% nrow()
  
  match_data = match_data %>% dplyr::filter(player1 != "", player2 !="")
  
  
 
  test=match_data %>% dplyr::filter(player1 != "", player2 !="") %>%                                        # Create ID by group
    dplyr::group_by(player1) %>%
    dplyr::mutate(ID = cur_group_id()) %>% select(ID)
  
  names(match_data)
  
  test = test[order(test$ID),]
  
  names(match_data)
  matches_played1 = match_data %>% dplyr::group_by(player1) %>% dplyr::summarise(n_matches = n_distinct(match_id))
  matches_played2 = match_data %>% dplyr::group_by(player2) %>% dplyr::summarise(n_matches = n_distinct(match_id)) 
 
  match_data <- match_data %>%
    dplyr::group_by(match_id) %>%
    dplyr::mutate(male = case_when(
      first(points_remaining_overall1) == 72 ~ TRUE,
      first(points_remaining_overall1) == 48 ~ FALSE,
      TRUE ~ NA
    )) %>%
    ungroup()
  
  
  names(matches_played1)[1] = "player2"
  matches_players_all= bind_rows(matches_played1,matches_played2) %>% dplyr::group_by(player2) %>% dplyr::summarise(all_matches = sum(n_matches))

    # matches_players_all %>% dplyr::group_by(player2)%>%  dplyr::mutate(all_matches =n_matches.x+n_matches.y)
  
  matches_players_all %>% dplyr::arrange(all_matches)
  
  players_n_matches = matches_players_all[order(matches_players_all$all_matches,decreasing =T),]
  # 
  
  initial_id =1
  players_to_keep=players_n_matches[c(initial_id:(initial_id+n_players)),]  # 
  players_to_keep = players_to_keep$player2
  # 
  test =  match_data %>% dplyr::group_by(match_id) %>%  dplyr::filter(player1 %in% players_to_keep | player2 %in% players_to_keep)
  # print(nrow(test))
  nrow(match_data)
  
  match_data =  match_data %>% dplyr::filter(player1 %in% players_to_keep | player2 %in% players_to_keep)
  nrow(match_data)
  
  matches_played1 = match_data %>% dplyr::group_by(player1) %>% dplyr::summarise(n_matches = n_distinct(match_id))
  matches_played2 = match_data %>% dplyr::group_by(player2) %>% dplyr::summarise(n_matches = n_distinct(match_id)) 
  
  
  names(matches_played1)[1] = "player2"
  matches_players_all= bind_rows(matches_played1,matches_played2) %>% dplyr::group_by(player2) %>% dplyr::summarise(all_matches = sum(n_matches))
  #dplyr::filter designated players

    
  matches_players_all %>% dplyr::arrange(all_matches)
  
  players_n_matches = matches_players_all[order(matches_players_all$all_matches,decreasing =T),]
  one_gamers = players_n_matches %>% dplyr::filter(all_matches==1)
  # need to change above to bind_rows instaed of left koin
  while (nrow(one_gamers)>0){
    
    
    players_n_matches$numeric_id = 1:nrow(players_n_matches)
    
    
    #dplyr::filter players with 1 match
    
    
    print(paste("number of one timers = ",nrow(one_gamers)))
    print(paste("total observations before = ",nrow(match_data)))
    
    matches_one_gamers = match_data %>% dplyr::filter(player1%in%one_gamers$player2 |player2%in%one_gamers$player2)
    n_distinct(matches_one_gamers$match_id)
    n_distinct(match_data$match_id)
    
    match_data=match_data %>% dplyr::filter(!(player1%in%one_gamers$player2 |player2%in%one_gamers$player2))
    print(paste("total observations after = ",nrow(match_data)))
    
    n_distinct(matches_players_all$player2)
    
    #recalculate number of matches per player
    matches_played1 = match_data %>% dplyr::group_by(player1) %>% dplyr::summarise(n_matches = n_distinct(match_id))
    matches_played2 = match_data %>% dplyr::group_by(player2) %>% dplyr::summarise(n_matches = n_distinct(match_id)) 
    n_distinct(c(match_data$player1,match_data$player2))
    names(matches_played1)[1] = "player2"
    # matches_players_all= matches_played1 %>% left_join(matches_played2, by = "player2")
    matches_players_all= bind_rows(matches_played1,matches_played2) %>% dplyr::group_by(player2) %>% dplyr::summarise(all_matches = sum(n_matches))
    
    #dplyr::filter designated players
    players_n_matches = matches_players_all[order(matches_players_all$all_matches,decreasing =T),]
    one_gamers = players_n_matches %>% dplyr::filter(all_matches==1)
    # 
  }
  print(paste("total observations = ",nrow(match_data)))
 
  match_data<-match_data
  one= match_data %>% select(c(player1))
  two = match_data %>% select(c(player2))

  common_elements <- intersect(two$player2, one$player1)
  
  # Count the number of common elements
  num_identical <- length(common_elements)
  
  # Output the result
  print(num_identical)
  head(two)
  head(one)
  players_n_matches$numeric_id = 1:nrow(players_n_matches)
  
  names(two)[1] = "player1"
  all = bind_rows(one,two)
  names(players_n_matches)[1] = "player1"
  all = all %>% left_join(players_n_matches)
  
  all %>% dplyr::filter(player1=="Jan Lennard Struff") %>% dplyr::summarise(n_distinct(numeric_id))
  all$player1
  test=all %>% dplyr::group_by(player1) %>% dplyr::mutate(n_unique = n_distinct(numeric_id)) %>% ungroup()
  test %>% dplyr::filter(n_unique>1)
  n_distinct(test$player1)
  n_distinct(test$numeric_id)
  
  
  all=  all %>% dplyr::group_by(player1) %>% dplyr::summarise(numeric_id1 = unique(numeric_id))

    
  # match_data=match_data %>% select(-numeric_id1)
  match_data = match_data %>% left_join(all, by = "player1")
  all2 =  all
  names(all2)[1] = "player2"
  names(all2)[2] = "numeric_id2"
  match_data = match_data %>% left_join(all2, by = "player2")
  
  djokovic_index <- unique(c(match_data$numeric_id1[match_data$player1 == "Novak Djokovic"], 
                             match_data$numeric_id2[match_data$player2 == "Novak Djokovic"]))
  
  nadal_index <- unique(c(match_data$numeric_id1[match_data$player1 == "Rafael Nadal"], 
                          match_data$numeric_id2[match_data$player2 == "Rafael Nadal"]))
  
  federer_index <- unique(c(match_data$numeric_id1[match_data$player1 == "Roger Federer"], 
                            match_data$numeric_id2[match_data$player2 == "Roger Federer"]))
  
  print(paste("Novak Djokovic index:", djokovic_index))
  print(paste("Rafael Nadal index:", nadal_index))
  print(paste("Roger Federer index:", federer_index))

    
  
  
  test=match_data %>% dplyr::filter(player1=="Jan Lennard Struff" |player2=="Jan Lennard Struff" ) 
  
  common_elements <- intersect(match_data$numeric_id1, match_data$numeric_id2)
  
  # Count the number of common elements
  num_identical <- length(common_elements)
  num_identical
  
  n_distinct(c(match_data$numeric_id1,match_data$numeric_id2))

    
  # 
  # match_data$player2
  # all2 %>% dplyr::filter(player2=="Marcos Daniel")
  # all %>% dplyr::filter(player1=="Marcos Daniel")
  # 
  #  match_data %>% dplyr::mutate(numeric_id=numeric_id) 
  #  as.numeric(levels(factor(match_data$player1)))
  #  
  # transform(match_data,                                 # Create ID by group
  #           ID = as.numeric(factor(player1)))$ID
  # 
  
  
  one= match_data %>% select(c(numeric_id1,match_id))
  two = match_data %>% select(c(numeric_id2,match_id))
  # names(two)[1] = "player1"
  
  one$player1_id_match_id = paste0(match_data$match_id,"player1id", one$numeric_id1)
  two$player2_id_match_id = paste0(match_data$match_id,"player2id", two$numeric_id2)
  
  one = one %>% dplyr::mutate( player1_id_match_id = as.numeric(as.factor(player1_id_match_id)))
  two = two %>% dplyr::mutate( player2_id_match_id = as.numeric(as.factor(player2_id_match_id)))
  
  one=  one %>% dplyr::group_by(numeric_id1,match_id) %>% dplyr::summarise(player1_id_match_id = unique(player1_id_match_id)) %>% ungroup()
  two=  two %>% dplyr::group_by(numeric_id2,match_id) %>% dplyr::summarise(player2_id_match_id = unique(player2_id_match_id)) %>% ungroup()
  
  
  
  
  # all = bind_rows(one,two)
  # 
  # 
  # all$player1_id_match_id = paste0(match_data$match_id,"player1id", all$player1)
  # 
  # 
  # all = all %>% dplyr::mutate( player1_id_match_id = as.numeric(as.factor(player1_id_match_id)))
  # 
  # 
  # all=  all %>% dplyr::group_by(player1,match_id) %>% dplyr::summarise(player1_id_match_id = unique(player1_id_match_id))
  
  
  # match_data=match_data %>% select(-numeric_id1)
  match_data = match_data %>% left_join(one, by = c("numeric_id1","match_id"))
  # all2 =  all
  # names(all2)[1] = "player2"
  # names(all2)[3] = "player2_id_match_id"
  match_data = match_data %>% left_join(two, by =c("numeric_id2","match_id"))
  # 
  
  n_distinct(match_data$numeric_id1)
  n_distinct(match_data$numeric_id2)
  
  n_players =  n_distinct(c(match_data$numeric_id1,match_data$numeric_id2))
  n_players
  
  
  common_elements <- intersect(match_data$numeric_id1, match_data$numeric_id2)
  
  # Count the number of common elements
  num_identical <- length(common_elements)
  num_identical
  
  logistic_reg_data = match_data %>% 
    dplyr::select(
      # Existing variables
      served_and_scored, scored1, player1_id_match_id, player2_id_match_id,
      Speed_KMH, ServeNumber, PointServer, P1Ace, P2Ace, P1BreakPointMissed,
      P1BreakPointWon, numeric_id1, numeric_id2, player1, player2, match_id,
      GameNo, SetNo, points_remaining_game1, points_remaining_game2,
      points_remaining_set1, points_remaining_set2, points_remaining_overall1,
      points_remaining_overall2,tie_breaker,sets_to_win1,sets_to_win2,rally_new,
      # Updated score variables
      P1Score, P2Score,male,P1GamesWon,P2GamesWon,P1DistanceRun,P2DistanceRun
    )# 
  
  
  names(match_data)
  n_distinct(match_data$numeric_id1)
  n_distinct(match_data$numeric_id2)
  
  
  n_players =  n_distinct(c(logistic_reg_data$numeric_id1,logistic_reg_data$numeric_id2))
  n_players
  # logistic_reg_data = logistic_reg_data %>% head(n=5000)
  # logistic_reg_data$Point_non_Server_id
  # n_distinct(match_data$match_id)
  # 
  # as.numeric(factor(logistic_reg_data$player1))
  # 
  # 
  # match(logistic_reg_data$player1id, sample(unique(logistic_reg_data$player2id)))
  # 
  

  
  names(logistic_reg_data)
  logistic_reg_data = logistic_reg_data %>% dplyr::mutate(scored2 = ifelse(scored1 == 1,0,1))
  
  logistic_reg_data$Point_non_Server = ifelse(logistic_reg_data$PointServer==1,2,1)
  
  
  logistic_reg_data = logistic_reg_data %>% dplyr::mutate(Point_Server_score = 
                                                            ifelse(logistic_reg_data$PointServer==1,scored1 ,scored2))
  
  
  
  logistic_reg_data = logistic_reg_data %>% dplyr::mutate(Point_non_Server_score = 
                                                            ifelse(logistic_reg_data$Point_non_Server==1,scored1 ,scored2))
  
  
  
  logistic_reg_data = logistic_reg_data %>%dplyr::group_by(match_id,GameNo) %>%  dplyr::mutate(score_difference = scored1 -
                                                                                          scored2) %>% ungroup()
  
  
  logistic_reg_data = logistic_reg_data %>%dplyr::group_by(match_id,GameNo) %>%  
    dplyr::mutate(cumulative_score_difference = cumsum(score_difference),  P1DistanceRun = P1DistanceRun,  # Explicitly maintain these columns
    P2DistanceRun = P2DistanceRun,rally_new=rally_new) %>% ungroup()
  
  
  logistic_reg_data <- logistic_reg_data  %>%  
    dplyr::mutate(served_and_scored = case_when(
      PointServer == 1 & scored1== 1 ~ 1,
      PointServer == 2 & scored2== 1 ~ 1,
      PointServer == 1 & scored1== 0 ~ 0,
      PointServer == 2 & scored2== 0 ~ 0
    ))
  
  
  
  n_players =  n_distinct(c(logistic_reg_data$numeric_id1,logistic_reg_data$numeric_id2))
  n_players
  # 
  # test=logistic_reg_data %>% dplyr::filter(score_difference*player1_served==-1 & served_and_scored==1)
  # test=logistic_reg_data %>% dplyr::filter(score_difference*player1_served==1 & served_and_scored==-1)
  
  
  names(logistic_reg_data)
  #create a game index
  # logistic_reg_data%>% dplyr::mutate( game = as.numeric(as.factor(GameNo)))
  #create a variable "did player 1 served?
  # logistic_reg_data = logistic_reg_data %>% dplyr::mutate(player1_served = case_when(
  #   PointServer==1 ~1,
  #   PointServer ==2 ~-1
  # ))
  
  logistic_reg_data = logistic_reg_data %>% dplyr::mutate(player1_served = case_when(
    PointServer==1 ~1,
    PointServer ==2 ~-1
  ))
  
  
  
  # 
  # for (i in 2:nrow(logistic_reg_data)){
  #   if (logistic_reg_data$player1_served[i-1] !=1 ){
  #     logistic_reg_data$score_difference[i-1] = -logistic_reg_data$score_difference[i-1];
  #   }}
  
  
  logistic_reg_data$Point_Server_id = 
    ifelse(logistic_reg_data$PointServer==1,logistic_reg_data$numeric_id1 ,logistic_reg_data$numeric_id2)
  any(logistic_reg_data$numeric_id2==113)
  
  logistic_reg_data$Point_non_Server_id = 
    ifelse(logistic_reg_data$Point_non_Server==1,logistic_reg_data$numeric_id1,logistic_reg_data$numeric_id2)
  names(logistic_reg_data)
  
  n_players =  n_distinct(c(logistic_reg_data$numeric_id1,logistic_reg_data$numeric_id2))
  n_players
  # test=dplyr::arrange(logistic_reg_data, Point_non_Server_id, Point_Server_id)
  
  
  logistic_reg_data$Point_non_Server_id
  logistic_reg_data$Point_Server_id
  
  logistic_reg_data$Point_non_Server_id==logistic_reg_data$Point_Server_id
  
  logistic_reg_data[logistic_reg_data$Point_Server_id==501,]
  # 
  # names(match_data)
  # logistic_reg_data = logistic_reg_data %>% 
  #   dplyr::mutate(id1 = match(player1id, sample(unique(player2id))))
  # 
  # logistic_reg_data = logistic_reg_data %>% 
  #   dplyr::mutate(id2 = ifelse(player1id == player2id,id1,match(player1id, sample(unique(player2id)))))
  
  # logistic_reg_data = logistic_reg_data %>% select(-c(player1id,player2id))
  
  as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))
  
  names(logistic_reg_data)
  # 
  # logistic_reg_data=logistic_reg_data %>% dplyr::filter(!is.na(scored1))
  # logistic_reg_data=logistic_reg_data %>% dplyr::filter(!is.na(P1DistanceRun))
  test=logistic_reg_data %>% dplyr::filter(is.na(Point_Server_id))
  # 
  # logistic_reg_data=logistic_reg_data %>% dplyr::filter(!is.na(Point_Server_id))
  # logistic_reg_data=logistic_reg_data %>% dplyr::filter(!is.na(Point_non_Server_id))
  # 
  
  
  n_players =  n_distinct(c(logistic_reg_data$numeric_id1,logistic_reg_data$numeric_id2))
  n_players
  
  
  
  logistic_reg_data_backup = logistic_reg_data
  
  
  # logistic_reg_data = logistic_reg_data %>% tail(n= 10000)
  # logistic_reg_data$PointServer = logistic_reg_data$PointServer-1
  
  n_distinct(logistic_reg_data$Point_Server_id)
  n_distinct(logistic_reg_data$Point_non_Server_id)
  
  # logistic_reg_data = logistic_reg_data %>% head(n=30000)
  
  nrow(logistic_reg_data)
  # served_and_scored
  served_and_scored = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$served_and_scored
  

  points_to_game1 = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$points_remaining_game1 / 4
  points_to_game2 = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$points_remaining_game2 / 4
  points_to_set1 = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$points_remaining_set1 / 24
  points_to_set2 = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$points_remaining_set2 / 24
  points_to_match1 = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$points_remaining_overall1 / 72
  points_to_match2 = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$points_remaining_overall2 / 72
  PointServer = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$PointServer
  P1Ace = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$P1Ace
  P2Ace = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$P2Ace
  P1BreakPointMissed = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$P1BreakPointMissed
  P1BreakPointWon = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$P1BreakPointWon
  id1 = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$id1
  id2 = as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$id2
  P1DistanceRun = scale(as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$P1DistanceRun)
  P2DistanceRun = scale(as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$P2DistanceRun)
  rallynew = scale(as.list(logistic_reg_data%>% select(c(-scored1,-match_id)))$rally_new)
  
  Point_non_Server_id = logistic_reg_data$Point_non_Server_id
  Point_Server_id = logistic_reg_data$Point_Server_id
  
  
  
  n_players =  n_distinct(c(logistic_reg_data$numeric_id1,logistic_reg_data$numeric_id2))
  n_players
  
  
  
 
  logistic_reg_data$game_id = paste0(logistic_reg_data$match_id,"Set", logistic_reg_data$SetNo, "_Game", logistic_reg_data$GameNo, "_" )
  
  logistic_reg_data$game_id= as.factor(logistic_reg_data$game_id)
  levels(logistic_reg_data$game_id) <- seq_along(levels(logistic_reg_data$game_id))
  logistic_reg_data$game_id = as.numeric(logistic_reg_data$game_id)
  #logistic_reg_data$receiver_score=as.integer(logistic_reg_data$receiver_score)
  #logistic_reg_data$server_score=as.integer(logistic_reg_data$server_score)
  
  logistic_reg_data=dplyr::mutate(logistic_reg_data, id = row_number())
  
  
  logistic_reg_data=logistic_reg_data %>% dplyr::group_by(match_id) %>% dplyr::mutate(initial_value= ifelse(row_number()==1,id,NA ), P1DistanceRun = P1DistanceRun,  # Explicitly maintain these columns
                                                                                      P2DistanceRun = P2DistanceRun,rally_new=rally_new) %>% 
    tidyr::fill(initial_value)
  
  common_elements <- intersect(logistic_reg_data$numeric_id1, logistic_reg_data$numeric_id2)
  
  # Count the number of common elements
  num_identical <- length(common_elements)
  num_identical
  
  
  
  logistic_reg_data$server_id_game_id = paste0(logistic_reg_data$match_id,"server_id", logistic_reg_data$Point_Server_id)
  
  logistic_reg_data$server_id_game_id= as.factor(logistic_reg_data$server_id_game_id)
  levels(logistic_reg_data$server_id_game_id) <- seq_along(levels(logistic_reg_data$server_id_game_id))
  logistic_reg_data$server_id_game_id = as.numeric(logistic_reg_data$server_id_game_id)
  
  
  # n_players1_matches,n_players1_matches,player1_id_match_id,player2_id_match_id
  
  
  
  logistic_reg_data$player1_id_match_id = paste0(logistic_reg_data$match_id,"player1id", logistic_reg_data$numeric_id1)
  
  logistic_reg_data$player1_id_match_id= as.factor(logistic_reg_data$player1_id_match_id)
  levels(logistic_reg_data$player1_id_match_id) <- seq_along(levels(logistic_reg_data$player1_id_match_id))
  logistic_reg_data$player1_id_match_id = as.numeric(logistic_reg_data$player1_id_match_id)
  
  n_distinct(Point_non_Server_id)
  n_distinct(Point_Server_id)
  n_players =  n_distinct(c(logistic_reg_data$numeric_id1,logistic_reg_data$numeric_id2))
  
  
  any(Point_non_Server_id==113)
  any(Point_Server_id==113)
  
  sort(unique(Point_non_Server_id))
  sort(unique(Point_Server_id))
  
  shift <- function(x, n){
    c(x[-(seq(n))], rep(NA, n))
  }
  player1_served = logistic_reg_data$player1_served
  
  sign_served = rep(1, length(player1_served))
  for (i in 2:length(player1_served)){
    if (player1_served [i] != player1_served [i-1]){
      sign_served[i] = sign_served[i]*-1
    }
  }
  
  any(is.na(Point_Server_id))
  any(is.na(Point_non_Server_id))
  
  n_players = n_distinct(c(logistic_reg_data$numeric_id1,logistic_reg_data$numeric_id2))
  id1 = logistic_reg_data$numeric_id1
  id2 = logistic_reg_data$numeric_id2
  match_id = as.numeric(as.factor(logistic_reg_data$match_id))
  n_matches = n_distinct(match_id)
  case6=logistic_reg_data
  
  n_distinct(logistic_reg_data$match_id)
  
  
  
  
  
  
  
  # how many matches per player
  n_player_matches = rep(0, n_players)
  player_matches = matrix(0,nrow=n_players, ncol=n_matches)
  for (n in 1:n_matches){
    pl_id1 = mean(id1[match_id==n])
    pl_id2 = mean(id2[match_id==n])
    player_matches[pl_id1,n] = 1
    player_matches[pl_id2,n] = -1
    n_player_matches[pl_id1] = n_player_matches[pl_id1] + 1
    n_player_matches[pl_id2] = n_player_matches[pl_id2] + 1
  }
  players_n_matches$all_matches
  length(matches_players_all$all_matches)
  sum(matches_players_all$all_matches)

  # whether there is serve speed
  ServeNumber = logistic_reg_data$ServeNumber
  ServeNumber[is.na(ServeNumber)] = -1
  is_serve_speed = logistic_reg_data$Speed_KMH>0 & ServeNumber>0
  is_serve_speed[is.na(is_serve_speed)] = F
  
  # compute difference between players' progress
  if (progress_type == "match"){
    progress_1minus2 = (points_to_match2 - points_to_match1) / (points_to_match2 + points_to_match1)
  } else if (progress_type == "game") {
    progress_1minus2 = (points_to_game2 - points_to_game1) / (points_to_game2 + points_to_game1)
  }  
  progress_1minus2_abs = abs(progress_1minus2)

  # include players with both first and second serves
  is_serve_speed_player = rep(0, n_players)
  for (n in 1:n_players){
    if (any(is_serve_speed[Point_Server_id==n]==T & ServeNumber[Point_Server_id==n]==1) & any(is_serve_speed[Point_Server_id==n]==T & ServeNumber[Point_Server_id==n]==2)){
      is_serve_speed_player[n] = 1
    } else {
      is_serve_speed[Point_Server_id==n] = F 
    }
  }

  
    
  # players who served in at least 5 games
  is_serve_speed_player_enough = rep(0, n_players)
  for (n in 1:n_players){
    if (is_serve_speed_player[n]==1 && length(unique(match_id[is_serve_speed==T & Point_Server_id==n]))>=6){
      is_serve_speed_player_enough[n] = 1
    } 
  }
  
  # players who played in at least 5 games
  is_player_enough = rep(0, n_players)
  for (n in 1:n_players){
    if (length(unique(match_id[id1==n | id2==n]))>=6){
      is_player_enough[n] = 1
    } 
  }
  
  players_with_serve_speed_indices = rep(0, n_players)
  players_with_serve_speed_indices[is_serve_speed_player==1] = 1:sum(is_serve_speed_player==1)
  is_serve_speed_indices = which(is_serve_speed)
  n_serve_speed_matches = n_distinct(match_id[is_serve_speed])
  serve_speed_match_id = as.numeric(as.factor(match_id[is_serve_speed]))  
  
  common_elements <- intersect(id1, id2)
  
  # Count the number of common elements
  num_identical <- length(common_elements)
  num_identical
  
  sum(n_player_matches==1)
  N = nrow(logistic_reg_data)
  n_players_with_serve_speed = sum(players_with_serve_speed_indices>0)
  n_players_with_serve_speed_enough = sum(is_serve_speed_player_enough==1)
  n_players_enough = sum(is_player_enough==1)
  players_with_serve_speed_enough_indices = which(is_serve_speed_player_enough[is_serve_speed_player==1]==1)
  players_enough_indices = which(is_player_enough==1)
  
  stan_data <- list(
    n_score = nrow(logistic_reg_data),  # number of observations
    y = logistic_reg_data$served_and_scored,# outcome variable (scored or not)
    serve_speed = scale(as.vector(logistic_reg_data$Speed_KMH[is_serve_speed_indices]))[,1], 
    ServeNumber = ServeNumber, # whether first or second serve
    #points_to_game1 = points_to_game1,
    #points_to_game2 = points_to_game2,
    #points_to_match1 = points_to_match1,    
    #points_to_match2 = points_to_match2,
    Point_Server_id = Point_Server_id,
    Point_Server_id_serve_speed = players_with_serve_speed_indices[Point_Server_id[is_serve_speed_indices]],
    Point_non_Server_id =  Point_non_Server_id,
    P1Ace = P1Ace,
    sign = 2 * logistic_reg_data$served_and_scored - 1,
    P1DistanceRun=as.vector(P1DistanceRun),
    P2DistanceRun=as.vector(P2DistanceRun),
    P1BreakPointMissed = P1BreakPointMissed,
    P1BreakPointWon = P1BreakPointWon,
    id1 = id1,
    id2 = id2,
    match_id = match_id,
    n_matches = n_matches,
    serve_speed_match_id = serve_speed_match_id,
    n_serve_speed_matches = n_serve_speed_matches,
    n_players = n_players,
    n_players_enough = n_players_enough,
    player1_served = logistic_reg_data$player1_served,
    is_serve_speed_indices = is_serve_speed_indices,
    n_players_with_serve_speed = n_players_with_serve_speed,
    n_players_with_serve_speed_enough = n_players_with_serve_speed_enough,
    PointServer = PointServer,
    n_serve_speed = length(is_serve_speed_indices),
    n_player_matches = n_player_matches,
    players_enough_indices = players_enough_indices,
    players_with_serve_speed_enough_indices = players_with_serve_speed_enough_indices

    
    
    )
  
  # compute categories of state in game
  effect = substr(progress_effect, 1, 3)
  if (effect=="int"){
    stan_data <- append(stan_data, list( progress_1minus2 = progress_1minus2,
                                         progress_1minus2_abs = progress_1minus2_abs,
                                         progress_server = progress_1minus2[is_serve_speed_indices]*(PointServer[is_serve_speed_indices]==1) + (-progress_1minus2[is_serve_speed_indices])*(PointServer[is_serve_speed_indices]==2)
    ))
    logistic_reg_data$progress_1minus2 = progress_1minus2
    logistic_reg_data$progress_1minus2_abs = progress_1minus2_abs
    
  } else if (effect=="cat") {
    progress_1minus2_category = rep(2, N)
    progress_1minus2_category[(progress_1minus2) <= -0.5] = 1
    progress_1minus2_category[(progress_1minus2) >= 0.5] = 3
    n_categories = 3
    stan_data <- append(stan_data, list( progress_1minus2 = progress_1minus2_category,
                                          progress_2minus1 = n_categories+1-progress_1minus2_category,
                                          progress_server = progress_1minus2_category[is_serve_speed_indices]*(PointServer[is_serve_speed_indices]==1) + (n_categories+1-progress_1minus2_category[is_serve_speed_indices])*(PointServer[is_serve_speed_indices]==2),
                                          n_categories = n_categories
    ))
  }
  
  return(list(stan_data,logistic_reg_data))
  
}