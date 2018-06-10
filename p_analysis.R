library(plyr)
library(dplyr)
library(fuzzyjoin)

set_factors<- function(frame_to_set,factor_1_set,factor_2_set){
	for (row in 1:nrow(frame_to_set)) {
	date <- frame_to_set[row, "gmDate"]
	team <- frame_to_set[row, "teamAbbr"]
	team_factors = factor_1_set[factor_1_set$gmDate == date & factor_1_set$teamAbbr == team,]
	opponent = team_factors$opptAbbr
    frame_to_set[row, "closeness_factor"] <- team_factors$closeness_factor
    team_winp = factor_2_set[teams==team,]
    opp_winp = factor_2_set[teams==opponent,]
    frame_to_set[row, "competition_factor"] <- if_else(team_winp$team_win_percentage>opp_winp$team_win_percentage,4/15,if_else(team_winp$team_win_percentage==opp_winp$team_win_percentage,1/3,6/15))
   	}
	return(frame_to_set)
}

set_usage_rate<- function(frame_to_set,usage_set){
	for (row in 1:nrow(frame_to_set)) {
	date <- frame_to_set[row, "gmDate"]
	team <- frame_to_set[row, "teamAbbr"]
	fga <- frame_to_set[row, "playFGA"]
	fta <- frame_to_set[row, "playFTA"]
	to <- frame_to_set[row, "playTO"]
	min <- frame_to_set[row, "playMin"]
	team_data <- usage_set[usage_set$gmDate == date & usage_set$teamAbbr == team,]
    numerator <- (fga + (0.44 * fta) + to) * (team_data$teamMin/5)
    denominator <- (min * (team_data$teamFGA + (0.44 * team_data$teamFTA) + team_data$teamTO))
    usage_value <- numerator / denominator
    frame_to_set[row, "usage_rate"] <- usage_value
	}
	return(frame_to_set)
}

set_pie<- function(frame_to_set,usage_set){
	for (row in 1:nrow(frame_to_set)) {
	date <- frame_to_set[row, "gmDate"]
	team <- frame_to_set[row, "teamAbbr"]
	pts <- frame_to_set[row, "playPTS"]
	ast <- frame_to_set[row, "playAST"]
	stl <- frame_to_set[row, "playSTL"]
	blk <- frame_to_set[row, "playBLK"]
	pf <- frame_to_set[row, "playPF"]
	drb <- frame_to_set[row, "playDRB"]
	orb <- frame_to_set[row, "playORB"]
	fgm <- frame_to_set[row, "playFGM"]
	fga <- frame_to_set[row, "playFGA"]
	ftm <- frame_to_set[row, "playFTM"]
	fta <- frame_to_set[row, "playFTA"]
	to <- frame_to_set[row, "playTO"]
	team_data <- usage_set[usage_set$gmDate == date & usage_set$teamAbbr == team,]
    numerator <- pts + fgm + ftm - fga - fta + drb + (0.5 * orb) + ast + stl + (0.5 * blk) - pf - to
    game_pts <- team_data$teamPTS + team_data$opptPTS
    game_fgm <- team_data$teamFGM + team_data$opptFGM
    game_ftm <- team_data$teamFTM + team_data$opptFTM
    game_fga <- team_data$teamFGA + team_data$opptFGA
    game_fta <- team_data$teamFTA + team_data$opptFTA
    game_drb <- team_data$teamDRB + team_data$opptDRB
    game_orb <- team_data$teamORB + team_data$opptORB
    game_ast <- team_data$teamAST + team_data$opptAST
    game_stl <- team_data$teamSTL + team_data$opptSTL
    game_blk <- team_data$teamBLK + team_data$opptBLK
    game_pf <- team_data$teamPF + team_data$opptPF
    game_to <- team_data$teamTO + team_data$opptTO
    denominator <- game_pts + game_fgm + game_ftm - game_fga - game_fta + game_drb + (0.5 * game_orb) + game_ast + game_stl + (0.5 * game_blk) - game_pf - game_to
    pie_value <- numerator / denominator
    frame_to_set[row, "pie"] <- pie_value
	}
	return(frame_to_set)
}

setwd("/player_analysis")
off_box_score <- data.frame(read.csv("2017-18_officialBoxScore.csv", header = TRUE, sep = ","))
player_box_score <- data.frame(read.csv("2017-18_playerBoxScore.csv", header = TRUE, sep = ","))
standings <- data.frame(read.csv("2017-18_standings.csv", header = TRUE, sep = ","))
team_box_score <- data.frame(read.csv("2017-18_teamBoxScore.csv", header = TRUE, sep = ","))

#storing dates as proper format
off_box_score$gmDate <- as.Date(off_box_score$gmDate)
player_box_score$gmDate <- as.Date(player_box_score$gmDate)
standings$stDate <- as.Date(standings$stDate)
team_box_score$gmDate <- as.Date(team_box_score$gmDate)

#Get and store team win percentages
final_standings <- standings[standings$stDate=="2018-04-11",]
teams <- final_standings$teamAbbr
games_won <- final_standings$gameWon
games_lost <- final_standings$gameLost
team_win_percentage <- final_standings$gameWon / 82
team_win_percentage <- data.frame(teams,games_won,games_lost,team_win_percentage)

#closeness factor coefficient
team_box_score$score_closeness <- abs( team_box_score$teamPTS - team_box_score$opptPTS )
team_box_score$closeness_factor <- case_when(
			team_box_score$score_closeness >= 20 ~ 6/15,
			team_box_score$score_closeness >= 6 ~ 1/3,
			TRUE ~ 4/15
			)

player_box_score$closeness_factor <- NA
player_box_score$competition_factor <- NA
#set factors from team_box_score and team_win_percentage
player_box_score <- set_factors(player_box_score,team_box_score,team_win_percentage)

#Time factor coefficient
player_box_score$time_factor <- player_box_score$playMin / 48

#Set usage rate
player_box_score$usage_rate <- NA
player_box_score <- set_usage_rate(player_box_score,team_box_score)

#set pie
player_box_score$pie <- NA
player_box_score <- set_pie(player_box_score,team_box_score)

#set pbr
player_box_score$player_box_rating <- 1000 * player_box_score$usage_rate * player_box_score$time_factor * player_box_score$closeness_factor * player_box_score$competition_factor *player_box_score$pie
player_box_score$games_played <- 1


#set variable for group_by operation
#In this case full name
player_box_score$full_name <- paste(player_box_score$playFNm,player_box_score$playLNm,sep=" ")


#Pbr per game
pbr_per_game <- player_box_score[player_box_score$playStat == "Starter",] %>% group_by(full_name) %>% summarise(
										games_played = sum(games_played),
										player_box_rating = mean(player_box_rating),
										usage_rate = mean(usage_rate),
										pie = mean(pie)
										)

#Losses
loss_data_set <- player_box_score[player_box_score$teamRslt == "Loss" & player_box_score$playStat == "Starter",]
#Pbr per game
plr_per_game <- loss_data_set %>% group_by(full_name) %>% summarise(
										losses = sum(games_played),
										player_loss_rating = mean(player_box_rating),
										usage_rate = mean(usage_rate),
										pie = mean(pie)
										)

#Wins
win_data_set <- player_box_score[player_box_score$teamRslt == "Win" & player_box_score$playStat == "Starter",]
pwr_per_game <- win_data_set %>% group_by(full_name) %>% summarise(
										wins = sum(games_played),
										player_win_rating = mean(player_box_rating),
										usage_rate = mean(usage_rate),
										pie = mean(pie)
										)

pbr_pwr_stats <- pbr_per_game %>% regex_left_join(pwr_per_game, by = c(full_name = "full_name"))
player_stats <- pbr_pwr_stats %>% regex_left_join(plr_per_game, by = c(full_name.x = "full_name"))
stats_for_calculation <- data.frame(player_stats$full_name.x,player_stats$player_box_rating,player_stats$player_win_rating,player_stats$player_loss_rating,player_stats$wins,player_stats$losses)
stats_for_calculation$consistency <- (stats_for_calculation$player_stats.player_loss_rating / stats_for_calculation$player_stats.player_win_rating) * 100
stats_for_calculation$true_impact_rating <- ((stats_for_calculation$player_stats.player_win_rating - stats_for_calculation$player_stats.player_loss_rating) * 10)
player_impact <- stats_for_calculation[(stats_for_calculation$player_stats.win > 30 | stats_for_calculation$player_stats.losses > 30) & stats_for_calculation$consistency > 70,]