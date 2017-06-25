library(dplyr)
library(RSQLite)

source('./functions.R')

#Show all columns when printing tbl_df
options(dplyr.width = Inf)

#Establish connection
con <- dbConnect(SQLite(), dbname="./data/database.sqlite")
#List all the tables
dbListTables(con)

#Read the data from db
#player       <- tbl_df(dbGetQuery(con,"SELECT * FROM Player"))
#player_stats <- tbl_df(dbGetQuery(con,"SELECT * FROM Player_Attributes"))
country <- tbl_df(dbGetQuery(con,"SELECT * FROM Country"))
league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
match <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
team <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
team_attrributes <- tbl_df(dbGetQuery(con,"SELECT * FROM Team_Attributes"))

#Select the relevant columns
match <- match %>% select(id, country_id, league_id, season, match_api_id, date,
                          home_team_api_id, away_team_api_id, home_team_goal, away_team_goal)

#calculate home team points
match <- match %>% mutate(home_team_points = ifelse(home_team_goal > away_team_goal, 3,
                                                    ifelse(home_team_goal == away_team_goal, 1, 0)))

#calculate home team wins, draws & losses
match <- match %>% mutate(home_win = ifelse(home_team_goal > away_team_goal, 1, 0)) %>%
                   mutate(home_loss = ifelse(home_team_goal < away_team_goal, 1, 0)) %>%
                   mutate(home_draw = ifelse(home_team_goal == away_team_goal, 1, 0)) 

#calculate away team points
match <- match %>% mutate(away_team_points = ifelse(home_team_goal < away_team_goal, 3,
                                                    ifelse(home_team_goal == away_team_goal, 1, 0)))

#calculate away team wins, draws & losses
match <- match %>% mutate(away_win = ifelse(home_team_goal < away_team_goal, 1, 0)) %>%
                   mutate(away_loss = ifelse(home_team_goal > away_team_goal, 1, 0)) %>%
                   mutate(away_draw = ifelse(home_team_goal == away_team_goal, 1, 0))

#calculate total home team points, matches by team by league & season & rename
team_home_stats <- match %>% group_by(league_id, season, home_team_api_id) %>% 
                              summarise(total_home_points = sum(home_team_points), 
                                        home_matches = n(),
                                        total_home_wins = sum(home_win),
                                        total_home_losses = sum(home_loss),
                                        total_home_draws = sum(home_draw)) %>% 
                              rename(team_api_id  = home_team_api_id)

#calculate total away team points by team by season and league
team_away_stats <- match %>% group_by(league_id, season, away_team_api_id) %>% 
                              summarise(total_away_points = sum(away_team_points),
                                        away_matches = n(),
                                        total_away_wins = sum(away_win),
                                        total_away_losses = sum(away_loss),
                                        total_away_draws = sum(away_draw)) %>%
                              rename(team_api_id  = away_team_api_id)

#Join the 2 tables
#Add the team names
team_season_details <- team_home_stats %>% left_join(team_away_stats) %>% left_join(team)

#Calculate total season details
team_season_details <- team_season_details %>% mutate(total_matches = home_matches + away_matches,
                                                      total_wins = total_home_wins + total_away_wins,
                                                      total_draws = total_home_draws + total_away_draws,
                                                      total_loss = total_home_losses + total_away_losses,
                                                      total_season_points = total_home_points + total_away_points)

#Capture the League Names
team_season_details <- team_season_details %>% left_join(league, by = c("league_id" = "id"))

#Remove unwanted fields
team_season_details <- select(team_season_details, -team_fifa_api_id, -id)

#Rearrange the tbl
team_season_details <- team_season_details[, c(21, 22, 1:3, 14:15, 5:8, 4, 10:13, 9,16:20)]

rm(team_away_stats, team_home_stats)

team_attrributes$season <- sapply(team_attrributes$date, switch,"2010-02-22 00:00:00" = '2009/2010',
                                                                "2014-09-19 00:00:00" = '2014/2015',
                                                                "2015-09-10 00:00:00" = '2015/2016',
                                                                "2011-02-22 00:00:00" = '2010/2011',
                                                                "2012-02-22 00:00:00" = '2011/2012',
                                                                "2013-09-20 00:00:00" = '2013/2014')

temp <- team_attrributes %>% filter(season == '2009/2010' | season == '2011/2012')
temp$season[temp$season == '2009/2010'] <- '2008/2009'
temp$season[temp$season == '2011/2012'] <- '2012/2013'
team_attrributes <- rbind(team_attrributes, temp)

#Create a merged dataset for each teams performance & attributes in every season
team_merged <- team_season_details %>% left_join(team_attrributes, by = c("team_api_id", "season"))
team_merged <- team_merged[,c(1:7, 22, 26,28,30,33,35,37,40,42,44)]

rm(temp,team_attrributes)

team_Summary <- team_season_details %>% group_by(league_id, team_api_id) %>% 
                summarise(Total_Matches = sum(total_matches), Wins = sum(total_wins),
                          Losses = sum(total_loss),
                          Win_Perc = round((Wins/Total_Matches) * 100, 1)) %>%
                left_join(team) %>%
                select(team_api_id, 'Team' = team_long_name, Wins, Losses, 'Win %' = Win_Perc, 
                       'Total Matches' = Total_Matches)

#Git changes
