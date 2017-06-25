#Function to extract season points table for a given league
getleagueSeasonTable <- function(data, league, a_season){
  pointsTable <- data %>% filter(league_id == league, season == a_season)
  # oldnames <- names(pointsTable)
  # newnames <- c('league','season','Team','HM','HW', 'HL','HD','HP','AM','AW','AL','AD','AP','TM','TW','TL','TD','TP')
  # names(pointsTable)[match(oldnames,names(pointsTable))] <- newnames
  return(pointsTable)
}

#Function to extract a team's points across seasons
getTeamAllSeasons <- function(data, team_id){
  pointsTable <- data %>% filter(team_api_id == team_id)
  return(pointsTable)
}

#Function to extract multiple teams' points in a particular season
getNTeamAllSeasons <- function(data, team_id){
  pointsTable <- data %>% filter(team_api_id %in% team_id)
  return(pointsTable)
}

#Function to extract a team's points in a particular season
getTeamONESeason <- function(data, team_id, a_season){
  pointsTable <- data %>% filter(team_api_id == team_id, season == a_season)
  return(pointsTable)
}

#Function to extract all teams that belong to a league
getLeagueTeams <- function(data, league){
  teams <- data %>% filter(league_id == league) %>% distinct(team_api_id, .keep_all = TRUE) %>% select(team_api_id, team_long_name)
  return(teams)
}