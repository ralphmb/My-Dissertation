data_path = "/Users/ralphbraithwaite/Documents/dissertation/data"
setwd(data_path)
rm(data_path)
locations <- read.csv("longlatpairs.csv")
prev_standings <- read.csv("prev_season_standings.csv")
footystats <- read.csv("footystats-match.csv")
footballdata <- read.csv("./Supplementary:Unused/footballdata-by-match.csv")
library(geosphere)
coordinates <- locations[, c("HomeLong", "HomeLat")]
pairwise_distances <- matrix(NA, nrow = nrow(locations), ncol = nrow(locations))
rownames(pairwise_distances) <- locations$Team
colnames(pairwise_distances) <- locations$Team
for (i in 1:nrow(locations)) {
  for (j in 1:nrow(locations)) {
    pairwise_distances[i, j] <- distVincentyEllipsoid(coordinates[i, ],
                                                      coordinates[j, ])
  }
}
pairwise_distances <- round(pairwise_distances/ 1000)
rm(coordinates, i, j, locations)
team_names <- sort(unique(footystats$home_team_name))
prev_season_data <- prev_standings[,c("Team", "PTS", "W", "Diff")]
data_by_team <- data.frame(team_names)
data_by_team <- merge(data_by_team,
                      prev_season_data,
                      by.x ="team_names",
                      by.y = "Team", all.x = TRUE)
colnames(data_by_team) <- c("team", "points", "wins", "goal_difference")
rm(prev_standings, team_names, prev_season_data)
find_first_goal_time <- function (timings) {
  if (timings == "") {
    is_censored = 0
    goal_time = 90
    return(c(goal_time, is_censored))
  }
  first_time = strsplit(timings,",")[[1]][1] #get first number preceding any comma, just the number if no comma
  extra_time_removed = strsplit(first_time, "'")[[1]][1] #if goal time is 45'2, 90'4 etc, just take the 45 or 90
  goal_time = as.integer(extra_time_removed) 
  is_censored = 1
  if (goal_time == 0) {
    goal_time = 0.5
  }
  return(c(goal_time, is_censored))
}
time_to_first_goal_home <- t(sapply(footystats$home_team_goal_timings,
                                    find_first_goal_time))
time_to_first_goal_away <- t(sapply(footystats$away_team_goal_timings,
                                    find_first_goal_time))
match_distances <- pairwise_distances[cbind(footystats$home_team_name,
                                            footystats$away_team_name)]
home_win_margin <- footystats$home_team_goal_count - footystats$away_team_goal_count
result_for_home <- sign(home_win_margin)
result_for_away <- -sign(home_win_margin)
result_for_home <- factor(result_for_home, labels = c("Loss", "Draw", "Win"))
result_for_away <- factor(result_for_away, labels = c("Loss", "Draw", "Win"))
home_matches <- data.frame(
  footystats$timestamp,
  footystats$home_team_name,
  footystats$away_team_name,
  result_for_home,
  home_win_margin,
  footystats$home_team_goal_count,
  time_to_first_goal_home,
  match_distances,
  footystats$home_team_red_cards,
  footystats$away_team_red_cards,
  rep("Home",nrow(footystats))
)
away_matches <- data.frame(
  footystats$timestamp,
  footystats$away_team_name,
  footystats$home_team_name,
  result_for_away,
  home_win_margin,
  footystats$away_team_goal_count,
  time_to_first_goal_away,
  match_distances,
  footystats$home_team_red_cards,
  footystats$away_team_red_cards,
  rep("Away",nrow(footystats))
)
unique_matches <- data.frame(
  footystats$timestamp,
  footystats$home_team_name,
  footystats$away_team_name,
  result_for_home,
  home_win_margin,
  time_to_first_goal_home,
  match_distances,
  footystats$home_team_red_cards,
  footystats$away_team_red_cards,
  footballdata$B365H,
  footballdata$B365D,
  footballdata$B365A
)
rm(footystats, result_for_away, result_for_home, home_win_margin,
   time_to_first_goal_away, time_to_first_goal_home, match_distances,
   find_first_goal_time, pairwise_distances, footballdata)
cnames <- c("timestamp_unix", "team", "opponent",
            "result", "win_margin", "goal count", "time_to_first_goal_mins",
            "first_goal_censored", "away_distance_km",
            "red_cards_home", "red_cards_away", "home_or_away")
colnames(home_matches) <- c(cnames)
colnames(away_matches) <- c(cnames)
colnames(unique_matches) <- c("timestamp_unix", "team", "opponent",
                              "result", "win_margin", "time_to_first_goal_mins",
                              "first_goal_censored", "away_distance_km",
                              "red_cards_home", "red_cards_away", "bet365odds_home","bet365odds_draw", "bet365odds_away")
rownames(home_matches) <- NULL
rownames(away_matches) <- NULL
rownames(unique_matches) <- NULL
data_by_match <- rbind(home_matches, away_matches)
rm(cnames, home_matches, away_matches)
data_by_match <- merge(data_by_match,
                        data_by_team[,c("team", "points", "wins", "goal_difference")],
                        by.x = "team",
                        by.y = "team")
unique_matches <- merge(unique_matches,
                        data_by_team[,c("team", "points", "wins","goal_difference")],
                        by.x = "team",
                        by.y = "team")
is_derby <- function(teams) {
  rival_pairs = data.frame(
    # Could do this by distance instead
    #https://www.mirror.co.uk/sport/football/news/premier-league-derby-fixtures-dates-27249622
    # Used a small python script to parse these from the website
    A = c("Arsenal", "Tottenham Hotspur"),
    B = c("Leicester City", "Nottingham Forest"),
    C = c("Manchester City", "Manchester United"),
    D = c("West Ham United", "Fulham"),
    E = c("Brentford", "Chelsea"),
    F = c("Chelsea", "Arsenal"),
    G = c("West Ham United", "Crystal Palace"),
    H = c("Arsenal", "West Ham United"),
    I = c("Brentford", "Tottenham Hotspur"),
    J = c("West Ham United", "Brentford"),
    K = c("Aston Villa", "Wolves"),
    L = c("Crystal Palace", "Tottenham Hotspur"),
    M = c("Chelsea", "Crystal Palace"),
    N = c("Fulham", "Tottenham Hotspur"),
    O = c("Chelsea", "Fulham"),
    P = c("Crystal Palace", "Brighton & Hove Albion"),
    Q = c("Liverpool", "Everton"),
    R = c("West Ham United", "Chelsea"),
    S = c("Brentford", "Crystal Palace"),
    T = c("Tottenham Hotspur", "West Ham United"),
    U = c("Tottenham Hotspur", "Chelsea"),
    V = c("Brentford", "Fulham"),
    W = c("Fulham", "Arsenal"),
    X = c("Arsenal", "Crystal Palace")
  )
  any(sapply(rival_pairs, function(pair) { all(teams == pair) || all(rev(teams) == pair) }))
}
data_by_match <- cbind(
  data_by_match,
  apply(data_by_match[,c("team","opponent")],1,is_derby)
)
unique_matches <- cbind(
  unique_matches,
  apply(unique_matches[,c("team","opponent")],1,is_derby)
)
colnames(data_by_match)[ncol(data_by_match)] <- "match_is_derby"
colnames(unique_matches)[ncol(unique_matches)] <- "match_is_derby"
rm(is_derby)
breaks <- c(-Inf, median(data_by_team$points, na.rm=TRUE), Inf)
labels <- c("Lower", "Higher")
data_by_team$grouping <- cut(data_by_team$points, breaks = breaks, labels = labels, include.lowest = TRUE)
data_by_team$grouping[is.na(data_by_team$grouping)] <- "Lower"
rm(breaks)
add_grouping_columns <- function(this_df) {
  team_grouping = c()
  opponent_grouping = c()
  for (i in 1:(nrow(this_df))) {
    team_grouping[i] <- data_by_team$grouping[data_by_team$team == this_df$team[i]]
    opponent_grouping[i] <- data_by_team$grouping[data_by_team$team == this_df$opponent[i]]
  }
  
  team_grouping <- factor(team_grouping, levels = c(1,2), labels = labels)
  opponent_grouping <- factor(opponent_grouping, levels = c(1,2), labels = labels)
  prevnames <- names(this_df)
  
  this_df <- cbind(this_df, team_grouping, opponent_grouping)
  names(this_df) <- c(prevnames, "team_grouping", "opponent_grouping")
  return(this_df)
}
data_by_match <- add_grouping_columns(data_by_match)
unique_matches <- add_grouping_columns(unique_matches)
rm(add_grouping_columns, labels)
breaks = c( -Inf, median(data_by_match$away_distance_km), Inf)
data_by_match$distance_grouping <- cut(data_by_match$away_distance_km,
                                    breaks = c( -Inf, median(data_by_match$away_distance_km), Inf),
                                    labels = c("closer", "farther"),
                                    include.lowest = TRUE
)
unique_matches$distance_grouping <- cut(unique_matches$away_distance_km,
                                      breaks = c( -Inf, median(data_by_match$away_distance_km), Inf),
                                      labels = c("closer", "farther"),
                                      include.lowest = TRUE
)
rm(breaks)
data_by_match <- merge(data_by_match, data_by_team[c("team", "points")], by.x = "opponent", by.y = "team")
names(data_by_match)[names(data_by_match) == "points.x"] <- "points2021" # name got mangled by the merge, resetting
names(data_by_match)[names(data_by_match) == "points.y"] <- "opponent_points2021" # change name of new var
unique_matches <- merge(unique_matches, data_by_team[c("team", "points")], by.x = "opponent", by.y = "team")
names(unique_matches)[names(unique_matches) == "points.x"] <- "points2021"
names(unique_matches)[names(unique_matches) == "points.y"] <- "opponent_points2021"
write.csv(unique_matches, file = "./final/unique_redcards.csv")
write.csv(data_by_match, file = "./final/by_match_redcards.csv")
unique_matches <- subset(unique_matches, red_cards_home == 0 & red_cards_away == 0)
data_by_match <- subset(data_by_match, red_cards_home == 0 & red_cards_away == 0)
write.csv(data_by_team, file = "./final/by_team.csv")
write.csv(data_by_match, file = "./final/by_match.csv")
write.csv(unique_matches, file = "./final/unique.csv")
rm(unique_matches, data_by_match, data_by_team)
