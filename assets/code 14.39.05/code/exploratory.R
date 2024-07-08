setwd("/Users/ralphbraithwaite/Documents/dissertation/data/")
match_data <- read.csv("./final/by_match.csv")
team_data <- read.csv("./final/by_team.csv")
match_unique <- read.csv("./final/unique.csv")
colours = data.frame(
  blue = c("#BBD5F1", "#78ADE3", "#186FC6"),
  red = c("#EDBFBF","#D97676","#C62C2C"),
  green = c("#BCD4BE","#7AAB7C","#2F7833")
)
colours_transparent <- as.data.frame(lapply(colours, function(x) paste0(x, "7F")))
rownames(colours) <- c("pale", "mid", "vivid")
rownames(colours_transparent) <- c("pale", "mid", "vivid")
row_order <- c("Win", "Draw", "Loss")
result_table <- table(match_unique$result, match_unique$team_grouping)[row_order,]
ptab_result_group <- prop.table(result_table, margin = 2)
barplot(ptab_result_group, ylab = "Proportions of each result", xlab = "Team grouping based on last-season league points", main = "Results by grouping", col = colours$blue, xlim = c(0,3))
legend("topright", legend = rev(rownames(ptab_result_group)), fill = rev(colours$blue))
segments(x0 = 0, y0 = 0.5, x1 = 2.6, y1 = 0.5, lty = "dashed", col = "darkred")
text("50%", x= 2.6, y= 0.45, col = "darkred")
rm(result_table, ptab_result_group)
result_table <- table(match_data$result, match_data$team_grouping)[row_order,]
ptab_result_group <- prop.table(result_table, margin = 2)
barplot(ptab_result_group, ylab = "Proportions of each result", xlab = "Team grouping based on last-season league points", col = colours$red, xlim = c(0,3))
legend("topright", legend = rev(rownames(ptab_result_group)), fill = rev(colours$red))
segments(x0 = 0, y0 = 0.5, x1 = 2.6, y1 = 0.5, lty = "dashed", col = "darkred")
text("50%", x= 2.6, y= 0.45, col = "darkred")
rm(result_table, ptab_result_group)
tab_homeaway <- table(match_data$result, match_data$home_or_away)[row_order,]
ptab_homeaway <- prop.table(tab_homeaway, margin = 2)
barplot(ptab_homeaway, ylab = "Proportions of each result", xlab = "Home or Away games", col = colours$green, xlim = c(0,3))
legend("topright", legend = rev(rownames(ptab_homeaway)), fill = rev(colours$green))
segments(x0 = 0, y0 = 0.5, x1 = 2.6, y1 = 0.5, lty = "dashed", col = "darkred")
text("50%", x= 2.6, y= 0.45, col = "darkred")
rm(tab_homeaway, ptab_homeaway)
group_home_result_tab <- xtabs( data = match_data, formula = ~ result + home_or_away + team_grouping)
group_home_result_ptab <-  prop.table(group_home_result_tab)
two_var_stack_plotter <- function(ptable, this_title) {
  df <- data.frame(matrix(nrow = 3, ncol = 0)) # nrow needs to = 3 for cbind to work
  these_names <- c()
  for (i in c(1, 2)) {
    for (j in c(1, 2)) {
      newcol <- ptable[,i,j] / sum(ptable[,i,j])
      df <- cbind(df, newcol)
      fact1 <- strsplit(dimnames(ptable)[2][[1]], " ")[j]
      fact2 <- strsplit(dimnames(ptable)[3][[1]], " ")[i]
      these_names[2*(j-1)+(i)] <- paste0(fact1, " + ",  fact2)
    }
  }
  
  df <- df[c("Win", "Draw", "Loss"),]
  colnames(df) <- these_names
  print(df)
  
  barplot(height = as.matrix(df), col = colours$blue,
          xlim = c(0,6), ylim = c(0,1.19),
          main = this_title, xlab = "(Team + Opponent) Rankings")
  legend("topright", legend = rev(rownames(df)), fill = rev(colours$blue))
  segments(x0 = 0, y0 = 0.5, x1 = 5, y1 = 0.5, lty = "dashed", col = "darkred")
  text("50%", x= 5, y= 0.45, col = "darkred")
}
two_var_stack_plotter(group_home_result_ptab, "The effect of side + team grouping on match result")
rm(group_home_result_tab,group_home_result_ptab)
rankingvranking_tab <- xtabs( data = match_unique, formula = ~ result + opponent_grouping + team_grouping)
rvr_ptab <-  prop.table(rankingvranking_tab)
two_var_stack_plotter(rvr_ptab, "Home result by grouping of each team")
rankingvranking_tab <- xtabs( data = match_data, formula = ~ result + opponent_grouping + team_grouping)
rvr_ptab <-  prop.table(rankingvranking_tab)
two_var_stack_plotter(rvr_ptab, "Result for team by ranking of opponent")
flatRes <- ifelse(match_data$Result == "Win", "Win", "Not Win")
flatRes_unique <- ifelse(match_unique$result == "Win", "Win", "Not Win")
result_by_homeaway <- table(match_data$result, match_data$home_or_away)[row_order,]
result_by_grouping <- table(match_unique$result,match_unique$team_grouping)[row_order,]
result_by_derby <- table(match_unique$result, match_unique$match_is_derby)
median_distance <- median(unique(match_unique$away_distance_km))
above_median_distance <- ifelse(match_data$away_distance_km > median_distance, TRUE, FALSE)
plot(match_data$result, match_data$distance_km)
rm(median_distance, above_median_distance,result_by_derby, result_by_grouping, result_by_homeaway)
cor.test(team_data$points2021,team_data$wins2021) # r = 0.984, p = 1e-12
cor.test(team_data$points2021, team_data$goal_difference2021) # r = 0.967, p = 2e-10
rm(additional_predictors)
avgPoints <- mean(match_unique$points2021, na.rm = TRUE)
mod_allpoints <- lm(data = match_unique, 
                    formula = win_margin ~  I(points2021 - avgPoints)+ I(opponent_points2021 - avgPoints))
summary(mod_allpoints)
inter <- mod_allpoints$coefficients[1]
al1 <- mod_allpoints$coefficients[2]
al2 <- mod_allpoints$coefficients[3]
exampleTeams <- c("Man City", "Arsenal", "Average", "Leeds")
examplePoints <- c(93, 69, avgPoints, 38)
these_colours <- c("#186FC6", "#C62C2C", "black", "#2F7833")
  
plot(match_unique$points, match_unique$win_margin, xlab = "Last-season points of home team", ylab = "Home team's win margin", main = "Team strength against win margin", col = colours["mid","red"])
csts <- inter - (al1+al2)*avgPoints + al2*examplePoints
for (i in 1:4) {
    abline(a = csts[i], b = al1, col = these_colours[i], lty = "dashed")
}
legend(x= 77, y = 8.5, legend = paste0("Vs. ", exampleTeams), fill = these_colours)
abline(h=0, col = "grey75")
mod_test = lm(data = match_unique, 
              formula = win_margin ~  points2021+ opponent_points2021)
