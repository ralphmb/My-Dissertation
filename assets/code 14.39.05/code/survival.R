setwd("/Users/ralphbraithwaite/Documents/dissertation/data/")
match_data <- read.csv("./final/by_match_redcards.csv")
team_data <- read.csv("./final/by_team.csv")
match_unique <- read.csv("./final/unique_redcards.csv")
colours = data.frame(
  blue = c("#BBD5F1", "#78ADE3", "#186FC6"),
  red = c("#EDBFBF","#D97676","#C62C2C"),
  green = c("#BCD4BE","#7AAB7C","#2F7833")
)
colours_transparent <- as.data.frame(lapply(colours, function(x) paste0(x, "7F")))
rownames(colours) <- c("pale", "mid", "vivid")
rownames(colours_transparent) <- c("pale", "mid", "vivid")
library(survival)
library(ggplot2)
one_line_plot<- function (survival_model, this_title) {
  # Give this function the output of a survfit() function
  quarts = c(0.25, 0.5, 0.75)
  quart_labs = paste0( as.character(100 * quarts), "%")
  decs = seq(0, 1, 0.1)
  quints = seq(0, 1, 0.2)
  y_labels = c( "0%", "", "20%", "", "40%", "", "60%", "", "80%", "", "100%")
  xticks = seq(0, 90, 15)
  xbounds = c(0, 90)
  
  summ <- summary(survival_model)
  plot(summ$time, summ$surv, main = this_title, ylab = "Percent chance of having scored no goal",
       xlab = "Match Time (mins)", axes = 0, ylim = c(0,1), xlim = c(0,90))
  # 95% Confidence region1
  polygon(c(summ$time, rev(summ$time)), c(summ$lower, rev(summ$upper)), col = "grey75", border = FALSE)
  lines(summ$time, summ$surv, type="s")
  
  axis(1, at = xticks, lab = xticks, pos = 0) # x axis
  axis(2, at = decs, lab = y_labels, pos = 0) # y axis
  axis(4, at = decs, lab = y_labels, pos = 90) # y axis - right side
  segments( x0 = 2, y0 = quarts, x1 = 88, y1 = quarts, lty = "dashed", col = "darkred") # Quartile markers
  text(quart_labs, x = 10, y= quarts - 0.05, col = "darkred")   # Quartile labels
  segments( x0 = 0, y0 = 1, x1 = 90, y1 = 1, col = "darkgrey") #plot boundary - top
  legend(x = 72, y = 0.99, legend= "95% CI", fill = "grey75")
}
two_line_plot<- function (surv_mod, this_title, reverse_order) {
  
  summ <- summary(surv_mod)
  levs <- levels(summ$strata)
  indic1 <- which(summ$strata == levs[1])
  indic2 <- which(summ$strata == levs[2])
  time1 <- summ$time[indic1]
  time2 <- summ$time[indic2]
  lower1 <- summ$lower[indic1]
  lower2 <- summ$lower[indic2]
  upper1 <- summ$upper[indic1]
  upper2 <- summ$upper[indic2]
  surv1 <- summ$surv[indic1]
  surv2 <- summ$surv[indic2]
  name1 <- strsplit(levs, "=")[[1]][2]
  name2 <- strsplit(levs, "=")[[2]][2]
  
  quarts = c(0.25, 0.5, 0.75)
  quart_labs = paste0( as.character(100 * quarts), "%")
  decs = seq(0, 1, 0.1)
  quints = seq(0, 1, 0.2)
  y_labels = c( "0%", "", "20%", "", "40%", "", "60%", "", "80%", "", "100%")
  xticks = seq(0, 90, 15)
  xbounds = c(0, 90)
  
  plot(surv_mod, main = this_title, ylab = "Percent chance of having scored no goal",
       xlab = "Match Time (mins)", axes = 0, ylim = c(0,1), xlim = c(0,90))
  fills = colours_transparent[c("blue","red")][1,]
  if (reverse_order) {
    polygon(c(time2, rev(time2)), c(lower2, rev(upper2)), col = fills$red, border = FALSE)
    polygon(c(time1, rev(time1)), c(lower1, rev(upper1)), col = fills$blue, border = FALSE)
    lines(time2, surv2, type="s", col = colours["mid", "red"])
    lines(time1, surv1, type="s", col = colours["mid", "blue"])
  }
  else {
    polygon(c(time1, rev(time1)), c(lower1, rev(upper1)), col = fills$blue, border = FALSE)
    polygon(c(time2, rev(time2)), c(lower2, rev(upper2)), col = fills$red, border = FALSE)
    lines(time1, surv1, type="s", col = colours["mid", "blue"])
    lines(time2, surv2, type="s", col = colours["mid", "red"])
  }
  axis(1, at = xticks, lab = xticks, pos = 0) # x axis
  axis(2, at = decs, lab = y_labels, pos = 0) # y axis
  axis(4, at = decs, lab = y_labels, pos = 90) # y axis - right side
  segments( x0 = 2, y0 = quarts, x1 = 88, y1 = quarts, lty = "dashed", col = "darkred") # Quartile markers
  text(quart_labs, x = 10, y= quarts - 0.05, col = "darkred")   # Quartile labels
  segments( x0 = 0, y0 = 1, x1 = 90, y1 = 1, col = "darkgrey") #plot boundary - top
  
  legend(x = 72, y = 0.99, legend = c(name2, name1), fill = c(colours["mid","red"], colours["mid", "blue"]))
}
cum_haz_plot<- function (survival_model, this_title) {
  # Give this function the output of a survfit() function
  xticks = seq(0, 90, 15)
  xbounds = c(0, 90)
  summ <- summary(survival_model)
  
  low <- -log(summ$lower)
  upp <- -log(summ$upper)
  ymax = ceiling(max(low)/0.2) * 0.2
  plot(summ$time, summ$cumhaz, main = this_title, ylab = "Cumulative Hazard",
       xlab = "Match Time (mins)", axes = 0, ylim = c(0, ymax), xlim = c(0,90))
  
  # 95% Confidence region
  low <- -log(summ$lower)
  upp <- -log(summ$upper)
  polygon(c(summ$time, rev(summ$time)), c(low, rev(upp)), col = "grey75", border = FALSE)
  
  lines(summ$time, summ$cumhaz, type="s") #actual plot
  
  # line of best fit
  l <- lm(summ$cumhaz ~ 0 + summ$time)
  abline(a = 0, b = l$coefficients, col = "darkred", lty = "dashed")
  
  print("Gradient of best fit line:")
  print(l$coefficients)
  
  yticks = seq(0,ymax,0.2)
  axis(2, at = yticks, lab = yticks, pos = 0) # y axis
  axis(4, at = yticks, lab = yticks, pos = 90) # y axis - right side
  axis(1, at = xticks, lab = xticks, pos = 0) # x axis
  legend(x = 5, y = ymax * 0.95, legend= c("95% CI", "Linear fit"), fill = c("grey75", "darkred"))
}
two_line_cumhaz<- function (surv_mod, this_title, flip_line) {
  
  summ <- summary(surv_mod)
  levs <- levels(summ$strata)
  indic1 <- which(summ$strata == levs[1])
  indic2 <- which(summ$strata == levs[2])
  time1 <- summ$time[indic1]
  time2 <- summ$time[indic2]
  cumhaz1 <- summ$cumhaz[indic1]
  cumhaz2 <- summ$cumhaz[indic2]
  name1 <- strsplit(levs, "=")[[1]][2]
  name2 <- strsplit(levs, "=")[[2]][2]
  ymax <- ceiling(max(cumhaz2, cumhaz1)*5)/5
  quints = seq(0, ymax, 0.2)
  xticks = seq(0, 90, 15)
  xbounds = c(0, 90)
  
  plot(summ$time, summ$cumhaz, main = this_title, ylab = "Cumulative Hazard",
       xlab = "Match Time (mins)", axes = 0, ylim = c(0,ymax), xlim = c(0,90), col = "white")
  fills = colours_transparent[c("blue","red")][1,]
  # Cum haz lines
  lines(time2, cumhaz2, type="s", col = colours["mid", "red"])
  lines(time1, cumhaz1, type="s", col = colours["mid", "blue"])
  # Log diff line
  interp_cumhaz1 <- approx(x = time1, y = cumhaz1, xout = time2)$y
  lines(time2, flip_line*(log(cumhaz2) - log(interp_cumhaz1)), type = "s", col = "black")
  # Best fit
  l1 <- lm(cumhaz1 ~ 0 + time1)
  l2 <- lm(cumhaz2 ~ 0 + time2)
  abline(a = 0, b = l1$coefficients, col = colours["vivid","blue"], lty = "dashed")
  abline(a = 0, b = l2$coefficients, col = colours["vivid","red"], lty = "dashed")
  abline(h = flip_line*(log(l2$coefficients) -log(l1$coefficients)), col = "darkgray", lty = "dashed")
  print(paste0(name1, "coefficient is ", l1$coefficients))
  print(paste0(name2, "coefficient is ", l2$coefficients))
  print(paste0("their log difference is ", flip_line*(log(l2$coefficients) -log(l1$coefficients))))
  print(paste0("mean of line is ", mean(flip_line*(log(cumhaz2) - log(interp_cumhaz1)))))
  axis(1, at = xticks, lab = xticks, pos = 0) # x axis
  axis(2, at = quints, lab = quints, pos = 0) # y axis
  axis(4, at = quints, lab = quints, pos = 90) # y axis - right side
  
  legend(x = 4, y = 1.3, legend= c(name1, name2, "Difference of logs"), fill = c(colours["mid","blue"], colours["mid", "red"], "black"))
}
four_line_plot<- function (surv_mod, this_title, rates, scale_par) {
  # rates must be sorted
  # works for exp or wei dists only
  summ <- summary(surv_mod)
  levs <- levels(summ$strata)
  indic1 <- which(summ$strata == levs[1])
  indic2 <- which(summ$strata == levs[2])
  indic3 <- which(summ$strata == levs[3])
  indic4 <- which(summ$strata == levs[4])
  time1 <- summ$time[indic1]
  time2 <- summ$time[indic2]
  time3 <- summ$time[indic3]
  time4 <- summ$time[indic4]
  
  surv1 <- summ$surv[indic1]
  surv2 <- summ$surv[indic2]
  surv3 <- summ$surv[indic3]
  surv4 <- summ$surv[indic4]
  
  quarts = c(0.25, 0.5, 0.75)
  quart_labs = paste0( as.character(100 * quarts), "%")
  decs = seq(0, 1, 0.1)
  quints = seq(0, 1, 0.2)
  y_labels = c( "0%", "", "20%", "", "40%", "", "60%", "", "80%", "", "100%")
  xticks = seq(0, 90, 15)
  xbounds = c(0, 90)
  
  plot(surv_mod, main = this_title, ylab = "Percent chance of having scored no goal",
       xlab = "Match Time (mins)", axes = 0, ylim = c(0,1), xlim = c(0,90))
  lines(time4, surv4, type="s", col = "orange")
  lines(time3, surv3, type="s", col = "blue")
  lines(time2, surv2, type="s", col = "green")
  lines(time1, surv1, type="s", col = "red")
  
  times <- sort(unique(summ$time))
  lines(times, exp(-(times/exp(rates[1]))^(1/scale_par)), col = "orange", lty = "dashed") #
  lines(times, exp(-(times/exp(rates[2]))^(1/scale_par)), col = "blue", lty = "dashed")
  lines(times, exp(-(times/exp(rates[3]))^(1/scale_par)), col = "green", lty = "dashed")
  lines( times, exp( -(times/exp(rates[4]))^(1/scale_par) ), col = "red", lty = "dashed")
  axis(1, at = xticks, lab = xticks, pos = 0) # x axis
  axis(2, at = decs, lab = y_labels, pos = 0) # y axis
  axis(4, at = decs, lab = y_labels, pos = 90) # y axis - right side
  segments( x0 = 2, y0 = quarts, x1 = 88, y1 = quarts, lty = "dashed", col = "darkred") # Quartile markers
  text(quart_labs, x = 10, y= quarts - 0.05, col = "darkred")   # Quartile labels
  segments( x0 = 0, y0 = 1, x1 = 90, y1 = 1, col = "darkgrey") #plot boundary - top
}
surv <- Surv(match_data$time_to_first_goal_mins, match_data$first_goal_censored)
surv_model <- survfit(
  surv ~ 1
)
one_line_plot(surv_model, "Survival function for the time to first goal")
cum_haz_plot(surv_model, "Cumulative hazard plot for first-goal times")
surv_homeaway <- survfit( 
  surv ~ match_data$home_or_away
) 
two_line_plot(surv_homeaway, "Time to first goal for home games vs away", 0)
two_line_cumhaz( surv_homeaway, "Cumulative hazard for home vs away sides", 1)
survdiff(surv ~ match_data$home_or_away, rho = 0)
plot(1, type="n", xlab="Log(Match Time)", ylab="Log(- CH)", xlim=c(0, 4.5), ylim=c(-4.554,0.4156), main = "Graphical test for Weibull PH, home vs away")
lines(log(surv_homeaway$time[1:84]),log(surv_homeaway$cumhaz[1:84]), col = colours$red[3])
lines(log(surv_homeaway$time[85:169]),log(surv_homeaway$cumhaz[85:169]), col = colours$blue[3])
surv_grouping <- survfit(
  surv ~  match_data$team_grouping
)
two_line_plot(surv_grouping, "Time to first goal by grouping", 0)
two_line_cumhaz( surv_grouping, "Cumulative hazard for higher vs lower ranked teams", -1)
survdiff(surv ~ match_data$team_grouping, rho = 0)
surv_derby <- survfit(
  surv ~ match_data$match_is_derby
)
two_line_plot(surv_derby, "Time to first goal for derby games (TRUE) vs non derby games", 1)
two_line_cumhaz( surv_derby, "Cumulative hazard for (non-/)derby games", -1)
survdiff(surv ~ match_data$match_is_derby, rho = 1)
away_games <- subset(match_data, home_or_away == "Away")
surv_distance <- survfit(
  Surv(away_games$time_to_first_goal_mins, away_games$first_goal_censored) ~ away_games$distance_grouping
)
two_line_plot(surv_distance, "Time to first goal in away games by distance travelled",0)
two_line_cumhaz( surv_distance, "Cumulative hazard by distance travelled for away sides", -1)
survdiff(Surv(away_games$time_to_first_goal_mins, away_games$first_goal_censored) ~ away_games$distance_grouping, rho = 1)
rm(surv_model, surv_homeaway, surv_grouping, surv_derby, surv_distance, away_games)
library(dplyr)
median_distances <- match_unique %>%
  group_by(opponent) %>%
  summarise(median_distance = median(away_distance_km, na.rm = TRUE))
print(median_distances)
cor.test(median_distances$median_distance, team_data$points)
rm(median_distances)
half_season <- ifelse(match_data$timestamp_unix> median(match_data$timestamp_unix), "Later", "Earlier")
surv_timestamp <- survfit(
  surv ~ half_season
)
two_line_plot(surv_timestamp, "Time to first goal for games earlier/later in the season", 1)
two_line_cumhaz( surv_timestamp, "Cumulative hazard", -1)
survdiff(surv ~ half_season, rho = 1)
two_line_plot(surv_redcards, "Time to first goal for games earlier/later in the season", 1)
two_line_cumhaz( surv_redcards, "Cumulative hazard", 1)
home_games <- subset(match_data, home_or_away == "Home")
survdiff(Surv(home_games$time_to_first_goal_mins, home_games$first_goal_censored) ~ ifelse(home_games$red_cards_home>0, 1, 0), rho = 1)
surv_oppgrouping <- survfit( surv ~ opponent_grouping, data = match_data) #Worth studying
two_line_plot(surv_oppgrouping, "Time to first goal for matches by strength of opponent", 0)
two_line_cumhaz( surv_oppgrouping, "Cumulative hazard by grouping of opponent", 1)
survdiff(surv ~ match_data$opponent_grouping, rho = 0)
rm(half_season, surv_timestamp, surv_oppgrouping)
library(survival)
library(flexsurv)
hyptester <- function(model) {
  # 90% significance tests on model coefficients
  # Saves my eyes a bit, convenience function
  coeffs <- model$coefficients
  ses <- sqrt(diag(vcov(model)))
  zcrit <- qnorm(0.95)
  upper <- coeffs + zcrit*ses
  lower <- coeffs - zcrit*ses
  
  for(i in 1:length(coeffs)) {
    varname <- names(coeffs)[i]
    if(varname == "rate" || varname == "shape" || varname == "scale") {
      next
    }
    verdict <- ifelse(sign(lower[i]) != sign(upper[i]), "Reject", "Accept")
    print(paste0("___", varname, "___"))
    
    print(paste0("Lower:  ", format(round(lower[i], 2), nsmall = 3),", Upper:", format(round(upper[i], 2), nsmall = 3)))
    
    print(paste0(verdict))
  }
}
valid_matches <- subset(match_data, !is.na(points2021) & !is.na(opponent_points2021))
red_card_home <- ifelse(valid_matches$red_cards_home > 0, 1, 0) 
red_card_away <- ifelse(valid_matches$red_cards_away > 0, 1, 0) 
red_card_team <- c()
red_card_opp <- c()
for(i in 1:nrow(valid_matches)) {
  if(valid_matches$home_or_away[i] == "Home"){
    red_card_team[i] <- red_card_home[i]
    red_card_opp[i] <- red_card_away[i]
  } else {
    red_card_team[i] <- red_card_away[i]
    red_card_opp[i] <- red_card_home[i]
  }
}
rm(red_card_away, red_card_home)
s <- Surv(valid_matches$time_to_first_goal_mins, valid_matches$first_goal_censored)
model_wei<- flexsurvreg(formula = s ~ home_or_away*(red_card_team + red_card_opp + distance_grouping) + points2021 + opponent_points2021, data = valid_matches, dist = "WeibullPH")
model_exp<- flexsurvreg(formula = s ~ home_or_away*(red_card_team + red_card_opp + distance_grouping) + points2021 + opponent_points2021, data = valid_matches, dist = "exponential")
model_wei
model_exp
hyptester(model_wei)
hyptester(model_exp)
rm(model_exp, model_wei)
model_wei2<- flexsurvreg(formula = s ~ home_or_away*( distance_grouping) + points2021 + opponent_points2021 + red_card_home + red_card_away, data = valid_matches, dist = "WeibullPH")
model_exp2<- flexsurvreg(formula =s ~ home_or_away*( distance_grouping) + points2021 + opponent_points2021 + red_card_home + red_card_away, data = valid_matches, dist = "exponential")
hyptester(model_wei2)
hyptester(model_exp2)
rm( model_exp2 , model_wei2)
model_wei3<- flexsurvreg(formula = s ~ home_or_away*( distance_grouping) + points2021 + opponent_points2021, data = valid_matches, dist = "WeibullPH")
model_exp3<- flexsurvreg(formula = s ~ home_or_away*( distance_grouping) + points2021 + opponent_points2021, data = valid_matches, dist = "exponential")
hyptester(model_wei3)
hyptester(model_exp3) #double checking
model_exp3
model_wei3
cs_wc <- coxsnell_flexsurvreg(model_wei3)
cs_ec <- coxsnell_flexsurvreg(model_exp3)
cstest_wc <- survfit(Surv(cs_wc$est, valid_matches$first_goal_censored) ~ 1)
plot(cstest_wc, fun="cumhaz", main = "Weibull",ylim = c(0,4), xlim = c(0,2.5))
abline(0, 1, col="red")
cstest_ec <- survfit(Surv(cs_ec$est, valid_matches$first_goal_censored) ~ 1)
plot(cstest_ec, fun="cumhaz", main = "Exponential", ylim = c(0,4), xlim = c(0,2.5))
abline(0, 1, col="red")
chisq_obs <- -2*(model_exp3$loglik-model_wei3$loglik) # =9.07
1-pchisq(9.07, 1)
library(survminer)
coxreg <- coxph(data = valid_matches, formula = s ~ home_or_away*( distance_grouping) + points2021 + opponent_points2021)
summary(coxreg)
cox_test <- cox.zph(coxreg)
cox_test
plot(cox_test, main = "Cox Residual Plot", xlab = "Match time", xaxp  = c(0,90,15))
abline(h=0, col = "darkred", lty = "dashed")
ggcoxzph_fixed(cox_test)
to_predict <- data.frame(home_or_away = c("Home", "Away"), points2021 = c(69, 49), opponent_points2021 = c(49,69), distance_grouping = c("farther", "farther"))
lps <- predict(coxreg, newdata=to_predict, type = "lp", reference = "zero")
hrs <- exp(lps)
lps
hrs # To double check the workings-out
hrs[1]/hrs[2]
hr_flexsurvreg(x = model_wei3, newdata = to_predict, t = seq(0,90,1))
               
rm(colours, colours_transparent, cox_test, coxreg, match_data, match_unique, model_ctns_final, team_data, actual_distance, surv, cum_haz_plot, four_line_plot, one_line_plot, two_line_cumhaz, two_line_plot)
library(Countr)
table(match_data$goal.count, match_data$home_or_away)
count_poiss <- glm(formula = goal.count ~ home_or_away*distance_grouping+points2021+opponent_points2021, family = poisson, data = valid_matches)
count_wei <- renewalCount(formula = goal.count ~ home_or_away*distance_grouping+points2021+opponent_points2021, data = valid_matches, dist = "weibull")
predictions <- compareToGLM(poisson_model = count_poiss, weibull = count_wei, breaks = 0:5)
colnames(predictions) <- c("Counts", "Actual", "Weibull", "Poisson", "weibull_pearson", "poisson_pearson")
frequency_plot(count_labels = predictions$Counts, actual =  predictions$Actual, pred = predictions[c("Weibull", "Poisson")], colours = c("#78ADE3","#D97676", "#7AAB7C"))
lmtest::lrtest(count_wei, count_poiss) # Weibull is no better
chiSq_gof(count_poiss,breaks = 0:5)
chiSq_gof(count_wei,breaks = 0:5) #doesn't work - wonder why
home_counts <- rep(0,90)
away_counts <- rep(0,90)
home_bins <- rep(0,18)
away_bins <- rep(0,18)
footystats <- read.csv("footystats-match.csv")
for (i in 1:nrow(footystats)){
  hometimes <- footystats$home_team_goal_timings[i]
  awaytimes <- footystats$away_team_goal_timings[i]
  home_goals_int <- strsplit(hometimes, ",")[[1]]
  away_goals_int <- strsplit(awaytimes, ",")[[1]]
  for (s in home_goals_int) {
    goaltime <- as.numeric(strsplit(s, "'")[[1]][1])
    goal_bin <- floor(goaltime/5)
    home_bins[goal_bin] <- home_bins[goal_bin] +1
    home_counts[goaltime] <- home_counts[goaltime] +1
  }
  for (s in away_goals_int) {
    goaltime <- as.numeric(strsplit(s, "'")[[1]][1])
    goal_bin <- floor(goaltime/5)
    away_bins[goal_bin] <- away_bins[goal_bin] +1
    away_counts[goaltime] <- away_counts[goaltime] +1
  }
}
barplot(home_bins, ylim = c(0,60)
axis(side=2, pos= -2, at = seq(0,60,15), ylim = c(0,60))
axis(side=1, pos = 0, at = seq(0,18,2))
s <- Surv(valid_matches$time_to_first_goal_mins, valid_matches$first_goal_censored)
valid_model <-survfit( s ~ 1 )
weib_line <- 1.14469*0.00696*(surv_model$time)^(0.14469)
one_line_plot(valid_model, "Kaplan Meier survival curve, Weibull fit overlaid")
lines(valid_model$time, weib_line)
lines(surv_model$time, exp(-0.00696*1.14*(surv_model$time)^(1.14469)))
vcens <- subset(valid_matches, first_goal_censored != 0)
hist(vcens$time_to_first_goal_mins)
lines(surv_model$time,1.14469*0.00696*(surv_model$time)^(0.14469))
lines(surv_model$time, 70*surv_model$surv)
