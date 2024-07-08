#Old logistic regression code
# __ FITTING THE LOGISTIC REGRESSION MODEL __
valid_matches <- subset(match_unique, !is.na(points2021) & !is.na(opponent_points2021))

result_bin <- ifelse(valid_matches$result == "Win", 1, 0)

#minmax_norm <- function(val) { return( (val - min(val))/(max(val) - min(val)) ) }

#points_adj <- minmax_norm(valid_matches$points)
#opp_points_adj <- minmax_norm(valid_matches$opponent_points)
#dist_adj <- minmax_norm(valid_matches$away_distance_km)


logreg <- glm(data = valid_matches, family = "binomial", formula = result_bin ~ away_distance_km + opponent_points2021 + points2021)
summary(logreg)

logreg_2 <- glm(data = valid_matches, family = binomial(link="logit"), formula = result_bin  ~ opponent_points2021 + points2021)
summary(logreg_2)






#__ Testing inclusion of red cards __

all_matches <- read.csv("./final/unique_redcards.csv")
all_matches_valid <- subset(all_matches, !is.na(points2021) & !is.na(opponent_points2021))

red_result <- ifelse(all_matches_valid$result == "Win", 1,0)
logreg_redcards1 <- glm(data = all_matches_valid, formula = red_result ~ away_distance_km + opponent_points2021 + points2021, family = binomial(link="logit"))
logreg_redcards2 <- glm(data = all_matches_valid, formula = red_result ~ opponent_points2021 + points2021,family = binomial(link="logit") )
logreg_redcards3 <- glm(data = all_matches_valid, formula = red_result ~ 0 + opponent_points2021 + points2021,family = binomial(link="logit") )

summary(logreg_redcards1)
summary(logreg_redcards2)
summary(logreg_redcards3)



# __ PLOTTING THE LOGISTIC REGRESSION MODEL __ 
diffs<- seq(-70, 70, 1)
percentiles <- 1/(1 + exp(-0.02393*diffs))
plot(diffs, percentiles, ylim = c(0,1), main = "Predicted win rate by point difference", xlab = "Point difference (Home - Away)", ylab = "Predicted chance of home win", type = "l", col = colours$blue[3])
abline(v = c(-55, 55), lty = "dashed", col = "grey35")
abline(v = 0, h = 0.5, col = "grey25")
text(c("Lowest\ndifference\nobserved", "Highest\ndifference\nobserved"), x = c(-47, 47), y = c(0.8, 0.2), col = "grey35")

# __ CLASSIFYING MATCH RESULTS __ 
all_diffs <- valid_matches$points2021 - valid_matches$opponent_points2021
cutoff <- 0.5
win_chance <- 1/(1+ exp(-0.02393*all_diffs))
pred_result <- ifelse(win_chance > cutoff, "Win", "Not Win")
actual_result <- ifelse(valid_matches$result == "Win", "Win", "Not Win")
table(pred_result, actual_result)

rm(logreg, logreg_2, diffs, percentiles, result_bin, win_chance, pred_result, actual_result, all_diffs, cutoff)

#Bradley Terry stuff


# BRADLEY TERRY MODEL  - 
library(BradleyTerry2)
match_unique <- read.csv("./final/unique_redcards.csv")
result_bin <- ifelse(match_unique$result == "Win", 1, 0)
team_levels <- factor(team_data$team)
bt_mod <- BTm(outcome = result_bin, player1 = factor(match_unique$team), player2 = factor(match_unique$opponent), family = binomial(link = "logit"), x = TRUE)
s<- summary(bt_mod)


team_with_home <- data.frame(team = factor(match_unique$team,      levels = team_levels), at_home = 0)
opp_with_home <-  data.frame(team = factor(match_unique$opponent,  levels = team_levels), at_home = 1)


bt_home_adv <- BTm(outcome = result_bin, player1 = team_with_home, player2 = opp_with_home, formula = ~ team + at_home, id = "team")
summary(bt_home_adv) # not useful. home teams win half the time anyway.


# INCLUDING TIES AND HOME ADV
# https://davidissamattos.github.io/bpcs/articles/b_ties_and_home_advantage.html#bradley-terry-with-order-effect-home-advantage-

#install.packages("remotes")
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#cmdstanr::install_cmdstan()
#remotes::install_github('davidissamattos/bpcs') #- I think this is the only way to get BT-like models with ties and home adv. effect - they don't make it easy!

library(bpcs)

relevant_data <- match_unique[c("opponent","team", "result")]
relevant_data$result <- ifelse(relevant_data$result == "Win", 1, 
                               ifelse(relevant_data$result == "Loss", 0, 2))
relevant_data <- cbind(relevant_data, data.frame(home_marker = rep(1, nrow(relevant_data))))

mod_ties_home_adv <- bpc(relevant_data, 
                         player0 = 'opponent', 
                         player1 =  'team', 
                         result_column = 'result',
                         z_player1 = 'home_marker',
                         model_type = 'davidson-ordereffect',
                         solve_ties = 'none',
                         iter = 3000)


#example
prop.table(table(predict(mod_ties_home_adv, newdata = data.frame(opponent = "Newcastle United", team = "Leicester City", home_marker = 1))))
# This seems to simulate 100 trials hence the prop table
# Seems to be treating home adv. as 0, results in writeup are done manually using formula from davidson 1977
#



# ___ FORECASTING ___
training <- match_unique[1:350, c("opponent", "team", "result")]
testing <- match_unique[351:380, c("opponent", "team", "result")]

# Bradley Terry model
result_bin <- ifelse(training$result == "Win", 1, 0)
team_levels <- factor(team_data$team)
bt_testing <- BTm(outcome = result_bin, player1 = factor(training$team, levels = team_levels), player2 = factor(training$opponent, levels = team_levels), family = binomial(link = "logit"))
s<- summary(bt_testing)

# Davidson Model
training <- cbind(training, data.frame(dav_result = ifelse(training$result == "Win", 1, 
                                                           ifelse(training$result == "Loss", 0, 2)),
                                       home_marker = rep(1, nrow(training))))

mod_david_testing<- bpc(cbind(training), 
                        player0 = 'opponent', 
                        player1 =  'team', 
                        result_column = 'dav_result',
                        z_player1 = 'home_marker',
                        model_type = 'davidson-ordereffect',
                        solve_ties = 'none',
                        iter = 3000)

#summaries
summary(bt_testing)

# Forecasts
# BT

test_res <- ifelse(testing$result == "Win", 1, 0)
predict(bt_testing, newdata = cbind(testing[,c("opponent", "team")], data.frame(result = test_res)))

res_table <- matrix(0, nrow = 2, ncol = 2)

for (i in 1:30) {
  op <- testing$opponent[i]
  te <- testing$team[i]
  act_res <- test_res[i]
  op_c <- bt_testing$coefficients[paste0("..", op)]
  te_c <- bt_testing$coefficients[paste0("..", te)]
  if (te == "AFC Bournemouth") {
    te_c <- 1
  }
  if (op == "AFC Bournemouth") {
    op_c <- 1
  }
  pred_odds <- te_c - op_c
  pred_prob <- exp(pred_odds)/(1+exp(pred_odds))
  pred_res <- round(pred_prob)
  res_table[pred_res+1, act_res+1] <- res_table[pred_res+1, act_res+1] +1
}
res_table



# Davidson
table(testing$result)

test_res2 <- matrix(0, nrow = 3, ncol = 3)
for (i in 1:30) {
  op <- match(testing$opponent[i], team_data$team)
  te <- match(testing$team[i], team_data$team)
  act_res <- ifelse(testing$result[i] == "Win", 1, 
                    ifelse(testing$result[i] == "Loss", 3, 2))
  phome <- exp(mod_david_testing$hpdi[te,2])
  paway <- exp(mod_david_testing$hpdi[op,2])
  gamma <- exp(mod_david_testing$hpdi[21,2])
  nu <- exp(mod_david_testing$hpdi[22,2])
  denom <- phome + gamma*paway + nu *sqrt(phome*paway)
  PrEachOutcome <- c(phome / denom, nu *sqrt(phome*paway)/ denom, (gamma*paway)/ denom) # home win, draw, away win
  pred_res <- which.max(PrEachOutcome)
  test_res2[pred_res, act_res] <- test_res2[pred_res, act_res] + 1
  print(PrEachOutcome)
}
test_res2 # element [2,3] is predicted draw (2) and actual away win (3). rows are predicted outcomes and columns are actual. Away team never predicted to win!

#prev survival stuff

valid_matches <- subset(match_data, !is.na(points2021) & !is.na(opponent_points2021))
red_card_home <- ifelse(valid_matches$red_cards_home > 0, 1, 0) 
red_card_away <- ifelse(valid_matches$red_cards_away > 0, 1, 0) 

s <- Surv(valid_matches$time_to_first_goal_mins, valid_matches$first_goal_censored)

model_wei_ctns<- flexsurvreg(formula = s ~ home_or_away*(red_card_home + red_card_away + away_distance_km) + points2021 + opponent_points2021, data = valid_matches, dist = "WeibullPH")
model_exp_ctns<- flexsurvreg(formula = s ~ home_or_away*(red_card_home + red_card_away + away_distance_km) + points2021 + opponent_points2021, data = valid_matches, dist = "exponential")

model_wei_fact<- flexsurvreg(formula = s ~ home_or_away*(red_card_home + red_card_away + distance_grouping) + team_grouping + opponent_grouping, data = valid_matches, dist = "WeibullPH")
model_exp_fact<- flexsurvreg(formula = s ~ home_or_away*(red_card_home + red_card_away + distance_grouping) + team_grouping + opponent_grouping, data = valid_matches, dist = "exponential")


model_wei_ctns
model_exp_ctns
model_wei_fact
model_exp_fact



hyptester <- function(model) {
  #90% significance tests on model coefficients
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

hyptester(model_wei_ctns)
hyptester(model_exp_ctns)
hyptester(model_wei_fact)
hyptester(model_exp_fact)
rm(model_exp_fact, model_exp_ctns, model_wei_fact, model_wei_ctns)


#Mostly just removing the insignificant interaction terms.
model_wei_ctns2<- flexsurvreg(formula = s ~ red_card_home + red_card_away + away_distance_km + home_or_away + points2021 + opponent_points2021, data = valid_matches, dist = "WeibullPH")
model_exp_ctns2<- flexsurvreg(formula = s ~ red_card_home + red_card_away + away_distance_km + home_or_away + points2021 + opponent_points2021, data = valid_matches, dist = "exponential")

model_wei_fact2<- flexsurvreg(formula = s ~ red_card_home + red_card_away + home_or_away*distance_grouping + team_grouping + opponent_grouping, data = valid_matches, dist = "WeibullPH")
model_exp_fact2<- flexsurvreg(formula = s ~ red_card_home + red_card_away + home_or_away*distance_grouping + team_grouping + opponent_grouping, data = valid_matches, dist = "exponential")

hyptester(model_wei_ctns2)
hyptester(model_exp_ctns2)
hyptester(model_wei_fact2)
hyptester(model_exp_fact2)
rm(model_exp_fact2, model_exp_ctns2 ,model_wei_fact2, model_wei_ctns2)


model_wei_ctns3<- flexsurvreg(formula = s ~ home_or_away + points2021 + opponent_points2021, data = valid_matches, dist = "WeibullPH")
model_exp_ctns3<- flexsurvreg(formula = s ~ home_or_away + points2021 + opponent_points2021, data = valid_matches, dist = "exponential")
model_wei_fact3<- flexsurvreg(formula = s ~ home_or_away*distance_grouping + team_grouping, data = valid_matches, dist = "WeibullPH")
model_exp_fact3<- flexsurvreg(formula = s ~ home_or_away*distance_grouping + team_grouping, data = valid_matches, dist = "exponential")

hyptester(model_wei_ctns3)
hyptester(model_exp_ctns3) #double checking
hyptester(model_wei_fact3)
hyptester(model_exp_fact3)

# QQ residual plots

cs_wc <- coxsnell_flexsurvreg(model_wei_ctns3)
cs_ec <- coxsnell_flexsurvreg(model_exp_ctns3)
cs_wf <- coxsnell_flexsurvreg(model_wei_fact3)
cs_ef <- coxsnell_flexsurvreg(model_exp_fact3)

cstest_wc <- survfit(Surv(cs_wc$est, valid_matches$first_goal_censored) ~ 1)
plot(cstest_wc, fun="cumhaz", main = "Weibull, ctns",ylim = c(0,4), xlim = c(0,2.5))
abline(0, 1, col="red")

cstest_ec <- survfit(Surv(cs_ec$est, valid_matches$first_goal_censored) ~ 1)
plot(cstest_ec, fun="cumhaz", main = "Exponential, ctns", ylim = c(0,4), xlim = c(0,2.5))
abline(0, 1, col="red")

cstest_wf <- survfit(Surv(cs_wf$est, valid_matches$first_goal_censored) ~ 1)
plot(cstest_wf, fun="cumhaz", main = "Weibull, factors", ylim = c(0,4), xlim = c(0,2.5))
abline(0, 1, col="red")

cstest_ef <- survfit(Surv(cs_ef$est, valid_matches$first_goal_censored) ~ 1)
plot(cstest_ef, fun="cumhaz", main = "Exponential, factors", ylim = c(0,4), xlim = c(0,2.5))
abline(0, 1, col="red")

# Likelihood ratio test

ctns_chisq <- -2*(model_exp_ctns3$loglik-model_wei_ctns3$loglik) # 8.27
fact_chisq <- -2*(model_exp_fact3$loglik-model_wei_fact3$loglik) # 7.62


# ___ COX ___
library(survminer)

coxregfact <- coxph(data = valid_matches, formula = s ~ home_or_away*distance_grouping + team_grouping)
summary(coxregfact)
cox_testfact <- cox.zph(coxregfact)
plot(cox_testfact, main = "Cox Residual Plot", xlab = "Match time", xaxp  = c(0,90,15))
abline(h=0, col = "darkred", lty = "dashed")


coxregctns <- coxph(data = valid_matches, formula = s ~ home_or_away+ points2021 + opponent_points2021)
summary(coxregctns)
cox_testctns <- cox.zph(coxregctns)
plot(cox_testctns, main = "Cox Residual Plot", xlab = "Match time", xaxp  = c(0,90,15))
abline(h=0, col = "darkred")

ggcoxzph_fixed(cox_testfact)
ggcoxzph_fixed(cox_testctns)


to_predict <- data.frame(home_or_away = c("Home", "Away"), distance_grouping = c("farther", "farther"), team_grouping = c("Higher", "Lower"))
hrs <- exp(predict(coxregfact, newdata=to_predict, type = "lp"))
hrs # To double check the workings-out
hrs[1]/hrs[2]
hr_flexsurvreg(x = model_wei_fact3, newdata = to_predict, t = seq(0,90,1))



to_predict <- data.frame(home_or_away = c("Home", "Away"), points2021 = c(69, 49), opponent_points2021 = c(49,69))
hrs <- exp(predict(coxregctns, newdata=to_predict, type = "lp"))
hrs # To double check the workings-out
hrs[1]/hrs[2]
hr_flexsurvreg(x = model_wei_ctns3, newdata = to_predict, t = seq(0,90,1))
#exp(0.32056 + 0.01027*20 + 0.00705*20) gives same result.


model_wei4 <- flexsurvreg(data = valid_matches, dist = "WeibullPH", formula = s ~ home_or_away*(distance_grouping) + points2021 + opponent_points2021)

rm(colours, colours_transparent, cox_test, coxreg, match_data, match_unique, model_ctns_final, team_data, actual_distance, surv, cum_haz_plot, four_line_plot, one_line_plot, two_line_cumhaz, two_line_plot)



