setwd("/Users/ralphbraithwaite/Documents/dissertation/data/")
team_data <- read.csv("./final/by_team.csv")
unique <- read.csv("./final/unique_redcards.csv")
colours = data.frame(
  blue = c("#BBD5F1", "#78ADE3", "#186FC6"),
  red = c("#EDBFBF","#D97676","#C62C2C"),
  green = c("#BCD4BE","#7AAB7C","#2F7833")
)
colours_transparent <- as.data.frame(lapply(colours, function(x) paste0(x, "7F")))
rownames(colours) <- c("pale", "mid", "vivid")
rownames(colours_transparent) <- c("pale", "mid", "vivid")
valid_matches <- subset(unique, !is.na(points2021) & !is.na(opponent_points2021))
result_bin <- ifelse(valid_matches$result == "Win", 1, 0)
red_card_home <- ifelse(valid_matches$red_cards_home > 0, 1, 0) 
red_card_away <- ifelse(valid_matches$red_cards_away > 0, 1, 0) 
late_season <- ifelse(valid_matches$timestamp_unix > median(valid_matches$timestamp_unix), 1, 0)
logreg_full <- glm(data = valid_matches, family = binomial(link="logit"), formula = result_bin ~ red_card_away + red_card_home + distance_grouping + opponent_points2021 + points2021 + match_is_derby + late_season)
summary(logreg_full)
logreg_2 <- glm(data = valid_matches, family = binomial(link="logit"), formula = result_bin ~ red_card_home + distance_grouping + opponent_points2021 + points2021 + late_season)
summary(logreg_2)
logreg_interactions <- glm( data = valid_matches, family = binomial(link="logit"), formula = result_bin ~ red_card_home + distance_grouping + opponent_points2021 + (points2021)*late_season)
summary(logreg_interactions) #no relevant interactions - moving on. (I did test other interactions)
logreg_3 <- glm(data = valid_matches, family = binomial(link="logit"), formula = result_bin ~ red_card_home + opponent_points2021 + points2021 + distance_grouping)
lr3_summ <- summary(logreg_3)
lr3_summ
lr3_summ$coefficients
rm(logreg_2, logreg_full, logreg_interactions)
upper <- exp(lr3_summ$coefficients[,1] + 1.96*lr3_summ$coefficients[,2])
lower <- exp(lr3_summ$coefficients[,1] - 1.96*lr3_summ$coefficients[,2])
normal_values <- qnorm(c(0.025, 0.5, 0.975))
table <- matrix(nrow = 5, ncol = 3)
for (i in 1:5) {
  for(j in 1:3){
    table[i,j] <- exp(lr3_summ$coefficients[i,1] + normal_values[j]*lr3_summ$coefficients[i,2])
  }
}
table
prob_of_win <- function(homepoints, awaypoints, distance_level) {
  res <- predict(logreg_3, 
          newdata = data.frame(red_card_home = 0, points2021 = homepoints, opponent_points2021 = awaypoints, distance_grouping = distance_level),
          type = "response")
  return( res )
}
x <- seq(35, 95, length.out = 100)
y <- seq(35, 95, length.out = 100)
grid <- expand.grid(x = x, y = y)
z <- prob_of_win(grid$x, grid$y, "farther")
filled.contour(x = x, y = y, z = matrix(z, nrow = length(x)), 
               main = "Home win chance for more distant teams", xlab = "Home points", ylab = "Away Points", 
              key.title = title(main = "Chance\nof win"),
              plot.axes = {
                axis(1)
                axis(2)
                contour(x,y,matrix(z, nrow = length(x)), add = TRUE, lwd = 2, levels = c(0.25,0.5,0.75) )
                })
segments(x0 = 30, y0 = 31.5, x1 = 81.65, y1 = 95.6, col = "black", lty = "dashed")
z <- prob_of_win(grid$x, grid$y, "closer")
filled.contour(x = x, y = y, z = matrix(z, nrow = length(x)), 
               main = "Home win chance for closer located teams", xlab = "Home points", ylab = "Away Points", 
               key.title = title(main = "Chance\nof win"),
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(x,y,matrix(z, nrow = length(x)), add = TRUE, lwd = 2, levels = c(0.25,0.5,0.75) )
               })
segments(x0 = 30, y0 = 31.5, x1 = 81.65, y1 = 95.6, col = "black", lty = "dashed")
model_null <- glm(result_bin ~ 1, family = binomial)
R2_MF <- 1-0.9210946
library(MASS)
results <- factor(valid_matches$result, labels = c("Loss", "Draw", "Win"))
olog_model_prelim <- polr(formula = results ~ red_card_home + red_card_away + points2021 + opponent_points2021 + distance_grouping + late_season, data = valid_matches, Hess = TRUE) #all variables - too many low t values
summary(olog_model_prelim)
olog_model_2 <- polr(formula = results ~ red_card_home + points2021 + opponent_points2021 + distance_grouping, data = valid_matches, method="logistic")
summary(olog_model_2)
predict(predict(olog_model_2, newdata = data.frame(red_card_home = 0, points2021 = 74, opponent_points2021 = 69, distance_grouping = "closer"), type = "probs"))
library(nnet)
multinom_mod <- multinom( result ~ red_card_home + points2021 + opponent_points2021 + distance_grouping, data = valid_matches)
summary(multinom_mod)
df_olrm <- 12
df_multinom <- 20
p_val <- 1-pchisq(abs(olog_model_2$deviance-multinom_mod$deviance),   abs(df_multinom-df_olrm))
library(brant)
brant(olog_model_2)
olog_model_factors <- polr(formula = results ~ red_card_home + team_grouping+ opponent_grouping + distance_grouping, data = valid_matches, method="logistic")
brant(olog_model_factors) # this works - explain why.
set.seed(4)
sample <- sample(c(TRUE, FALSE), nrow(valid_matches), replace=TRUE, prob=c(0.8,0.2))
training  <- valid_matches[sample, ]
testing   <- valid_matches[!sample, ]
train_res_bin <- ifelse(training$result == "Win", 1, 0)
test_res_bin <- ifelse(testing$result == "Win", 1, 0)
red_card_train <- ifelse(training$red_cards_home > 0, 1, 0)
red_card_test <- ifelse(testing$red_cards_home > 0, 1, 0)
bookies_odds <- data.frame(testing[,c("bet365odds_home", "bet365odds_draw", "bet365odds_away")])
colnames(bookies_odds) <- c("Home", "Tie", "Away")
logreg_test <- glm(data = training, family = binomial(link="logit"), formula = train_res_bin ~ red_card_train + opponent_points2021 + points2021 + distance_grouping)
to_predict <- data.frame(red_card_train = red_card_test, opponent_points2021 = testing$opponent_points2021, points2021 = testing$points2021, distance_grouping = testing$distance_grouping)
lps <- predict(logreg_test, newdata = to_predict)
probs <- 1/(1 + exp(-1*lps))
table(ifelse(sign(lps) == 1, "Pred win", "Pred tie/loss"), ifelse(test_res_bin == 1, "Actual win", "Tie/loss"))
bookie_mat <- matrix(0, nrow =2, ncol =2)
our_mat <- matrix(0, nrow =2, ncol =2)
for (i in 1:nrow(testing)) {
  res <- ifelse(test_res_bin[i] ==1, 2, 1)
  bookie <- ifelse(match(min(bookies_odds[i,]),bookies_odds[i,]) == 1, 2, 1) # 1 home 2 tie 3 away
  pred <- ifelse(probs[i] > 0.5, 2, 1)
  bookie_mat[bookie, res] <- bookie_mat[bookie, res] + 1
  our_mat[pred, res] <- our_mat[pred, res] + 1
}
bookie_mat
our_mat
olrm_res_train <- factor(training$result, labels = c("Loss", "Draw", "Win"))
olrm_test <- polr(formula = olrm_res_train ~ red_card_train + opponent_points2021 + points2021 + distance_grouping, data = training, method="logistic")
to_predict <- data.frame(red_card_train = red_card_test, opponent_points2021 = testing$opponent_points2021, points2021 = testing$points2021, distance_grouping = testing$distance_grouping)
probs <- predict(olrm_test, newdata = to_predict,type = "probs")
get_min_column <- function(row) {
  names(which.min(row))[1]
}
get_max_column <- function(row) {
  names(which.max(row))[1]
}
bookie_pred <- c()
our_pred <- c()
for (i in 1:nrow(testing)) {
  bookie_pred <- append(bookie_pred, get_min_column(bookies_odds[i,]))
  our_pred <- append(our_pred, get_max_column(probs[i,]))
}
print(bookie_pred)
print(our_pred)
table(bookie_pred, testing$result)
table(our_pred, testing$result)
olrm_res_train <- factor(training$result, labels = c("Loss", "Draw", "Win"))
olrm_testfact <- polr(formula = olrm_res_train ~ red_card_train + team_grouping + opponent_grouping + distance_grouping, data = training, method="logistic")
to_predict <- data.frame(red_card_train = red_card_test, opponent_grouping = testing$opponent_grouping, team_grouping= testing$team_grouping, distance_grouping = testing$distance_grouping)
probs <- predict(olrm_testfact, newdata = to_predict,type = "probs")
bookie_pred <- c()
our_pred <- c()
for (i in 1:nrow(testing)) {
  bookie_pred <- append(bookie_pred, get_min_column(bookies_odds[i,]))
  our_pred <- append(our_pred, get_max_column(probs[i,]))
}
print(bookie_pred)
print(our_pred)
table(bookie_pred, testing$result)
table(our_pred, testing$result)
  
