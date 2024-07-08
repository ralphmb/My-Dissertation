There is a wealth of literature concerning itself with the statistical modelling of football.
For fans of the sport it is perhaps the unpredictability of the game that makes it appealing, as while team strength is of course a large factor in game outcome, the multitude of other causal variables can lead to upset wins and tightly-fought battles.
This same reasoning might explain the large base of statistical research into the game, as statisticians and sport scientists seek to quantify the effects of myriad different factors.
Football is big business, with the English Premier League (EPL) having contributed £7.6 billion to the UK's gross domestic product in 2019/2020 @premgdp, invested throughout the country and disproportionately in more deprived areas such as the north west.
In the year preceding Dec 2020, 2.5 million Britons placed bets on Football @gambling, and the Financial Times notes that in the year to March 2022 £1.1bn was taken in revenue for gambling operators, nearly half the £2.3bn figure for their total revenue @ftgambling.
Statistical prediction of football matches is naturally very important to bookmakers, seeking to maximise their revenues by setting optimal odds for games.

Linear models predicting match outcome based on various pre-match variables are ubiquitous, but there are examples of authors applying time-series techniques to assess minute-to-minute statistics based on in-game events such as corner kicks and yellow cards. Some authors have applied Bayesian network techniques @razali2017 and in many more recent papers we see the use of machine learning models to answer similar questions.
In this section we hope to explore a limited number of techniques applied to football modelling.
// TODO - finish this para. ^^^

== Logistic Regression
Logistic regression is a common tool for modelling situations where outcomes can be classified as a binary variable.
In football, the result of a match is naturally three-valued - win vs draw vs loss - but this can be recast as a binary response: win vs not a win.
It is used as a tool for predicting outcome in many sports @asianhockey @willoughby, but its use in football is well trodden.
Here we will study one example.
// PRASETIO HARLILI
#cite(<prasetioharlili>, form: "prose") train a logistic regression model on variables representing defense/offense ratings against the logit of win chance.

//The logistic regression model can be written as follows:

//$ log Pi/(1-Pi) = beta_0 + sum_(i=1)^4 beta_i X_i $

//Where $Pi$ represents $ "Pr"("Win")$ and $beta_j$ are coefficients to be determined.

The authors use win/loss data from the English premier league from between 2010 and 2016.
They fit a number of models, excluding different subsets of the data for each, in order to examine to predictive usefulness of older data and for out-of-sample forecasting of newer data. 
All the models are tested on 2015/16 season data.
The authors determine that the defense variables are more significant than the offense ones, and examine the tradeoff between losing some predictive power when the offense variables are dropped, vs having a simpler model.
Part of their conclusion is that the ease of interpretation of logistic regression makes it a useful tool, as well as the good power of the technique to make accurate predictions.

== Survival Analysis
Goals are the main event of interest for both fans and players in football, and  the time-to-first-goal (TTFG) is a key measure for research.
Intuitively this may be seen as a measure of a team's offensive prowess over their opponents, and indeed studies have shown it to be a very useful predictor of match outcome:
#cite(<ibanezetal>, form: "prose") conducted a study of European women's football and found that teams who score first were 5 times more likely to win.
Survival analysis is a natural tool for modelling this, as it concerns itself with durations until or between events that may not be fully observed.
In football terms this is because the TTFG is often cut off, or censored, by the end of the match. Some authors censor at other events, for instance at points when the other team scores, as this changes the dynamic of the game.
Modelling censored time-to-event data can be done in many ways. Cox regression is a common tool, particularly in medical statistics, though parametric models are also common.

// NEVO RITOV
A paper by #cite(<nevoritov>, form: "prose") undertakes an in-depth analysis of the times to first and second goals using the Cox proportional hazard regression model.
Their analysis covers around 760 matches between 2008 and 2010 in the English premier league.
They use data on the times in minutes of the first and second goals, as well as some other information such as the times that red cards are shown to a team, as well as the season and home team.
// They use data on the times in minutes of the first and second goals. /*, ignoring stoppage time, so a goal scored in the second minute of stoppage in the first half would still be counted a 45th minute goal.*/

Goal times are censored at 90 minutes if they do not occur during the match, and time-to-second-goal is left-truncated since it cannot occur until the first goal has been scored.
Their given reason for focusing on the first two goals is that they'd find much heavier censoring if they included data for third or fourth goals - as football tends to be a fairly low-scoring sport, this would then require far more data to make a model accurate at higher goal counts.
//They find that of the 760 games, 1433 relevant goals were observed and 698 of these were censored.
Two nested models for the hazard function are introduced, a null model and a complete one.
The complete Cox hazard function is given as follows:
#let wspace(t) = style(styles => {
  let size = measure(t, styles)
  [#size.width.cm()]
})
#let gap= [#wspace($ I_(j=2)( $)] // 0.685

$ h_(i j) (t) &= h_0(t) exp( \
   &beta_1 "ProbWin"_i + beta_2 "Season"_i \
   &+ beta_3 "RedCardsAway"_i (t) \
  &+ I_(j=2)(beta_4 "Goal"_i \
  &#h(0.68cm)+ beta_5 "TimeOfFirstGoal"_i \
  &#h(0.68cm)+ beta_6 "FirstGoalTeam"_i \
  &#h(0.68cm)+ beta_7 "TimeFromFirstGoal"_i) \
  ) $

Where $j in {1,2}$ is the goal number, $i in {1, ..., 760}$ is the match number, and $I_(j=2)$ is an indicator function.
The function $I$ has the purpose of changing the hazard when the first goal has been scored.
All variables here except $"FirstGoalTeam"$ are kept, though the significance of $"Goal"$ and $"TimeFromFirstGoal"$ are low ($ p =0.058, 0.11$ resp).
The authors experiment with a 'frailty' term to account for random effects, however this is later shown to not be significant.
They find that red cards for the away team raise the hazard by 1.806 times, and that the change in hazard after the first goal depends on the time of the match, going down if the first goal occurs before 51 minutes and up afterwards.
The authors draft some ideas to explain parts of the model, for instance that the insignificance of $"FirstGoalTeam"$ may be due to tactical reasons, where after a goal a team may become more aggressive and both teams will find it easier to score further goals.
The baseline cumulative hazard function $hat(H)_0(t)$ seems linear, implying an exponential distribution for goal time.
A similar model is fit to the same data for the away team, so censoring works the opposite way.
Some conclusions drawn from the away model include evidence toward away teams playing more conservatively, likely due to the away disadvantage that we discuss later.

== Weibull Models
//Statistical techniques for predicting the outcome of football matches are numerous.
//Different authors attempt to model this in different ways.
A fairly natural way to model the number of goals scored in a match is as a Poisson process.
Some authors @dixonrob model the goals of each team as separate Poisson processes, others @adamleuven model the total goals scored as Poisson, assigning each goal to a team according to a binomial distribution.
// KHARRAT BOSHNAKOV
An oft-cited 2019 paper by #cite(<kharratboshnakov2019>, form: "prose") highlights an issue with the Poisson approach - it begins by stating that: 
#linebreak()
#quote()[
  #set text(9pt)
`... the main issue with the Poisson model when modelling the goals scored by a team in football is that the hazard function [...] remains constant for every time unit [...] However, empirical studies showed that this is rather questionable...`
]
#linebreak()
Kharrat and Boshnakov explore an alternate class of models, based on the Weibull distribution.
The Weibull distribution is a generalisation of the exponential distribution, as it adds a shape parameter $k$ that allows for a variable hazard rate.
Modelling the incidence rate of goals scored as following a Weibull distribution leads to a Weibull-count model, which generalises the Poisson model.
When $k=1$ the two models are equivalent.
Using the R statistical programming language they fit two seperate models to a publically available set of football data.
The first is a general linear model with goals scored based on the Poisson distribution, and the second is a renewal-count model based on the Weibull distribution.
In a visual analysis of the actual goal counts compared to the counts predicted by each model, the Weibull model appears to more closely match reality, this is later confirmed formally.
Due to the aforementioned property of these models that one nests the other, the authors can use a likelihood ratio test (LRT) to compare them.
They find that to a $p<0.001$ confidence level that the additional fit parameter introduced by the Weibull model does improve the fit, and conclude that the Poisson model is outclassed for modelling this set of data.
// Todo - Add more here ?

== Home Advantage
Home advantage (HA) has a major effect on match outcome in football, and in fact research @jamieson has shown that across 10 different sports the HA effect is significantly highest in football.
Various authors try to relate HA to the distance that athletes must travel, the pressure from crowds of local fans on the players and the referee, or other psychological effects on the players from being in an unfamiliar environment. 

#cite(<pollardarmatas>, form: "prose") discusses the factors that determine home advantage in World Cup qualifying matches.
The authors examine 2040 games played by national teams during the 2006, '10 and '14 seasons.
They analyse the effect of home advantage on qualification points. /*rather than on goal difference, as other authors do, since for the purposes of qualification the goal difference is irrelevant, only a win matters.*/
While the 'strength' of a team is not directly measurable, for the purpose of quantifying it the authors use FIFA rankings.
/*This should let them control for its effects. */
/*Other variables examined include details about the home team's stadium such as attendance and the distance of the crowd from play; the distance the away team had to travel in terms of time-zones crossed, absolute distance, and altitude change; possible referee bias in terms of red/yellow cards and penalties awarded to each team; and the continental region in which the teams competed.*/

Other variables examined include details about match attendance, distance of the crowd from play, number of time zones the away team crossed to attend, altitude change, referee bias and the continental region in which the teams competed.
Variables spanning multiple orders of magnitude underwent log-transformations, and multicollinearity between the large number of variables was checked and found to be negligible.
The authors fit a generalised linear model (GLM) for qualification points against these data. 
//The data for attendance and distance travelled both span multiple orders of magnitude so these variables underwent a log-transformation.
//Multicollinearity between variables may have been an issue, since for instance distance-travelled and time-zones-crossed are obviously related, however the authors found that pairwise correlation coefficients were low ($<0.2$) for any pair of variables.
This GLM had $R^2 = 0.326$, suggesting that the chosen variables are significant predictors of game points.
The most significant variable was found to be the team's ranking, a representation of its strength.
Home advantage was found to be significantly higher in Africa than in Europe or Asia, but the differences between other continental areas were found to be insignificant.
The authors also find that altitude change and time zones crossed for the away team are both significant effects, as well as the number of attendees.
Their results indicate that neither attendance as a percentage of capacity nor distance travelled by the away team had a significant effect ($p=90%, 6.7%$ respectively).
This last result was perhaps surprising as other authors @normanclarke have found that distance travelled is a significant factor.