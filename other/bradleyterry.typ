#heading([Bradley-Terry models], numbering:none, outlined:false)

/* CHECK OUT KUK 1995 @kuk1995 */
In this section we will take a new approach to the problem of modelling football match results. 
A paper by #cite(<koning>, form:"prose") uses an ordered probit model to assess team strengths and home advantage. 
Using $D^(*)$ as a latent variable for match result, the author fits a model as $D^(*) = alpha_(i) - alpha_(j) + h_(i j) + nu_(i j)$, where $alpha_(k)$ is a term representing the relative strength of team $k$, $h_(i j)$ represents a home advantage factor when $i$ plays at home against $j$, and $nu_(i j)$ are normal errors. The model is thus treating the outcome of a game as being related to the difference in "strength" between the two teams, with some extra detail through the error and home advantage terms. 


This is very similar to a Bradley-Terry model (BT model), and in the following section we will see if these are useful for modelling match outcomes for our data. The BT model adapts logistic regression to treat the response variable as a difference of "strength" values: $"logit"(Pr(i>j)) = alpha_i - alpha_j$
A more common use of regression in sports involves taking information about the strengths of each and using it to predict match outcome, whereas BT models use data on match outcome to assign strengths to teams.
/*Here we will investigate whether the models are useful, and if they are, train them on a subset of the data and try to predict the rest*/
In addition to sports modelling, these models are often used for ranking items based on a series of pairwise comparisons. /*#highlight([Reference here?])*/
/* references needed here? */
The usual BT model isn't equipped to deal with ties in the data so as in the logistic regression section, the outcomes here will be 'home win' and 'not home win'.
An advantage of these models over the previous approach is that we aren't using any predictive variables, only match results against dummy variables for the teams, hence the low information content of our data won't hamper model performance. 
We can fit a basic model using `BTm()` in the `BradleyTerry2` package in R.
For this section we are including every match result, including games involving promoted teams and matches with red cards. 
```R
bt_mod <- BTm(outcome = result_bin,
          player1 = factor(match_unique$team),
          player2 = factor(match_unique$opponent))
summary(bt_mod)
```
Which finds the following list of team specific "strengths".
#table(
columns:2, 
table.cell([Coefficients:], align: center, colspan: 2),
[Arsenal],                    [1.964],
[Aston Villa],                [1.173],
[Brentford],                  [1.054],
[Brighton & HA],     [1.054],
[Chelsea],                    [0.242],
[Crystal Palace],          [0.359],
[Everton],                    [0.476],
[Fulham],                    [ 0.476],
[Leeds United],          [ -0.392],
[Leicester City],         [-0.126],
[Liverpool],                  [1.294],
[Manchester City],         [2.469],
[Manchester United],       [1.546],
[Newcastle United],        [1.679],
[Nottingham Forest],       [0.122],
[Southampton],              [-0.687],
[Tottenham Hotspur],       [1.173],
[West Ham United],      [0.122],
[Wolverhampton Wanderers], [0.359],

)


Notice the absence of AFC Bournemouth among the teams listed, they are treated as the reference team with strength 0.
The values given seem to make some sense, with stronger teams such as Manchester City and Liverpool having higher coefficients and weaker teams such as Leeds having negative ones.
All of the coefficients here have a standard error in the range $(0.49, 0.58)$. This is of course quite high, given many of the teams have coefficients of a magnitude lower than this. The high variance in these estimates is because each pairwise matchup occurs only twice, each team plays every other at home and away. Including multiple seasons worth of data may assuage this somewhat, as the results of more matchups could be included, however as teams move between leagues and their prowess changes over time this would add complications.

We can use the results of this fit to find predicted win chances for different teams. Taking Newcastle United (H) and Leicester City as examples:
$ Pr("N.U. Win" ) = 1/(1+e^(-1.679 + 0.126 )) = 0.825 $
Due to the fact that home teams win about half of their games and the outcomes are being grouped as "home win" vs "not home win", there's an argument against including a home advantage parameter here: the data support a roughly even rate of home wins and away "wins".
Supposing teams $i,j$ play one another with $i$ at home, and $alpha_i - alpha_j = Delta alpha$ is small, then the basic BT model (without additional parameters) will predict 
$ Pr(i "wins") = (1 + exp( alpha_i - alpha_j))^(-1) approx 1/2 - (Delta alpha)/4 $
And indeed if we choose to fit one with a constant home advantage factor, R estimates $h = -0.08$  with s.e. $0.11$, which is very close to zero. An interaction between home advantage and team strength might make one useful however, and we see evidence of that in the exploratory analysis section.

#heading([Including Ties], numbering:none, outlined:false)
/*#highlight([Kuk 1995 - Modelling Paired Comparison Data has a discussion of these models in relation to football, worth adding a reference to this?])*/
As alluded to earlier, modifications can be made to the BT model such that ties can be included. A number of methods of accomplishing this have been suggested @bradtertiesfit.
These more general models often use a different parametrisation. Instead of estimating $alpha$ terms whose difference gives the odds, terms $p_i$ are estimated such that $sum_(i) p_(i) = 1$, and probabilities are calculated in different ways, shown below.
One method for generalisation proceeds by estimating a tie-chance parameter $p_0$, as well as team-strength parameters $p_1, ... , p_n$. 
The chances of each outcome are then written as:
$
Pr(i "beats" j) &= (p_(i))/(p_(i) + p_(j) + p_(0)) \
Pr(i "ties" j) &= (p_(0))/(p_(i) + p_(j) + p_(0)) 
$
Another option (the Davidson model @bradleyties) treats the probability of teams $i,j$ tying as proportional to the geometric mean of the parameters estimated for each team: 
$
Pr(i "beats" j) &= (p_(i))/(p_(i) + p_(j) + nu sqrt(p_(i)p_(j))) \
Pr(i "ties" j) &= (nu sqrt(p_(i)p_(j)))/(p_(i) + p_(j) + nu sqrt(p_(i)p_(j))) 
$
With $nu$ representing a 'tendency towards ties'.
The Davidson model can be extended to include a multiplicative home advantage parameter @fulldavidson. For team $i$ playing at home, team $j$ away:
$
Pr(i "beats" j) &= (gamma p_(i))/(gamma p_(i) + p_(j) + nu sqrt(p_(i)p_(j))) \
Pr(i "ties" j) &= (nu sqrt(p_(i)p_(j)))/(gamma p_(i) + p_(j) + nu sqrt(p_(i)p_(j))) \
Pr(j "beats" i) &= (p_(j))/(gamma p_(i) + p_(j) + nu sqrt(p_(i)p_(j))) \
$
A number of R packages exist for fitting models like these. `VGAM::brat` and related functions can fit ties-allowed models of the first kind.
The problem with a ties-allowed model like this for our case is that the home advantage effect ought to then be accounted for, as including including all three match outcomes removes the home/away symmetry.
After searching for a while we came across the `bpcs` package. While quite laborious to get working, this package allows one to fit ties-allowed models with a home advantage factor, along the lines of the third model type listed.

In the table below we list estimated strength parameters for each team with 95% highest posterior density bounds, as well as the estimates for the home advantage correction $gamma$ and the tie-factor $nu$. All values are given in $log(...)$ form, as this transforms them onto the linear scale which is used for estimation.
#table(
  columns:4,
  [Parameter], [Mean], [2.5%], [97.5%],
[Wolves], [-0.600], [-2.070], [0.840],
[Leeds], [-1.830], [-3.390], [-0.320],
[South'ton], [-2.320], [-3.940], [-0.780],
[Not'ham], [-0.970], [-2.490], [0.470],
[Chelsea], [-0.780], [-2.260], [0.690],
[Man City], [2.860], [1.220], [4.520],
[Liverpool], [0.990], [-0.510], [2.510],
[Crystal P.], [-0.570], [-2.040], [0.920],
[Brighton], [0.600], [-0.900], [2.100],
[Fulham], [-0.390], [-1.860], [1.070],
[West Ham], [-1.000], [-2.490], [0.490],
[Newcastle], [1.620], [0.100], [3.140],
[Everton], [-0.430], [-2.000], [0.970],
[Arsenal], [2.090], [0.450], [3.630],
[Tottenham], [0.770], [-0.690], [2.260],
[Brentford], [0.580], [-0.850], [2.150],
[Aston V.], [0.780], [-0.740], [2.270],
[Leicester], [-1.440], [-3.000], [0.080],
[Man U.], [1.440], [-0.050], [3.040],
[Bournemouth], [-1.200], [-2.720], [0.290],
[gm], [-3.760], [-4.750], [-2.800],
[nu], [0.080], [-0.130], [0.290]
)
As in the previous section the bounds on these values are quite wide, due to the number of matchups between any pair of teams being limited to 2. 
The constant effect $ln(gamma)$ is quite strongly negative, indicating an advantage towards the home team.
Due to how we fit the model this reads more clearly as any "away-disadvantage" term, as will be shown momentarily.
The tie-parameter $ln(nu)$ is very close to zero, and the 95% bounds on it do include zero.
This indicates that tie chance $prop nu sqrt(p_i p_j)$ is mostly determined by the respective team strengths.
Taking a pair of teams as an example we can calculate win/draw/loss probabilities.
As before using Leicester City playing away vs Newcastle United, we find team strength values of $gamma p_("LC") = exp(-3.76)exp(-1.44) = 0.0054, p_("NU") = exp(1.62) = 5.05$ respectively.
Note that we multiply the away team win chance by $gamma$.
The tie term is $exp(0.08) sqrt( 0.237 times 5.05) = 1.18$, so the denominator in each case will be $0.0054 + 0.237 + 5.05= 6.24$
$
Pr("Leic win") &= 0.0054 / 6.24 = 0.00087\
Pr("Tie") &= 1.18 / 6.24 = 0.19\
Pr("N.U. win") &= 5.05 / 6.24 = 0.81\
$
Despite the possibly-unrealistic small chance of Leicester pulling through, this roughly matches the win chance for Newcastle we found in the previous section, 0.825.
/*#highlight([Could add a team ranking based on the scores here, maybe compare it to the team ranking based on last-season points.])*/
/* Should I add a team ranking here based on these results? Compare to points-based team ranking etc. */
#heading([Model Performance], numbering:none, outlined:false)
In this section we will test the forecasting accuracy of these models. 
At the moment the models we've been looking at have been trained on the full 380 matches in the season, but for this section we will restrict this to the first 350 games, and test on the remaining 30. 
/*#highlight([Should I instead select the testing games from uniformly throughout the season? Makes it less applicable in actual-life betting scenarios but will remove any bias due to time-dependence in match results])*/
/*  Should I instead select the testing games from uniformly throughout the season? Makes it less applicable in actual-life betting scenarios but will remove any bias due to time-dependence in match results*/ This is of course a fairly small sample size for testing, but given the bounds on the parameter estimates are already very wide, there's likely a steep tradeoff between more testing precision and model accuracy.
We'll train both the regular BT model using `BTm` , as well as the constant-effect Davidson model using `bpc`.
The testing dataset includes 30 games, of which 13 were home wins, 5 were ties and 12 were away wins.
For the Bradley Terry model, we can tabulate the predicted vs actual results. Using a cutoff at $p = 0.5$:

#table(
  columns:4,
  [],table.cell([Actual], colspan: 3, align:center),
  table.cell(rotate([Pred], -90deg), rowspan:3, align: horizon, inset: (x:2pt, y:5pt)),
  [],        [Home Win], [Not],
  [Home Win],       [3],   [4],
  [Not],           [10],  [13]
)
We can see that the model is very keen to predict good results for the away team. Why exactly this happens we aren't too confident. The sample size is naturally very small, both for training and testing, so biased/innaccurate parameters are a possibility, as is a biased sample. Perhaps a different choice of cutoff could improve results, though as far as we're aware there's no theoretical justification for doing this.
We can do the same for the three outcome model:
#table(
  columns:5,
  [], table.cell([Actual], colspan: 4, align:center),
  table.cell(rotate([Pred], -90deg), rowspan:4, align: horizon, inset: (x:2pt, y:5pt)),
  [],        [Home Win], [Tie], [Away Win],
  [Home Win],       [6],   [1],        [5], 
  [Tie],            [7],   [4],        [7],
  [Away Win],       [0],   [0],        [0]
)     
As we can see, this model has the opposite behaviour. The low sample size makes it quite fraught to extrapolate from any results here, but the model seems fairly capable of predicting ties, assessing 4/5 correctly. Home wins are predicted either correctly or as ties, though the model has around even odds of choosing correctly.
Away wins are where the model seems to struggle most. No match was predicted as an away win, and the model barely prefers predicting actual away wins as ties.
In this case the culprit is likely the away-disadvantage constant $gamma$. As we saw just above when fitting the first Davidson model, the magnitude of $ln(gamma)$ is far higher than any of the other constants, hence the $gamma p_(j)$ terms in the numerator of away-team win chance are going to be overly small. \
Between the negative results in the logistic regression section and now here, we seem to be struggling to find a good model for the win and loss patterns in football. As mentioned in the ordered logistic regression section, some degree of this is likely due to to poor information content in our data, but the failure to find a good predictive model in this section might suggest finding a new approach entirely. \
Bradley-Terry based models with covariates included do exist and might be useful, depending on the choice of covariate. They would likely still run into the issue of sample size. If we wanted more useful results here, it might be wise to include data from previous leagues, perhaps weighted such that more recent matches have higher influence.