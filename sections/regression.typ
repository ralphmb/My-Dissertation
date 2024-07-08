#show table: set text(8pt)
#show table: set align(center)

== Modelling match outcome

In this section we hope to explore the use of logistic regression in modelling match outcomes in football.
Bernoulli-distributed processes may have the 'success' chance $p$ be dependent on different variables $X$.
Here we have $p_i in (0,1)$ the probability of success for the $i$th subject, with $(p_i)/(1-p_i) in (0, infinity)$ the odds of success. Taking the logarithm of this quantity gives us the log-odds or 'logit' ($in (-infinity, infinity)$). The logit link function therefore allows us to use regression to model probabilities, as we can write the regression model as follows:
$ log (p_i)/(1-p_i) = beta_0 + beta_1 X_(i 1) + beta_2 X_(i 2) + ... $
Given a list of observed outcomes $in {0,1}$ and corresponding values for each variable, the coefficients $beta$ can be calculated using maximum-likelihood estimation. These coefficients have a nice interpretation in terms of the odds of success, as a unit increase in the $i"th"$ variable increases the odds of success by a factor of $e^(beta_i)$
An alternate way to express the model is to solve for $p_i$.
$ p_i = (1)/(1 + e^(-beta_0 - beta_1 X_(i 1) + ...)) $



As mentioned in the literature review, match outcome in football is three valued, but by grouping home losses with draws we can treat it as a binary response (hence in the R code this is called `result_bin`). Using logistic regression we can then predict the chance of a home win for given values of modelled variables, and examine the effect each variable has on the odds of winning.
First of all we will fit a full model, using most of the variables examined in the previous section. We will test whether red cards given to either side have an effect on the home team win chances. As the number of matches with any red cards is already fairly small we will use 1/0 valued variables denoting whether or not any were given to each side, rather than the number. Distance travelled by the away team will, as before, be grouped into a categorical variable based on whether it was above or below the median distance of 169km, or the distance between the stadia of Arsenal and Nottingham Forest. Distance could be used as a raw kilometre value, or undergo a log-transformation instead. Since it seems unlikely that a $1 arrow.r.bar 2$ increase in distance would have the same effect on win-odds as a $100 arrow.r,bar 101$ increase, we won't use the raw value, though the log-transformation could have more interesting results. The points each team scored in the previous (2021 - 2022) season will be included, as will a categorical variable denoting whether or not the match was a derby. A categorical variable denoting whether the match occured in the first or second half of the season will also be included.
We considered normalising the points variables. This would lead to more interpretable results when all covariates are set to zero but given the model would be equivalent as far as predictions, we decided against it.
#table(
  columns:4, 
[Variable],           [Estimate], [Std. Error], [p],
[(Intercept)],             [-1.527], [0.698], [0.029],
[Away team red card (yes:1)],            [0.276], [0.769], [0.720],
[Home team red card (yes:1)],           [-1.405], [0.683], [0.040],
[Distance grouping (far:1)], [0.404], [0.285], [0.160],
[Away points],     [-0.014], [0.008], [0.081],
[Home Points],               [0.033], [0.008], [0.000],
[Derby (yes:1)],      [-0.087], [0.429], [0.840],
[Season half (later:1)],              [0.370], [0.259], [0.150]
)
As can be seen from the p-value column, matches being derbies and red cards awarded to away teams seem to have little effect on match outcome. For derby matches this could be due to a correlation with the `distance_grouping` variable - rival teams are generally very geographically close to one another - raising the p-value. While red cards seem like an obvious boon to the other team, the sample size here might make the true effect size hard to uncover, as only 9 matches in this data set had any red cards given to the away team (18 to home teams).
Other variables have questionable significance, but there's an argument towards halving some of these p-values and treating them as one-tailed tests. The season half (`late_season`) seems like it could effect results either way, so a two-tailed test is appropriate for that variable. Red cards could be argued to be inherently good for the opposition so we will treat those as one-tailed, and the same for the 2021-2021 season league points variables and the closer/farther distance variable on the grounds that the away team would be less/more tired from the journey.

To build a more justified model, we will remove derbies and away-team red cards as variables. Though the distance grouping and season half have higher p-values, these will be kept. Season half might have some interesting interaction with other variables, and it may be worth checking whether distance becomes more relevant in the absence of the related derby variable.
A call to `glm` using the formula `result_bin ~ red_card_home + distance_grouping + opponent_points2021 + points2021 + late_season` gives a model with the following coefficients.
#table(
  columns: 4,
[Variable], [Estimate], [Std. Error], [p],
[(Intercept)], [-1.511], [0.696], [0.030],
[Home team red card (yes:1)], [-1.386], [0.682], [0.042],
[Distance grouping (far:1)], [0.437], [0.258], [0.091],
[Away points], [-0.014], [0.008], [0.073],
[Home points], [0.033], [0.008], [0.000],
[Season half (later:1)], [0.372], [0.258], [0.150]
)
The AIC value of this model is 356.66, down from 360.5. Of these coefficients, home team red cards and home team points are unquestionably significant. The distance variable has a 1-sided p-value of 0.0453, and the away team points of 0.0365, justifying their inclusion.
Season half still does not pass the 5% bar, and we fit a model containing interaction terms between each variable here and the season-half but none of the interactions are significant. /*#highlight("Should I show evidence of this? I fit a model with formula = result_bin ~ (red_card_home + distance_grouping + opponent_points2021 + points2021) * late_season. None of the interaction terms were significant, points2021 had the lowest p-value at 73% so I tried a single interaction term with that and the p-value only went up. Models with single interaction terms with season half were insig. too.")*/
In light of the irrelevant interactions we will try removing the season-half variable, which hasn't had any improvement in significance.

#table(
columns: 4,
[Variable], [Estimate], [Std. Error], [p],
[(Intercept)], [-1.322], [0.680], [0.052],
[Home team red card (yes:1)], [-1.322], [0.684], [0.053],
[Away points], [-0.014], [0.008], [0.069],
[Home points], [0.033], [0.008], [0.000],
[Season half (later:1)], [0.423], [0.257], [0.100],
)
The AIC of this model is 356.75, ever-so-slightly higher than the previous. We can calculate a McFadden $R^(2)$ for this model of $0.078$, down slightly from the values of 0.085, 0.084 for each of the previous two models. This $R^2$ is quite low, suggesting much of the variation in the data is yet unnaccounted for. A McFadden $R^(2)$ in the 0.2 to 0.4 range is considered an excellent fit @mcfadden. The significance of the distance grouping is right on the 5% one-sided borderline, but for the sake of a more interesting model we decide to keep it. Given all the other variables are significant we will proceed with this model. /*Since our goal isn't particularly to predict the results of matches (for which we might prefer a model that captures more information), but to quantify the effect that different variables have on match outcome, we will proceed with this third model.*/
The coefficient of home team cards is remarkably similar to the intercept term, -1.3221 vs -1.3218, but as far as we can tell this is just coincidence.


Taking $exp$ on each coefficient we can see the effects of each variable on the odds.
Red cards are associated with a reduction in the odds of a home win by a factor of $exp(-1.322) = 0.267$, which makes sense given the home team would be facing at a one-player disadvantage for some portion of the match. As a heuristic, we can check this value as follows. Home teams won 129 of 272 total matches, and of the 15 where they were given red cards they won 3. The odds of each event (win in general, win with red card) are thus 0.902 and 0.25, and we see that $exp(-1.322) times 0.902 = 0.24 approx 0.25$.

/*Away teams can be seen to suffer a greater disadvantage when they must travel farther to attend the match. The p-value is right on the borderline, but for the sake of a more interesting model we decide to keep it.*/

Each additional point the home team achieved in the previous season corresponds to a 1.03 times greater odds of winning, whereas each such point by the opponent multiplies the odds by 0.986. We can see confidence interval bounds on these odds ratios in the table below. The mean column gives point estimates for the odds multiplier $exp(beta_i)$ of each variable, and the lower and upper columns give 95% confidence interval bounds on these.
#table(
  columns: 4,
[Variable],      [Lower],  [Mean],  [Upper],
[Red Card Home],  [0.070], [0.267], [1.011],
[Away Points],    [0.970], [0.986], [1.001],
[Home Points],    [1.017], [1.034], [1.050],
[Distance],       [0.922], [1.526], [2.526]
)

The small number of matches with red cards given out leads to the bounds on this multiplier being quite wide. The 5.2% two-sided p value given earlier is seen here as the uppermost bound ($z = 1.96$)on this quantity only just exceeds 1. The bounds in general are quite wide. In the case of red cards this is likely a consequence of the low sample size. For the other variables this could be because of a slight lack in predictive power. Points as a proxy for strength isn't a perfect system, as a team's performance will naturally change between and through seasons, and also because the points aren't solely determined by strength, for instance they can be deducted when teams break rules.

As we have 4 variables it's difficult to plot the probability of a home win, however assuming no red cards are given we can provide two contour plots of $Pr("Home Win")$ against the points of each team.
In this first plot we set the distance grouping variable to 0, representing a closer-located away team.
In @contourcloser we set the distance grouping variable to 0, representing a closer-located away team, and in @contourfarther we set the variable to 1.
The bounds on each axis correspond to just either side of the minimum and maximum points values attained in the previous league season, 38 and 93. Three solid lines have also been plotted, each a level curve at which a match has a home win probability at 25, 50, 75%. The dotted line is $x=y$, where the 'strength' of each team is equal.
#figure(
  image("../assets/contour_plot_closer.png", fit: "contain"),
) <contourcloser>

#figure(
  image("../assets/contour_plot_farther.png", fit: "contain"),
) <contourfarther>

The larger coefficient of home team points vs away team points is visible here as the steeper slope of the level curves compared to $x=y$. This corresponds to stronger home teams expecting a higher chance of victory even when playing against equal opposition, as compared to weaker home teams. 
We can see the effect that location has as well, with 25% home win chance line being almost invisible when the home team plays more distant opposition.


Home advantage is tricky to quantify using this model due to the choice of response variable, which groups the neutral outcome of a tie with the negative outcome of loss. To make this somewhat easier to reason about we can try to fit a different class of model, one that can be fit against all three possible outcomes.


== Ordered logistic regression models

Ordered logistic regression models (OLRMs) are a way to generalise logistic regression, and they can be used to model situations with $>2$ ordered outcomes.
OLRMs have quite a similar setup to regular logistic regression. Binary logistic regression can be interpreted as a so-called latent variable model, whereby we define an unobserved variable $Y^(*)$ corresponding to the observed outcome $Y$ as follows. Let $Y^(*) = beta_0 + beta_1 X_1 + beta_2 X_2 + ... + epsilon$, with $epsilon$ a standard logistic error term and $X_i$ the $i"th"$ variable. Coefficients $beta$ are estimated such that $Y^(*)$ is positive when the observed outcome $Y=1$, and negative otherwise. $Y^(*)$ can be seen as a continuous version of the discrete outcome, on the log-odds scale.
We can suggestively rewrite the latent variable formulation of binary logistic regression as 
$ Y = 1 arrow.r.double Y^(*)- beta_0 = sum_(i) beta_i X_i > -beta_0 $
The quantity $Y^(*) - beta_0$ therefore corresponds to different observed outcomes based on how it compares to $-beta_0$, which acts as a threshold value.\
In ordered logistic regression, a similar process is followed. Instead of observing outcomes $Y in {0,1}$, we observe a number of ordered outcomes, labelled ${1,..., k,... ,N}$, and find coefficients $beta_i, mu_k$ such that $mu_(k-1) <= Y^(*) = sum_(i) beta_(i) X_(i) <= mu_(k)$ corresponds to the observed outcome $Y = k$. The values $mu_k$ therefore take the place of $beta_0$ as threshold values marking different outcomes. The odds and probabilities of each outcome can be calculated using the following formula, reminiscent of $Y^(*)-beta_0$ in the previous paragraph:
$
"logit"(Pr(Y<=k)) = mu_k -  Y^(*) $\

#table(
  columns:4,
  table.cell([Coefficients:], align: center, colspan:4 ),
[Variable], [Value], [Std. Error], [t value],
[Home red card], [-0.915], [0.484], [-1.892],
[Home Points], [0.027], [0.008], [3.548],
[Away Points], [-0.006], [0.007], [-0.813],
[Distance group], [0.266], [0.232], [1.147],
table.cell([Intercepts:], align: center, colspan: 4),
[Boundary], [Value], [Std. Error], [t value],
[Loss|Draw], [0.068], [0.627], [0.108],
[Draw|Win], [1.405], [0.632], [2.223]
) // olog_model_2 in code

Here we will try once more to fit a model to match outcome in football, this time respecting all three possible outcomes.
We can fit an OLRM in R using the `polr` function in the `MASS` package. For the sake of comparison we'll use a model fit against the same variables as the previous. Coefficients can be found in the table above.
The low t-values given clearly hint towards a low-confidence in these estimated values, a few of which are lower in magnitude than their standard errors. Nonetheless we can see the outcome this model would predict for a hypothetical game. For a game between Chelsea (74 pts, home) and Arsenal (69 pts) with no red cards we would see
$ hat(Y^(*)) = sum beta_i X_i = (&0.027 * 74 \
                    &-0.006 * 69 \
                    &+ 0.266 * 0 \
                    &- 0.915 * 0) = 1.677 $
This value 1.677 is greater than 1.405, hence the match is most likely a home win. We can also look at the probabilities predicted for this game to fall into each outcome. Given the boundaries $mu_k$ demarcating each outcome are  $-infinity, 0.068, 1.405, infinity$, we can extract each probability as:
$ Pr(Y = k) &= Pr(Y <= k) - Pr(Y <= k-1)\
            &="invlogit"(mu_k - hat(Y^(*)))\
                  &- "invlogit"(mu_(k-1) -hat(Y^(*))) $
//https://www.stata.com/manuals/rologit.pdf
Where $"invlogit"$ is the inverse logit function, $x arrow.bar (1+exp(-x))^(-1)$.
Thus the probability of a home loss would be $"invlogit"(0.068 -1.677) - "invlogit"(-infinity - 1.677) = 0.17$ (taking limits where needed), similarly $p=0.27$ for a draw and 0.56 for a home win.


In order for this class of models to be appropriate, the data must satisfy the proportional odds assumption.
This assumption mandates that the odds ratio between outcomes "$k_1$ or less" and "$k_2$ or less" must remain constant over different values of the independent variables.
There are a few suggested practical methods for assessing whether this assumption holds over a data set.
An online search suggests either graphical methods via plotting cumulative probability against each predictor @uclalogit, performing a likelihood ratio test between the OLRM and a multinomial regression fit using the same variables, or the Brant test @brant, implemented in R in the `brant` package. \
Multinomial logistic regression models are similar to OLRMs. In essence, seperate logistic regression models are fitted for each outcome, the results of each model giving the odds of a given subject falling into each outcomes. This allows for modelling outcomes that aren't ordered, at the cost of estimating multiple coefficients for each variable. The likelihood ratio test then looks at whether this model fits the data sufficiently better. The Brant test takes this same approach, but performs seperate tests on each variable.
/*https://stackoverflow.com/questions/37016215/testing-the-proportional-odds-assumption-in-r*/

We can perform the LRT fairly easily. The test has null hypothesis that the odds are indeed proportional. We find deviance values of 553.04 and 537.47, with degrees of freedom 12 and 20 for the OLRM and multinomial models respectively. This corresponds to a $chi^(2)$ test statistic of 15.55 on 8 d.f., higher than the critical value of $chi^(2)_(0.05) = 15.51$, hence we should assume that our data do not satisfy the proportional odds assumption.
The Brant test confirms this result.
#table(
  columns: 4,
[Test for], [X2], [df], [p],
[Omnibus], [17.040], [4], [0.000],
[Home red cards (yes:1)], [0.600], [1], [0.440],
[Home points], [7.240], [1], [0.010],
[Away points], [5.620], [1], [0.020],
[Distance grouping (far:1)], [2.680], [1], [0.100]
)
The null hypothesis is again that the odds are proportional, and we can see the offending variables are the points of either team. We can try fitting a new model, without the raw points values and instead using the above/below median grouping by points variable that we saw in the exploratory analysis section.
#table(
  columns:4,
[Test for], [X2], [df], [p],
[Omnibus], [6.030], [4], [0.200],
[Home red card (yes:1)], [0.730], [1],[0.39],
[Home point grouping (lower:1)], [1.010], [1],[0.31],
[Away point grouping (lower:1)], [1.69],[1],[0.19],
[Distance grouping (far:1)], [2.29],[1], [0.13],
)
And we see that the factor model can satisfy the assumptions required for OLR. This model ignores a lot of information as a result of disregarding the more granular data in the raw points values, however it meeting the assumptions does raise our confidence in the results of the model.

== Forecast Performance


We can test the forecast performance of all the models seen in this section. To avoid testing on the same data the models were trained on, we split our current dataset in two, 20% into the testing portion, assigned at random. New models with the same specifications were trained on the remaining 80% of the data. The coefficients in these testing models are fairly similar to the previous so we won't go into detail on them, though for obvious reasons the errors are wider.

This means the relevance of forecasting results here is murky. If we wished to simulate actual predictions, say predicting future matches in the league, we could have split the matches at a particular date and time. Though the significance of season time was shown earlier to be low, we thought it might bias the results to some degree. We could also have used the previous models to predict matches in the following league season. Between the change in teams due to promotion/relegation, as well as the even-more out of date points values, we decide to stick to the 2021-2022 season.

Out of 62 matches in the testing set, we see 26 wins, 20 ties and 16 home losses. In this sample tied results are very overrepresented, in general being rarer than losses, but the win:loss ratio remains similar, 1.63 vs 1.65 in the season overall.\
For the binary logistic regression model we see the following results.
#table(
  columns:3,
  [Pred/Actual], [Home Win],[Not],
  [Home Win],         [15], [11],
  [Not],         [11], [25]
)
So 40 out of 62 matches are correctly predicted. The model seems to err very symmetrically, though the tie-bias in the data is likely counteracting the bias inherent in the model. Below is the table for the ordinal logistic regression model that failed the proportional odds test earlier, included for comparison. 
#table(
  columns:4,
  [Pred/Actual], [Win],[Tie],[Loss],
  [Win],         [25], [18], [13],
  [Tie],         [0], [2], [1],
  [Loss],        [1], [0], [2]
)
And for the categorical (factor) model that passes the Brant test, we see the following predictions.
  #table(
  columns:4,
  [Pred/Actual], [Win],[Tie],[Loss],
  [Win],         [25], [19], [15],
  [Tie],         [0], [0], [0],
  [Loss],        [1], [1], [1]
)

We can see that both OLRMs are heavily skewed towards predicting home wins. Given the similarity of results we probably can't attribute this to the difference in variable choices, continuous or categorical descriptions of points. The large standard errors we saw earlier on the $mu_k$ threshold values are probably to blame. While it would lack any theoretical justification we could probably see more accurate results by increasing the values of both thresholds (between loss and tie, tie and win) as well as the distance between them.

A better solution might involve an entirely different quantification of "strength". Restricting ourselves to data also from the previous season, perhaps the team's ranking may be better, or strength could be calculated using the $alpha$ values estimated in a Bradley-Terry like model trained on last-season performance. 
Of course, incorporating bookmaker's odds or even just results from previous matches in the season would likely yield much more capable models, so if we wanted more positive results those are the directions in which we'd be most likely to look.