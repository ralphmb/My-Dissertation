// BRADLEY TERRY MODEL - TIES ALLOWED NO HOME ADV. 
// Cut because allowing ties requires a home advantage correction.


//With the extra parameter estimated being $nu$. We will try to fit a model to the first //specification.
//This model is a bit more complicated to fit than the previous model.
//`BTm` doesn't provide any support for tied results, but a search turned up the `VGAM` package, //which contains `brat` and related functions.
//We first create two matrices, `win_mat` and `ties`.
//In the former we set the $i,j^("th")$ element equal to the number of wins $i$ has had over $j$, and //in the latter to the number of times each team has tied the other.
//We can then fit the model using:
//```R
//ties_fit <- vglm(Brat(win_mat, ties) ~ 1,
//         bratt(refgp = 1, i0 = 0.1),
//         trace = TRUE,
//         crit = "coef")
//```
//This gives the following strength parameter estimates for each team:
//#table(
//  columns:2,
//[AFC Bournemouth], [1.000],
//[Arsenal], [6.840],
//[Aston Villa], [2.487],
//[Brentford], [2.221],
//[Brighton & HA], [2.549],
//[Chelsea], [1.116],
//[Crystal Palace], [1.123],
//[Everton], [0.744],
//[Fulham], [1.774],
//[Leeds United], [0.586],
//[Leicester City], [0.776],
//[Liverpool], [3.173],
//[Manchester City], [8.733],
//[Manchester United], [4.550],
//[Newcastle United], [3.714],
//[Nottingham Forest], [0.853],
//[Southampton], [0.446],
//[Tottenham Hotspur], [2.451],
//[West Ham United], [1.029],
//[Wolverhampton Wanderers], [1.059]
//)
//And an $alpha_0$ tie parameter of $0.511$. These parameters aren't directly comparable to those //from the tie-less model, but the higher and lower scores do correlate between them. 
//We can estimate the probabilities of win, tie, and draw between two teams as follows. Taking //Bournemouth, Manchester United as examples:
//$
//Pr("Bourn. win") &= (1)/(1 + 4.55 + 0.511) = 0.165 \
//Pr("Tie") &= (0.511)/(1 + 4.55 + 0.511)  = 0.084\
//Pr("Man U. win") &= (4.55)/(1 + 4.55 + 0.511) = 0.751 \
//$


```R
glm(data = valid_matches,
    family = "binomial",
    formula = result_bin ~ away_distance_km + opponent_points2021 + points2021)
```
Where `result_bin` is the binary win vs. not-win version of the match results, and `valid_matches` is a subset of the data set, excluded promoted teams. R gives the following summary for this model. For formatting and conciseness, any R output in this document will have less-relevant data removed and unnecessarily precise estimates rounded to an appropriate number of dec imal places.

```R
                   Estimate
(Intercept)          -0.9252544
away_distance_km      0.0005193
opponent_points2021  -0.0174105
points2021            0.0310194
```

For an example of interpreting these coefficients: each extra point attained by the home team last season leads to an expected $exp(0.0310194)$ times greater log-odds of winning a given match. R performs significance tests against the null hypotheses of each of these coefficients being 0. The p-values for each variable respectively are 0.203, 0.647, 0.034, 0.0002. The first two of these are quite a bit higher than the usual benchmark for significance of 0.05, hence we cannot conclude they are significantly different from zero. We will therefore drop both variables. \
Since it had the higher p-value we first dropped away team distance and tested the new model, however the intercept term was still insignificantly different from zero. This leaves us with quite a simple model, with only two parameters estimated. The new model fit by R using `formula = result_bin ~ 0 + points + opponent_points` has the following summary:

#figure(
  table(
    columns:5,
[Variable],        [Coeff.], [S.E],   [z], [p],
[opponent_points2021], [-0.024],   [0.006], [-4.090],  [0.000],
[points2021],          [0.024],    [0.006], [4.043],   [0.000], 
  )
)
Which is very symmetric. Point estimates for the coefficients, -0.02446 and 0.02393, do differ by about 2% (in magnitude), but this difference is about one tenth of standard error for each. The similarity suggests rephrasing this model in terms of point-difference, as we practically have one degree of freedom anyway. In light of this and thinking back to the overall home-team win rate of 49.2% in the previous section, the insignificance of the intercept term makes some sense. Two fairly evenly matched teams with $Delta"points" approx 0$ will be predicted log-odds of 0, corresponding to a home win rate of 50%, so the average lies in this neighbourhood too.\

Treating this as a one variable model, in @diffrate we plot the predicted probabilities for a given point-difference. The point difference for Leeds United and Manchester City is the largest at $plus.minus 55$, where the model predicts a 23% and 77% win chance for each team playing the other at home. \
#figure(
  image("../assets/logis_diff_winrate.png", fit: "contain"),
  caption: [Expected home win rate by point difference]
) <diffrate>

Using a cutoff of 0.5 for classifying wins vs losses, we can make a table of predicted results vs actual for every non-red-carded match in the season.
#table(
  columns:4,
  [],table.cell([Actual], colspan: 3, align:center),
  table.cell(rotate([Pred], -90deg), rowspan:3, align: horizon, inset: (x:2pt, y:5pt)),
  
  [],        [Win], [Not],
  [Win],     [74],    [53],
  [Not],     [48],    [75]
)


This gives it a true positive rate of 58%, so better than random chance. The obvious caveat with predicting outcomes like this is that the logistic model is redundant here. Simply predicting a home win if the point-difference is positive would yield the same results. As well as this, the model fails to particularly accurately model match outcome even when predicting based on the data from which it was trained. \

In the interest of making more interesting and useful models, we will expand the scope of this section, and explore a related technique.




// ________________________________________________________________________________________________________________________
// Shit models, parametric survival
// ________________________________________________________________________________________________________________________

In the above univariate analyses, we saw that home/away classification, grouping of team by last-season points, and distance travelled (for away sides) were shown significant predictors. Additionally, the grouping of the opponent had limited evidence suggesting its usefulness. As we want to include data for home sides, we will for now ignore distance-travelled. We will first fit an exponential distribution based survival model to the unambiguously useful variables, as well as a Weibull model. We will then discuss the suitability of each. If either appears useful, we can investigate using the opponent's grouping, and including distance-travelled again. \
The almost-linearity of the cumulative hazard curve in @cumhaz could suggest an exponential distribution is a good fit. //Delete this line?

We can fit parametric models in R using `survreg(formula, dist = "[...]")`. Our formula will be of the form
```R
surv ~ home_or_away + team_grouping
```
where `surv` is a `survival` object encoding data for times and censoring, and the right hand side gives the covariates. Our two models will have `exponential` and `weibull` as values for `dist`.
Basic information about the models fit can be seen in the table. The exponential fit given by R has an intercept term of 4.529, and coefficients -0.299, 0.277 for factor variables `home_or_away` and `team_grouping` respectively. For the Weibull model the scale parameter was estimated at 0.888, and the coefficients at 4.501, -0.278, -0.260 respectively. All variables in both models are significant to $p<0.01$

// @@@@@@ Rewrite this bit? @@@@@@

Given both models can be written as
$ ln(mu_i)= beta_0  + beta_1"home_or_away" + beta_2"team_grouping" $
We can make a table of values for $mu$ based on each set of coefficients.
#figure(
table(
  columns:3,
  text(size:10pt,[Exp. | Wei. ]), text(size:9pt,[Home]),      text(size:9pt,[Away]),
  text(size:9pt,[Higher]),       [52.1|52.6], [68.8|68.23],
  text(size:9pt,[Lower]),        [70.3|69.5], [92.7|90.1]
),
caption: [Values of $mu$ for each model]
) <twomodels>
@twomodels shows that the results are quite similar between models.
#highlight([Does this make sense? It feels a bit unclear to me])

// @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

We can see the similarity between the models quite clearly when comparing @survpredexp and @survpredwei.
#figure(
  image("../assets/surv_pred_exp.png", fit: "contain"),
  caption: [KM survival curves and the fitted exp. survival curves]
) <survpredexp>
The above plot shows in solid colours the Kaplan Meier estimates of survival function for each of the four treatment groups, and in dotted colours the exponential regression fit for each.
#figure(
  image("../assets/surv_pred_wei.png", fit: "contain"),
  caption: [KM survival curves and their Weibull fits]
) <survpredwei>
Both models seem to fit quite well, with the Weibull appearing to track the KM curves more closely near the earlier minutes of the game. The models have log-likelihoods of -2686.9 and -2682.5 for exponential and Weibull respectively, giving a LRT statistic of $lambda = 8.8$, exceeding $chi^(2)_(0.01) = 6.635 space ("df" = 1)$, hence we can favour the Weibull model as fitting the data significantly better. Note that since the models are nested (exponential models constrain $gamma = 1$) the likelihood ratio test is appropriate. A comparison of the AIC values for each also supports this, giving the exponential model 5379.8 and the Weibull 5372.9.


// Old full Models - parametric survival

#table(
  columns: 4,
  text(size:8pt,[Variable]), text(size:8pt,[exp(est)]), text(size:8pt,[2.5%]), text(size:8pt,[97.5%]),
  text(size:8pt,[Home (vs away)]), text(size:8pt,[1.182]), text(size:8pt,[0.909]), text(size:8pt,[1.537]),
  text(size:8pt,[High group (vs low)]), text(size:8pt,[1.319]), text(size:8pt,[1.107]), text(size:8pt,[1.572]), 
  text(size:8pt,[Opponent high group (vs low)]), text(size:8pt,[0.831]), text(size:8pt,[0.695]), text(size:8pt,[0.995]),
  text(size:8pt,[Distance]), text(size:8pt,[0.999]), text(size:8pt,[0.998]), text(size:8pt,[1.000]),
)

#table(
  columns: 4,
  text(size:8pt,text(size:8pt,[Variable])), text(size:8pt,[exp(est)]), text(size:8pt,[2.5%]), text(size:8pt,[97.5%]),
  text(size:8pt,[Home (vs away)]), text(size:8pt,[1.195]), text(size:8pt,[0.881]), text(size:8pt,[1.620]),
  text(size:8pt,[Points '21]), text(size:8pt,[1.010]), text(size:8pt,[1.004]), text(size:8pt,[1.016]), 
  text(size:8pt,[Opponent points '21]), text(size:8pt,[0.992]), text(size:8pt,[0.986]), text(size:8pt,[0.998]),
  text(size:8pt,[Distance]), text(size:8pt,[0.999]), text(size:8pt,[0.998]), text(size:8pt,[1.000]),
)


//#figure(
//  image("../assets/unused/factor_mod_one.png", fit: "contain"),
//) 
//#figure(
//  image("../assets/unused/ctns_mod_one.png", fit: "contain"),
//) 
/*#highlight([I can write these as tables instead of using screenshots, if that makes a difference. The reason for the screenshots was so I could fit more content here without changing the text size.])*/




// Rest of the deleted weibull stuff.


/*As can be seen from the coefficients in each model, playing at home and being higher ranked both correspond to shorter first goal times, and playing a higher ranked team, having to travel farther lead to longer times. We can see however that the confidence intervals are quite wide for both team side and distance travelled in each model, as both include 1. Corresponding p-values can be found for each of these variables, 21.6% and 25.3% for `home_or_away` and 14.8% and 19.8% for `actual_distance` in each respective model. This is likely due to the correlation between these variables. To remedy this we can remove the variable with highest p-value, in both cases the home/away factor.\*/
After removing this factor variable we get models with the following coefficients.

//TODO : Remake this table, with home and away as the variable chosen

#table(
  columns: 4, 
  [Variable],         [exp(est)], [L95%],  [U95%],
  [home_or_awayHome],    [1.394], [1.135], [1.713],
  [points2021], [1.010], [1.004], [1.016],
  [opponent_points2021], [0.992], [0.986], [0.999],
)
With shape = 1.14 and scale = 0.00572
#table(
  columns: 4,
  text(size:8pt,[Variable]), text(size:8pt,[exp(est)]), text(size:8pt,[2.5%]), text(size:8pt,[97.5%]),
  text(size:8pt,[High group (vs low)]), text(size:8pt,[1.314]), text(size:8pt,[1.102]), text(size:8pt,[1.566]), 
  text(size:8pt,[High grouped opponent (vs low)]), text(size:8pt,[0.826]), text(size:8pt,[0.691]), text(size:8pt,[0.988]),
  text(size:8pt,[Distance]), text(size:8pt,[0.999]), text(size:8pt,[0.998]), text(size:8pt,[0.999]),
)
With scale parameter 0.008979 and shape parameter 1.128343
#table(
  columns: 4,
  text(size:8pt,[Variable]), text(size:8pt,[exp(est)]), text(size:8pt,[2.5%]), text(size:8pt,[97.5%]),
  text(size:8pt,[Points '21]), text(size:8pt,[1.010]), text(size:8pt,[1.004]), text(size:8pt,[1.016]), 
  text(size:8pt,[Opponent points '21]), text(size:8pt,[0.992]), text(size:8pt,[0.985]), text(size:8pt,[0.998]),
  text(size:8pt,[Distance]), text(size:8pt,[0.999]), text(size:8pt,[0.998]), text(size:8pt,[0.999]),
)
With scale parameter 0.007999 and shape parameter 1.142912

As the 95% interval bounds show, all covariates are significant so we need not remove any further variables. Inconveniently, due to the lack of points-values for the teams promoted from a lower league, the continuous model is derived from much less data than the factor model, with 500 observations instead of 706. This means we can't compare AIC or likelihood values for each model to assess goodness-of-fit. The values estimated in each model are similar, with the Weibull shape parameter $gamma = 1.13, 1.14$ for the factor and continuous models respectively.
Given that the inclusion of promoted teams into the 'lower' grouping is to some degree a guess on where they would most likely be grouped, we will proceed with the model based on continuous point-values. \
The value of the shape parameter, $gamma = 1.14$, indicates a growing hazard throughout the game. This is perhaps a bit surprising, given the concave look of a few of the CH curves shown earlier.
/*The intercept term gives something like a baseline survival time /* CHECK THIS */ - $exp(4.22) = 68.36$.*/
This doesn't exactly correspond to the average, just the value when all covariates are 0. We can use this, and the rest of the fitted coefficients to calculate expected survival times given some values for each coefficient. 
Take a game between Arsenal (H) and Everton (A). These teams achieved 69 and 39 points last season respectively, and their stadia are 286km apart.
The TTFG expected for Everton in this match would be predicted as 109 minutes, and for Arsenal 47 mins, which makes sense, given they're the stronger team and playing at home.

#highlight([Thought about adding residual plots here, but maybe they aren't needed.])

// Residual Plots? - Housh says unnecessary
