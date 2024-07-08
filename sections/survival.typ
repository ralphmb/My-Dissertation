#show table: set text(8pt)
#show table: set align(center)
== Interactions between variables and survival time
The time to first goal (TTFG) is a statistic of interest for both bookmakers and bettors. Many bookmakers offer the ability to bet on a timeframe in which the first goal will be scored, usually dividing the match into 10 or 15 minute intervals. Similar bets can be placed on whether a goal will be scored before/after certain points in the game, particularly around the number of goals scored in the first/second half.


In this section we hope to answer some questions about the TTFG in football matches. We will examine its dependence on various factors, such as comparing home teams vs away. The Kaplan Meier plot is a 
We won't investigate any continuous variables until later as these aren't amenable to Kaplan Meier fitting, hence all the over/under factor variables. Also of note is that the data set in this section is structured differently to the previous. In the logistic regression section we looked at data where each observation corresponded to a match, in this section each observation corresponds to the match from the perspective of each side, home and away. This makes model fitting easier since we have one goal time in each observation but we ought to be careful examining variables with side-dependent effects, as any effect that benefits home sides as much as it hurts away sides will have no effect overall.


@survplot shows a Kaplan-Meier survival curve for first-goal events across every match in the 2022/23 season, including data from both home and away teams, Notable in this plot and most of the following ones is the sharp drop at 45 minutes.
This happens because goals occuring in injury time at the end of the first half are counted as occuring in the 45th minute, thus raising the number of goals falling into this bin./* Nevo Ritov mentions this - they have the same*/ All highlighted regions represent 95% confidence intervals, based on the estimated standard error of survival chance.

#figure(
  image("../assets/surv.png", fit: "contain")
) <survplot>
A quarter of games have their first goals within 22 minutes. The median time for first goal is 49 minutes, slightly into the second half. The third quartile value is not reached, as 72.8% of teams end the game without scoring, just shy of three quarters. 

#figure(
  image("../assets/cumhaz.png", fit: "contain")
) <cumhaz>
@cumhaz shows a cumulative hazard plot (CH plot) for times to first goal, along with a dashed line showing a straight-line fit. Apart from the jump at 45 minutes, it is clear that the graph is very close to linear, a visual suggestion that the hazard is close to constant throughout the match.

=== Interaction with game side
As shown in @survhomeaway, TTFG is clearly different for home sides vs away. The median first goal times are 45 and 53 minutes for home and away sides respectively, with lower quartiles in the 21th and 23rd minutes. The upper quartile TTFG for home sides lies in the 84th minute, but among away sides only 67.1% manage to score by the end of the match.
#figure(
  image("../assets/surv_homeaway.png", fit: "contain"),
  caption: [Survival curves for each side]
) <survhomeaway>
To ascertain whether these subgroups are really different from one another, we can first examine their respective cumulative hazard plots.

@cumhazhomeaway shows CH plots for both home and away sides, as well as the difference in the logarithms of each observed CH function. The red and blue dashed lines show best fit lines for each. The black curve gives the log of the hazard ratio (HR) between the groups, or equivalently the log of the difference between the two cumulative hazards.
A constant log difference line implies the CH functions are proportional to one another.
An assumption underlying many models in survival analysis is that of proportional hazards, where the hazard (equivalent cumulative hazard) functions differ by a constant multiplicative factor.
Among other things that we'll see later, this assumption justifies the use of more powerful log-rank test versus the Wilcoxon-Gehan test for difference in hazard between the groups.
In order to calculate the log HR the CH for one treatment group has to be interpolated to match the number of observations seen in the other, but this should not change its shape. \

#figure(
  image("../assets/cumhaz_homeaway.png", fit: "contain"),
  caption: [Cumulative hazard plots for home and away sides, and their log difference.]
) <cumhazhomeaway>

The log HR appears fairly constant after around 30 minutes, but before this it varies quite a bit, even changing sign at one point. This could be due to away teams playing more aggressively in the opening of a match, to gain an early lead; the aforementioned authors Nevo and Ritov observed a similar effect in their paper. As we will see a similar effect is present for many variables, with different groups indistinguishable in the opening minutes of the match. Some portion of this effect is possibly numerical, any effect on play due to a change in some variable will have had less chance to accumulate near the start, hence the groups will be closer together and their ratio will be more effected by noise. 

For this reason we will assume for this and similar examples that the hazards are proportional, and use the log-rank test, implemented in R using `survdiff(..., rho = 0)`.
The test shows that these populations differ significantly, with $chi_("df"=1)^2 = 11.6, p=0.0005$.



=== Interaction with team strength
We can plot seperate KM curves for each team grouping, by whether they attained above or below median points in the previous season. As we saw in the logistic regression section, these variables in their raw numerical form were problematic for the proportional odds assumption of ordered logistic regression. 

#figure(
  image("../assets/surv_grouping.png", fit: "contain"),
  caption: [Survival curves by team grouping]
) <survgrouping>
The curves in @survgrouping are superficially similar to those in @survhomeaway. The quartiles for higher grouped teams are in the 19th, 43th and 76th minutes, for lower teams the 23rd, 54th and 90\<th minutes, with 31.1% of these lower grouped teams failing to score.

Much like with home vs away sides, the CH curves are fit quite well by the lines, and the log difference is fairly constant after 30 or so minutes. The arm for lower grouped teams appears somewhat concave through the second half of the match, though this a fairly minor deviation.
#figure(
  image("../assets/cumhaz_grouping.png", fit: "contain"),
  caption: [Cumulative hazard by team grouping]
) <cumhazgrouping>
For the same reasoning as before, we perform a log rank test, showing this split has real effects on TTFG, giving $chi_("df"=1)^2 = 11.9, p=0.0006$.



=== Interaction with other team's strength
Intuitively the strength of the other team may also affect the rate at which a team can score. As in the previous section we'll look at the binary grouping by last-season points here to determine stronger and weaker teams.
#figure(
  image("../assets/surv_oppgrouping.png", fit: "contain"),
  caption: [Survival curves by opponent's grouping]
) <survoppgrouping>
The survival curves in this example are much closer together than the previous, but the effect still seems visible. As with many of the other variables looked at here, the curves are much more clearly distinct towards the second half of the match. Unlike the others, this distinction appears later, at around the 45 minute mark. The quartile values of TTFG are at 21, 45 and 89 minutes for teams playing a weaker opposition, and 21, 55, 90+ minutes otherwise, with 30.9% of teams facing a tougher opponent failing to score by full time.\


The log difference line again appears constant nearer the end, as the hazard ratio appears to rise towards and settle at $approx e^(0.2)$. 
The relatively larger shifts in the observed hazard ratio mean we will test the difference here using the Wilcoxon-Gehan test, resulting in a chi-square value of 1.9 on 1 d.f., so $p = 0.2$. This is above the usual threshold but given the interaction between the strengths of either side, seen in previous sections, it may be worth investigation in a fuller model.
#figure(
  image("../assets/cumhaz_oppgrouping.png", fit: "contain"),
  caption: [Cumulative hazard by opponent's grouping]
) <cumhazoppgrouping>



=== Interaction with distance travelled
// Check this using Cox model as well - and explain the need for binary grouping here.
Restricting our attention to away sides, we fit survival curves to TTFG based on whether each away side travelled more or less than the median distance to attend a match. As mentioned at the beginning of this section we do focus on away sides as the effect of this variable presumably depends on the side. In the next section we will fit this as an interaction term.
#figure(
  image("../assets/surv_distance.png", fit: "contain"),
  caption: [Survival curves for away sides by distance travelled]
) <survdistance>
@survdistance shows the results, again quite clearly distinguishable. Likely due to the inherent disadvantage suffered by every team examined here, neither curve quite reaches below the 25% line by 90 minutes, with teams who travelled a longer distance failing to score 41.1% of the time and closer teams 24.7% of the time. The lower quartile for first goal time lands in the 19th and 27th minute for near and far teams respectively, with medians at 50 and 62 minutes. \

@cumhazdistance shows that the CH plots for this variable don't fit the lines quite as cleanly. Some measure of this is likely due to the smaller sample - we've excluded results for home sides - and this is shown too in the confidence intervals in @survdistance.
#figure(
  image("../assets/cumhaz_distance.png", fit: "contain"),
  caption: [Cumulative hazard by distance travelled]
) <cumhazdistance>
Scoring rate appears to pick up at the end of the game for closer-located teams, and unlike with our previous two variables we see that the distinction between the groups near the start of the match is much wider, with the log difference line being positive (if very bumpy) for almost the entire 90 minute window.
The W-G test shows $chi_("df"=1)^2 = 5.8, p=0.02$, leading us to take this as a significant difference.

=== Interaction with derby matches
A 'derby' game is common terminology for a football match between two rival teams. Usually both teams are local to one another, maybe representing neighbouring towns or nearby city districts. 
These games are often considered to be higher stakes for fans of each team. In some particular cases this has boiled over into incidents of violence or property destruction.
Due to the fact that derby games are fought between geographically close teams, the effect found earlier where away teams score worse when travelling farther might lead these games to being more closely matched than games in general. 

Our definition of what games constitute derbies can be found in the data collection section earlier.
Perhaps due to the much smaller size of the derby group (about 60 games in 380) the estimates given by R are much wider, visible in the confidence intervals in red in @survderby.  The survival curves here seem very similar, with the point estimates not too distinct and one line contained completely within the CI of the other. 
#figure(
  image("../assets/surv_derby.png", fit: "contain"),
  caption: [Survival curves for (non-/)derby games]
) <survderby>
This similarity is just as visible in the CH plots, where both lines follow very similar paths. This could be a symptom of the smaller sample size, as that is likely what lends the curve for derby games to be so jagged.
#figure(
  image("../assets/cumhaz_derby.png", fit: "contain"),
  caption: [Cumulative hazard in (non-/)derby games]
) <cumhazderby>
The estimated log difference is very close to zero, and the observed log difference doesn't follow it closely at any point on the plot. Due to the erratic log-difference curve, we will confirm our visual analysis with the Wilcoxon test, which gives $chi_("df"=1)^2 = 0.5, p=0.5$.


=== Other interactions
An interaction with the day of the game was tested. No significant link was found between date/time of kickoff (for this we use the Unix timestamp, an integer value which gives the number of seconds since 1/1/70) and TTFG, nor was a link found for a similar factor variable classifying whether the match occured earlier/later in the season. 


Red cards, much like derby games, are scarcely represented in the data, hence much like derby games no significant effects were found in tests, even when restricting the data to only one side. In some literature (see @nevoritov) the authors use the match time at which red cards are given to investigate the change in hazard before and after the event but we don't have this data.  
The overall effect of red cards on first-goal time may be murky, since naturally it means a team is playing with one fewer player for some time, however there may be a confounding effect whereby a more aggressive team would score quicker goals and commit more rule-breaking behaviour. We will investigate this as an interaction term.




== Fitting parametric models
The R function `flexsurvreg` can fit parametric survival models of two kinds, accelerated failure time (AFT) and proportional hazards (PH). AFT models focus on the effects of each variable on survival times, so the survival time of the $k$th subject is distributed as $T_(k) = e^(eta_(k)) T_(0)$, with $eta = sum_(i)beta_(i) X_(i)$ the linear predictor and $T_(0)$ follows some appropriate baseline distribution. A unit change in the linear predictor thus "speeds up time" for the $k$th subject by a factor of $e$. 

By contrast PH models focus on how each variable affects the hazard, so $h_(k)(t) = exp(eta_(k))h_(0)(t)$ gives the hazard of the $k$th subject as a multiple of a baseline hazard. A unit increase in the linear predictor thus "increases the risk" of the event of interest by a factor of $e$. For parametric models this $h_(0)$ has a closed form, for instance an exponentially distributed survival time correponds to a constant baseline hazard.
//https://www.mdpi.com/2227-7390/12/1/56
Weibull models (and therefore exponential models) can be used in either of these frameworks, as Weibull distributions satisfy both properties. For the sake of comparison with the popular Cox regression model, we will take the proportional hazards approach. \

Having examined the effects of a few variables on TTFG alone, we will try fitting two parametric models to our data, one exponential and one Weibull.
In the logistic regression section we saw that the raw points values didn't follow the proportional odds assumption. As both the exponential and Weibull models follow a proportional hazards relationship, we may see a related effect here.
The first step will be to remove insignificant variables, and then to check that the model fits are appropriate.
Away team travel distance as a raw kilometer value was tested but found insignificant, so much like with the ordered logistic regression model we will use the closer/farther variable.
The model using raw distance values reduced down (after removal of insignificant variables) to a model strictly nested inside the one we'll soon arrive at, hence we won't include it here.

```R
s ~ home_or_away*(red_card_home + red_card_away + distance_grouping) + points2021 + opponent_points2021
```
Given the large amount of data in the model selection process, relevant R output can be found in the appendices. We first remove any insignificant interaction terms to arrive at our 2nd group of models, then to arrive at our third and final set of models we remove any variables still insignificant.

The results of this process are in the following table. Both final models use the same set of variables.

#table(
columns:3,
[Variable], [Estimate], [S.E.],
[Shape $gamma$], [1.145], [0.050],
[Scale $alpha$], [0.007], [0.002],
[Home (1) vs Away], [0.037], [0.140],
[Closer vs Farther (1)], [-0.460], [0.148],
[Points last season], [0.010], [0.003],
[Opponent's Points], [-0.007], [0.003],
[Side*Distance], [0.571], [0.202],
)
And for the exponential model:
#table(
columns:3,
[Variable], [Estimate], [S.E],
[Rate $lambda$], [0.013], [0.003],
[Home (1) vs Away], [0.030], [0.140],
[Closer vs Farther (1)], [-0.442], [0.148],
[Points last season], [0.010], [0.003],
[Opponent's Points], [-0.007], [0.003],
[Side*Distance], [0.548], [0.202],
)

All coefficients are significant to a 95% level, except the home/away factor, which is included as its interaction with distance is significant. Unlike in the logistic regression section, red cards seem to have less of an effect here. This could be because first goals skew towards the early game, meaning they will often have already occurred by the time a red card is given, unlike match outcome which is affected by events throughout the full course of the match.
The opposite signs on the distance variable and the distance interaction can be explained as the effect of travel distance differing for each team side. 
Coefficients in each model are broadly similar, as could be expected for similar baseline distributions. The shape parameter estimated for the Weibull-based model is greater than one, indicating an increasing hazard.

There are a number of methods for assessing Weibull PH model suitability. If the model depends on some categorical variable, one can plot $log(-log(S(t)))$ against $log(t)$ using the Kaplan Meier survival functions $S(t)$ estimated for each strata. If the underlying distribution is indeed Weibull, then under the proportional hazards assumption this plot should show a number of parallel lines.
#figure(
  image("../assets/logsurvplot.png"),
) <graphplot>
In @graphplot we show an example of such a test, for the home vs away variable. Most of the model observations are contained in the $3<ln(t)$ range, hence the clearer distinction there. We can see the same effect here as in the non-constant log HR curves earlier, where the curves seem hard to tell apart in the earlier parts of the game. Nonetheless we can at least see that the lines appear parallel around the top right. This method is less useful for continuous variables like points.\

There exist residual methods for assessing the fit of parametric survival models exist, such as Cox-Snell, martingale, and deviance residuals. We will just examine the Cox-Snell residuals.

// Explanation of cox snell resids
//https://stats.stackexchange.com/questions/246812/cox-snell-residuals-in-r
#set table(
  fill: none,
)
#figure(
  table(
    columns: 2,
    image("../assets/qqweib.png", fit: "contain"),
    image("../assets/qqexp.png", fit: "contain")
  ),
  caption: [Plots of Cox Snell residuals]
)
#set table(
  fill: (x, y) =>
    if x == 0 or y == 0 {
      gray.lighten(40%)
    },
  align: right,
)

In the Q-Q plots above, we plot Cox-Snell residuals against theoretical $~"exponential"(1)$ quantiles, with the $y=x$ line overlaid in red. 

Non-censored Weibull-distributed data have, in theory, exponentially distributed Cox-Snell residuals. @coxsnell. We see that both models fit the data quite well.
For both models the smaller residuals seem almost perfectly exponentially distributed.
At the upper end, the Weibull model seems to fit a bit closer, whereas the exponential model seems to have residuals lying above the line.

To examine which of the models fits better we can perform a likelihood ratio test.
As the same variables turned out to be significant, and the exponential is a special case of the Weibull model, these models are nested.
Calculating $-2(l_2 - l_1)$ we find a value of $chi^(2) = 9.07$.
This is on 1 df, as Weibull models estimate exactly one extra parameter, the shape $gamma$.
This is above the 1% $chi^2$ critical value of 6.635, hence we can conclude that the extra flexibility of the Weibull model does improve the fit somewhat.

We can quantify the estimated hazard mulitipliers for different categories of teams over the baseline hazard. While the differing coefficients of each team's points indicate a changing relationship of the hazard as points vary, we will just assume the two teams have an average number, 57.2.
#table(
  columns:3,
  [], [Home], [Away],
  [Closer], [1.25], [1.20],
  [Farther], [1.39], [0.76]
)
The advantage of home teams is clearly seen by comparing the first column to the second. The interaction with distance also shows, as away team performance almost matches that of home teams when they don't have to travel as far

== How does a Cox regression compare?

The linear fits to the Kaplan Meier curves shown in the univariate analyses seemed fairly close, but the discontinuous jump at 45 minutes and seeming non-linearity throughout parts of the curves may hint that a more general approach could be useful.
The Weibull models we just fit can accommodate a changing hazard, but they only have limited parameters through which to describe the full shape of the curve - their cumulative hazard has the form $H(t) prop t^(gamma)$.
For some curves, such as in @cumhazhomeaway, the early sections appear linear, and these are followed by a slightly concave shape.
In order to account for these features, as well as the jump in hazard at 45 minutes, we can use Cox regression.\
Cox regression still assumes proportional hazards, that is, the shape of the curve must be more or less the same for e.g. home teams and away teams, but they may differ by a constant multiplicative factor. The advantage of this is that the shape can be abstracted away, using a baseline hazard $h_(0)(t)$ of which the hazards for each treatment group are multiples. This means we aren't tied to a power-function cumulative hazard curve like a Weibull model, however it means that the survival curve may not be smooth, and we lose the niceties of a parametric distribution such as closed-form expressions for quantities of interest.

Since we've already determined the variables that are significant for the parametric model, we will fit a Cox model against the same covariates, and we find the following coefficients.
#table(
  columns: 3,
[Variable], [$beta$], [p],
[Home (1) vs Away], [0.0350], [0.802],
[Closer vs Farther (1)], [-0.460], [0.002],
[Points last season], [0.010], [0.000],
[Opponent points], [-0.0073], [0.020],
[Side \* Points], [0.574], [0.005]
)

The coefficients here are very similar to those in the Weibull fit, providing some more evidence that goal times follow a Weibull distribution quite well, as the less constrained baseline hazard function here still results in similar coefficient estimates.

As mentioned, the Cox model requires the proportional hazards assumption to hold. We can check this in R using `cox.zph()`. The output of this function is as follows:
```R
> cox.zph(coxreg)
                                chisq df     p
home_or_away                   0.5673  1 0.451
distance_grouping              0.0699  1 0.792
points2021                     0.6350  1 0.426
opponent_points2021            2.0605  1 0.151
home_or_away:distance_grouping 3.3777  1 0.066
GLOBAL                         9.0028  5 0.109
```
This test uses residuals from the models to test against the null hypothesis that hazards are indeed proportional. /*cite: Proportional hazards tests and diagnostics based on weighted residuals*/  As we can see, none of the tests for individual variables, nor for the models as a whole, gives enough evidence to reject the null hypothesis, though the interaction term gets close.


If (e.g.) the home-advantage, expressed as a hazard ratio between home and away teams, were larger towards the end of a match, this would violate the proportional hazards assumption.
The Schoenfeld residuals $s_k$ at time $t_k$ are calculated as the differences between the covariate values of the subject having an event at time $t_k$, minus the mean covariate values of those subjects still at risk, who haven't had the event yet @parkhendry.

These can be shown to have expected values proportional to the difference $beta(t)-beta$ between a time-varying coefficient and the constant Cox regression coefficient @grambsch. Therefore a trend in the Schoenfeld residuals implies that the model should perhaps be respecified with a time-dependent coefficient.

#figure(
  image("../assets/schoenfeld_interaction.png", fit: "contain"),
  caption: [Schoenfeld residuals against transformed match-time]
)<schoenfeld>
In @schoenfeld we plot scaled values of $s_k$ against time as the solid line.
Match time is rescaled according to event density, so the intervals between x-axis ticks get longer.

We can see that the black line rises toward the end of the match, indicating an increased effect on hazard ratio between the groups. We could conjecture this is caused by away teams who travel farther experiencing more fatigue as the game goes on, giving their opponents an edge, though the trend isn't strong enough to be statistically significant (and we don't have the data to confidently make causal arguments like this). Similar plots can be made for the other variables, though they show no trends of a similar magnitude.


Reassured that our Cox regression fit is appropriate, we can make some comparisons to the parametric model. To pick an example we can look at the hazard ratio for Arsenal (69 points last season) playing at home against Newcastle (49 points, 394km away), over the baseline hazard.
For our Cox model we see
$ (h_(1)(t))/(h_(0)(t)) &= exp(0.0350 times 1 - 0.460 times 1 + ...)\
&= exp(0.515) = 1.673 $
And the corresponding ratio for Newcastle in this match is 0.639, giving a hazard ratio between the teams of 2.620. Taking each of these values to be their lower (upper) 95% confidence bounds, we find a lower (upper) estimate for this hazard ratio to be 1.350 (5.0837)



We can calculate the same ratio for our earlier parametric models. The hazard function for the $i"th"$ subject can be parametrised as
$ h_(i)(t) = alpha gamma t^(gamma-1)exp(beta_1 X_(i 1) + ...) $
Then the hazard ratio between teams 1 and 2 is:
$ (h_(2)(t))/(h_(1)(t)) = exp(sum_(j) beta_j (X_(2 j)-X_(1 j))) $
The between-teams hazard ratio for our earlier Weibull model is 2.617, again very similar to the Cox model. 
While clearly not an exhaustive test, this example does demonstrate how similar the predictions given by each model are. 

Given this, we might prefer the Weibull model over the Cox model in order to answer questions about our data set.
The closed form hazard function gives a more complete description of our data, and in particular for the Weibull model can also be easily rephrased as an accelerated failure time model, letting us answer questions about expected goal times much more easily than the Cox model.
While the flexibility of semi- or non-parametric models is often needed in situations with unusually behaved data, in our case these don't seem particularly necessary.

== A brief look at a count model
Given a process with exponentially distributed inter-event times, the number of such events in some timespan will be Poisson distributed. Modelling the number of goals scored in a football match is often done through a Poisson approach, but methods have been developed to fit count models to data with Weibull distributed inter-event times. \

Such models are often inexpressible in closed form, but series approximations exist for calculation. 
The probability of $n$ events having occurred by time $t$ can be expressed as the probability density of having the first event at time $tau$, multiplied by the probability of $n-1$ events having occured in the period between $tau$ and $t$, integrated over $tau$.
The first term is given by the pdf of the inter-event time  distribution, $f(tau)$, so this calculation gives a recursive solution for the count distribution, solved practically through Taylor series @mcshane.\
#figure(
  image("../assets/wei_count.png",fit:"contain"),
  caption:[Bar plot of actual/fitted frequency for each goal count]
) <weicount>
Lucky for us, authors Kharrat and Boshnakov (mentioned in the literature review) have written the R package `countr`, which handles the model fitting.
We will fit a Poisson and a Weibull renewal-count model, each against the same covariates as before.


In a different paper @kharratboshnakov2017 the same authors show a plot similar to @weicount, though ours differs in that the Weibull count model doesn't show any obvious improvement visually over the Poisson one. We can confirm our suspicions with a likelihood ratio test, which shows that with $chi^(2)_("df"=1) = 2.17, p = 0.141$ the extra shape parameter used in the Weibull count model doesn't significantly improve the fit. 
Our earlier analysis of first goals came to the opposite conclusion, possibly suggesting that the nature of 2nd, 3rd goals might be different from first goals.
The authors mentioned examine over 1000 matches, so the issue could conceivably be one of sample size, though changing conditions in the sport could also be at play, as 7 years have passed since their paper. More work could be done to show why this difference in results has occured.