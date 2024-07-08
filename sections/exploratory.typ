In this section we will examine our data set, to get some idea of the magnitude of various effects on match result.

=== Testing correlations between variables
We considered various variables as proxies for team strength. We wanted to use a measure that exists before the beginning of a league season, so results in the previous league seemed obvious. Data on the number of wins in the previous league, total goal difference, and league points were assembled. As is to be expected, these are highly correlated, with pairwise Pearson correlations between 0.94 and 0.98. Since each variable almost entirely explains the others, from here on we will only use league points. Total points seems most relevant to the teams as it is the foremost factor in determining the team's league ranking, and thus the team's prospects of winning or of relegation. 
For applications requiring factor variables (e.g. Kaplan Meier curves) we will use a variable called `grouping`, a factor variable encoding whether or not the team scored above the median number of points. Teams promoted from a lower league, who have no EPL league points from last season, will be assigned to the lower group. \


We also wanted to test for a link between strength and location, theorising that perhaps teams in the center of the country and London would have higher local population density, from which they could draw more support in terms of revenue and choice in young players. To do this we calculated a Pearson rank statistic for the median distance a team had to travel for their away games, against their last-season points. This confirmed no significant link between the two ($rho = 0.14, t_("df"=15) = 0.58, p = 57%$), hence we can discount this as a confounding effect.


=== Home results vs away
@homeawaybar shows a bar chart of match results, split by whether the team was playing at home or away. Home teams won 49.2% of the matches, with 22.3% of matches a tie, and 28.3% were won by the away side. The advantage enjoyed by teams playing at home is clearly visible, and is about on par with results from other recent EPL seasons.
#figure(
  image("../assets/bar_result_homeaway.png", fit: "contain"),
  caption: [Match results for teams playing at home vs away.]
) <homeawaybar>

=== Match results by team strength
We can ignore the effects of home advantage by looking at the results of each match for each team, both the home and away side.
#figure(
  image("../assets/bar_result_grouping.png", fit: "contain"),
  caption: [Match results for lower and higher grouped teams]
) <groupingbar>
@groupingbar takes these side-agnostic results and splits them by whether the team attained above or below median league points in the previous season. We use this as a proxy for looking at stronger vs weaker teams. We can see that higher-grouped teams, naturally, had a higher chance of winning at 47.8%, so slightly less than average win rate for home teams. The chance of both ties and losses are smaller than among lower-ranked teams (19.0% vs 24.6%, 33.1% vs 42.6% resp.).

#figure(
  image("../assets/barplot_grouping_grouping.png", fit: "contain"),
  caption: [Match results by groupings of either side]
) <doublegroupingbar>
For more fine-grained information on the distribution of match results by grouping, we can plot against the grouping of each side. From @doublegroupingbar we can see that games among two higher-grouped teams tend to be more decisive than among two lower-grouped teams. as the chance of a draw on the farthest right bar is 17.3%, much lower than 28.1% on the left. Higher grouped teams can be seen to win 51.7% of the time against those in the lower group, a win chance higher even than for home teams vs away. In these unbalanced games we can see the rate of draws, 20%, is between the rates in games with equally grouped teams. This could suggest that team strength is a stronger determiner of outcome at higher levels, with results being more unpredictable among if one or more teams is on the weaker end.

=== Interaction between side and strength
#figure(
  image("../assets/bar_result_homeaway_grouping.png", fit: "contain"),
  caption: [Match results for teams by home vs away and grouping]
) <groupinghomeaway>
@groupinghomeaway shows the results for each side based on their grouping and whether they were playing at home or away. We see again that draw rate (left-to-right: 24.2%, 19.7%, 25.1%, 18.3%) is increased among matches with a lower-grouped team. Higher ranked teams playing at home win 59.1% of their games, 22.5% higher than their win rate away. This difference in win rate is larger than among lower ranked teams, who enjoy only a $42.7% - 22.8% = 19.9%$ higher win rate at home. Similarly, higher grouped teams playing away have a 13.9% higher win rate than lower grouped teams away, below the 16.5% difference among differently grouped home teams. This stacked effect could suggest an interaction between team strength and game side on win rate. \

=== Win Margin
Across every game in the season, the average win margin ($#"home goals" - #"away goals"$) was 0.48, so in a sense the home advantage can be quantified as being about equivalent to a half-goal headstart. Perhaps just coincidence, but this effect seems to be fairly consistent across time and league, as it's remarkably close to the value of 0.47 found by author #cite(<stefani>, form: "prose").
To investigate the effect of team strength on win margin, we can fit a simple linear regression of win-margin against the last-season points of each team playing. Each covariate was normalised by subtracting the mean value (57.4).
We fit the following model.
$ "win margin" = &+ 0.415 \
                 &+ 0.0315 times "adj. home points"\ 
                 &- 0.0170 times "adj. away points" $
The first two coefficients are found significant with $p<0.001$, the third with $p = 0.016$. The positive intercept term corresponds to the advantage of the home team - due to positive win margin indicating a home win. The higher coefficient of the home team's points is in agreement with the stacked effect from earlier. In the context of matches between teams of similar strength, the advantage of the home side would increase linearly as the strengths of the teams increased. For instance, a match between Everton (H) and Leeds (A) (39 and 38 points resp. - at the bottom) would be predicted a home win margin of $0.166$. A similar theoretical match of Manchester City (H) vs Liverpool (A) (93, 92 points - at the top) would be predicted $0.950$ more goals in favour of the home team.

#figure(
  image("../assets/win_margin_strength.png", fit: "contain"),
  caption: [Plot of win margin against last-season points, with regression lines]
) <winmarginstrength>

Despite strong significance of each coefficient, this model achieves an adj. $R^(2)$ value of $0.09$, suggesting that it only captures a small portion of the variation in win margin. We can visualise this fact using @winmarginstrength.
The regression lines overlaid on this scatter plot were calculated by substituting opponent points into the regression model. The teams were chosen as having the highest and lowest point values, as well as Arsenal in the middle. The large vertical spread of the data outside of the widest bounds of our regression is quite clear, hinting towards the large variation inherent in football results.


=== Other Results
No significant changes in win proportion were observed across changes in time. The time of each match was split into 4 bins and a $chi^2$ test was performed, finding no link with $chi^2 = 8.4$ on 6 degrees of freedom, $p=21%$. This variable will be considered in later models, as there could still be interesting interactions with other variables.

Red cards are found to have a significant effect on match outcome, when awarded to the home team. Due to the low number of matches with a team awarded multiple cards, we consider only the presence or absence of red cards. A Spearman test for correlation between presence/absence of red cards against match result (as numeric value 1/2/3) shows a significant ($p=2.9%$) correlation for home red cards but insignificant ($p=78%$) for away red cards, possibly due to the lower number of those compared to home cards.