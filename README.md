# CSAS2025_SYMULA_HILL

SEBASTIAN:
Bar charts, scatterplots
test for difference in overall swing length & bat speed compared to pitch specific averages
test for difference in pitch type when batter is good at hitting pitcher's favorite pitch
test for difference in pitch location when batter is good at hitting pitcher's favorite pitch location


Baseball Data Challenge Work.R takes a subset of the data with only pitches to hitters who were either in the top 25% for average bat speed or the bottom 25% for average swing length, and sorts both that subset and the large dataset by individual at bat. It then removes all of the pitches that did not end the at bat, to be able to sort through the end results of the at bats better. Next, it takes subsets of both of those datasets for only those at bats which qualify for each type of quality at bat and calculates the percentages of those at bats for both the smaller subset of hitters and the overall league. Then, it makes a ‘results’ dataframe to visually compare those percentages for each category, with the ‘better’ column indicating whether hitters with faster or shorter swings fared better than the league did in that category, as well as a column for the corresponding percent differences. It ends with a hypothesis test for evidence of a significant difference between the smaller subset of hitters and the overall league. 
