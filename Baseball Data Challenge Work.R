Data <- read.csv("statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv")
View(Data)
head(Data)
summary(Data)
colnames(Data)
library(psych)
describe(Data[c("bat_speed", "swing_length")])
fivenum(Data$bat_speed)
#fastest quarter between 74.3 and 88 mph
fivenum(Data$swing_length)
#shortest quarter between 0.3 and 6.6 feet
plot(data=Data, swing_length~bat_speed)

#Quality at bats include:
#6-8 pitches without a strikeout
#9+ pitches including strikeouts
#Hard hit ball (100+?) 
#Walk 
#HBP
#Moving runner into scoring position
#RBI
#Sac fly
#Sac bunt
#Hit

#Subset data on players who average swing with those speeds or lengths
#since some at bats probably aren't all in the fast or short categories for every pitch / swing of the at bat

length_means <- aggregate(swing_length ~ batter, mean, data = Data)
short_swings <- length_means[length_means$swing_length <= 6.6, ]
speed_means <- aggregate(bat_speed ~ batter, mean, data = Data)
fast_swings <- speed_means[speed_means$bat_speed >= 74.3, ]

full_outer_join <- merge(short_swings, fast_swings, by = "batter", all = TRUE)
my_batters <- subset(full_outer_join, select = c(batter))
myData <- merge(my_batters, Data, by = "batter")

library(dplyr)
n_distinct(myData$batter)
n_distinct(myData$player_name)
#79 batters in this subset

n_distinct(Data$batter)
n_distinct(Data$player_name)
#651 total batters

#572 batters not in the subset

View(myData)

library(dplyr)
sorted_by_at_bats <- myData %>%
  arrange(player_name, game_date, at_bat_number, pitch_number)

big_sorted_by_at_bats <- Data %>%
  arrange(player_name, game_date, at_bat_number, pitch_number)

##drop nas or missing values in events

sorted_w_events_only <- sorted_by_at_bats[!(is.na(sorted_by_at_bats$events) | sorted_by_at_bats$events==""), ]

big_sorted_w_events_only <- big_sorted_by_at_bats[!(is.na(big_sorted_by_at_bats$events) | big_sorted_by_at_bats$events==""), ]

##Category 1: 6-8 pitch at bat without a strikeout

six_through_eight <- sorted_w_events_only[sorted_w_events_only$pitch_number == 6 | 
                                            sorted_w_events_only$pitch_number == 7 |
                                            sorted_w_events_only$pitch_number == 8, ]

ste_no_so <- six_through_eight[six_through_eight$events != "strikeout", ]

Category_1_Percentage <- nrow(ste_no_so) / nrow(sorted_w_events_only) * 100
Category_1_Percentage

#Compared to general league number

big_six_through_eight <- big_sorted_w_events_only[big_sorted_w_events_only$pitch_number == 6 | 
                                            big_sorted_w_events_only$pitch_number == 7 |
                                            big_sorted_w_events_only$pitch_number == 8, ]

big_ste_no_so <- big_six_through_eight[big_six_through_eight$events != "strikeout", ]

big_Category_1_Percentage <- nrow(big_ste_no_so) / nrow(big_sorted_w_events_only) * 100
big_Category_1_Percentage


##Category 2: 9+ pitches including strikeouts

nine_plus <- sorted_w_events_only[sorted_w_events_only$pitch_number > 8, ]

big_nine_plus <- big_sorted_w_events_only[big_sorted_w_events_only$pitch_number > 8, ]

Category_2_Percentage <- nrow(nine_plus) / nrow(sorted_w_events_only) * 100
Category_2_Percentage

#Compared to general league number

big_Category_2_Percentage <- nrow(big_nine_plus) / nrow(big_sorted_w_events_only) * 100
big_Category_2_Percentage


##Category 3: Hard hit ball (100+ mph)

hard_hit <- sorted_w_events_only[sorted_w_events_only$launch_speed >= 100, ]

big_hard_hit <- big_sorted_w_events_only[big_sorted_w_events_only$launch_speed >= 100, ]

Category_3_Percentage <- nrow(hard_hit) / nrow(sorted_w_events_only) * 100
Category_3_Percentage

#Compared to general league number

big_Category_3_Percentage <- nrow(big_hard_hit) / nrow(big_sorted_w_events_only) * 100
big_Category_3_Percentage


##Category 4: Walk

walk <- sorted_w_events_only[sorted_w_events_only$events == "walk", ]

big_walk <- big_sorted_w_events_only[big_sorted_w_events_only$events == "walk", ]

Category_4_Percentage <- nrow(walk) / nrow(sorted_w_events_only) * 100
Category_4_Percentage

#Compared to general league number

big_Category_4_Percentage <- nrow(big_walk) / nrow(big_sorted_w_events_only) * 100
big_Category_4_Percentage


##Category 5: HBP

hbp <- sorted_w_events_only[sorted_w_events_only$events == "hit_by_pitch", ]

big_hbp <- big_sorted_w_events_only[big_sorted_w_events_only$events == "hit_by_pitch", ]

Category_5_Percentage <- nrow(hbp) / nrow(sorted_w_events_only) * 100
Category_5_Percentage

#Compared to general league number

big_Category_5_Percentage <- nrow(big_hbp) / nrow(big_sorted_w_events_only) * 100
big_Category_5_Percentage


##Category 6: Moving runner into scoring position

no_second_or_third <- sorted_w_events_only[(is.na(sorted_w_events_only$on_2b) & is.na(sorted_w_events_only$on_3b)), ]
no_second_or_third_or_hits <- no_second_or_third[!grepl("single", no_second_or_third$events), ]
no_second_or_third_or_hits <- no_second_or_third_or_hits[!grepl("double", no_second_or_third_or_hits$events), ]
no_second_or_third_or_hits <- no_second_or_third_or_hits[!grepl("triple", no_second_or_third_or_hits$events), ]
no_second_or_third_or_hits <- no_second_or_third_or_hits[!grepl("home_run", no_second_or_third_or_hits$events), ]

move_over <- no_second_or_third_or_hits[grep("to 2nd", no_second_or_third_or_hits$des), ]
move_over_2 <- no_second_or_third_or_hits[grep("to 3rd", no_second_or_third_or_hits$des), ]

big_no_second_or_third <- big_sorted_w_events_only[(is.na(big_sorted_w_events_only$on_2b) & is.na(big_sorted_w_events_only$on_3b)), ]
big_no_second_or_third_or_hits <- big_no_second_or_third[!grepl("single", big_no_second_or_third$events), ]
big_no_second_or_third_or_hits <- big_no_second_or_third_or_hits[!grepl("double", big_no_second_or_third_or_hits$events), ]
big_no_second_or_third_or_hits <- big_no_second_or_third_or_hits[!grepl("triple", big_no_second_or_third_or_hits$events), ]
big_no_second_or_third_or_hits <- big_no_second_or_third_or_hits[!grepl("home_run", big_no_second_or_third_or_hits$events), ]

big_move_over <- big_no_second_or_third_or_hits[grep("to 2nd", big_no_second_or_third_or_hits$des), ]
big_move_over_2 <- big_no_second_or_third_or_hits[grep("to 3rd", big_no_second_or_third_or_hits$des), ]

Category_6_Percentage <- (nrow(move_over) + nrow(move_over_2)) / nrow(sorted_w_events_only) * 100
Category_6_Percentage

#Compared to general league number

big_Category_6_Percentage <- (nrow(big_move_over) + nrow(big_move_over_2)) / nrow(big_sorted_w_events_only) * 100
big_Category_6_Percentage


##Category 7: RBI

rbi <- sorted_w_events_only[grep("scores", sorted_w_events_only$des), ]

big_rbi <- big_sorted_w_events_only[grep("scores", big_sorted_w_events_only$des), ]

Category_7_Percentage <- nrow(rbi) / nrow(sorted_w_events_only) * 100
Category_7_Percentage

#Compared to general league number

big_Category_7_Percentage <- nrow(big_rbi) / nrow(big_sorted_w_events_only) * 100
big_Category_7_Percentage


##Category 8: Sac fly

sf <- sorted_w_events_only[sorted_w_events_only$events == "sac_fly", ]

big_sf <- big_sorted_w_events_only[big_sorted_w_events_only$events == "sac_fly", ]

Category_8_Percentage <- nrow(sf) / nrow(sorted_w_events_only) * 100
Category_8_Percentage

#Compared to general league number

big_Category_8_Percentage <- nrow(big_sf) / nrow(big_sorted_w_events_only) * 100
big_Category_8_Percentage


##Category 9: Sac bunt

sb <- sorted_w_events_only[sorted_w_events_only$events == "sac_bunt", ]

big_sb <- big_sorted_w_events_only[big_sorted_w_events_only$events == "sac_bunt", ]

Category_9_Percentage <- nrow(sb) / nrow(sorted_w_events_only) * 100
Category_9_Percentage

#Compared to general league number

big_Category_9_Percentage <- nrow(big_sb) / nrow(big_sorted_w_events_only) * 100
big_Category_9_Percentage

##Category 10: Hit

hit <- sorted_w_events_only[sorted_w_events_only$events == "single" | 
                                            sorted_w_events_only$events == "double" |
                                            sorted_w_events_only$events == "triple" |
                                            sorted_w_events_only$events == "home_run", ]

big_hit <- big_sorted_w_events_only[big_sorted_w_events_only$events == "single" | 
                              big_sorted_w_events_only$events == "double" |
                              big_sorted_w_events_only$events == "triple" |
                            big_sorted_w_events_only$events == "home_run", ]

Category_10_Percentage <- nrow(hit) / nrow(sorted_w_events_only) * 100
Category_10_Percentage

#Compared to general league number

big_Category_10_Percentage <- nrow(big_hit) / nrow(big_sorted_w_events_only) * 100
big_Category_10_Percentage

####################Results DF#################



Results <- data.frame(
  Category = c("6-8 pitch at bat, no strikeout", "9+ pitch at bat", "Hard hit ball (100+ mph)", 
               "Walk", "HBP", "Move Runners into Scoring Position", "RBI", "Sac Fly", "Sac Bunt", "Hit"),
  Hitters_with_Fast_or_Short_Swings_Percentage = c(Category_1_Percentage, Category_2_Percentage, Category_3_Percentage,
                                        Category_4_Percentage, Category_5_Percentage, Category_6_Percentage,
                                        Category_7_Percentage, Category_8_Percentage, Category_9_Percentage, 
                                        Category_10_Percentage),
  League_Percentage = c(big_Category_1_Percentage, big_Category_2_Percentage, big_Category_3_Percentage,
                        big_Category_4_Percentage, big_Category_5_Percentage, big_Category_6_Percentage,
                        big_Category_7_Percentage, big_Category_8_Percentage, big_Category_9_Percentage,
                        big_Category_10_Percentage)
)
View(Results)

Results$Better <- "N/A"
Results$Better[Results$Hitters_with_Fast_or_Short_Swings_Percentage > Results$League_Percentage] <- "Yes"
Results$Better[Results$Hitters_with_Fast_or_Short_Swings_Percentage < Results$League_Percentage] <- "No"

Results$Difference <- Results$Hitters_with_Fast_or_Short_Swings_Percentage - Results$League_Percentage

View(Results)

with(Results, t.test(Hitters_with_Fast_or_Short_Swings_Percentage, League_Percentage, paired=TRUE))

###The p-value is 0.4738, much greater than alpha = 0.05, so we do not reject the null hypothesis
###that players with faster or shorter swings are better, on average, than the rest of the league 
###at getting quality at bats.