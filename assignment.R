#################
#### PART 1 #####
#################

#install.packages("dplyr")
#library(dplyr)

# Loading data
alcohol_consumption <- read.csv('data/any_drinking.csv', stringsAsFactors = FALSE)

#View(alcohol_consumption)

# Selecting required columns
data_2012 <- select(alcohol_consumption, state, location, both_sexes_2012, females_2012, males_2012)

# Create a new column with difference of male and female 2012
data_2012 <- mutate(data_2012, gender_diff_2012 = males_2012 - females_2012)

# Number of locations where females drink more than males
female_more_consumption <- filter(data_2012, females_2012 > males_2012)
print(paste("Females drink more than males in",length(female_more_consumption), "locations in the U.S."))

# Finding the location with similar male and female drinking percentage
similar_consumption <- filter(data_2012, gender_diff_2012 == min(gender_diff_2012))
print(paste("Drinking is most similar between genders in", similar_consumption$location, ",",similar_consumption$state))

# New data frame with only the 51 state and information
statewise_data_2012 <- filter(data_2012, state == location)
statewise_data_2012 <- select(statewise_data_2012, state, both_sexes_2012, females_2012, males_2012)

# State with maximum drinking (females and males combined)
max_state <- filter(statewise_data_2012, both_sexes_2012 == max(both_sexes_2012))
print(select(max_state, state, both_sexes_2012))

# State with minimum drinking (females and males combined)
min_state <- filter(statewise_data_2012, both_sexes_2012 == min(both_sexes_2012))
print(select(min_state, state, both_sexes_2012))

# Difference in combined-sexes drinking rates
difference_rates <- max_state$both_sexes_2012 - min_state$both_sexes_2012
print(paste("Range in drinking rate was", difference_rates, "%"))

#################
#### PART 2 #####
#################

# Reading the csv file
binge_drinking <- read.csv('data/binge_drinking.csv', stringsAsFactors = FALSE)

# Filters only county-like places
county_drinking <- filter(binge_drinking, state != location & state != "National")

# Confirmation that the data is correct
# + 1 is for nation
# returns TRUE
#nrow(alcohol_consumption) - nrow(county_drinking) == nrow(filter(data_2012, state == location)) + 1

# Adding 3 new columns with difference in drinking rates
county_drinking <- mutate(county_drinking, both_sexes_diff = both_sexes_2012 - both_sexes_2002,
                          females_diff = females_2012 - females_2002,
                          males_diff = males_2012 - males_2002)

# Mean rate of binge drinking in 2012 
mean_bingedrinking <- round(summarise(county_drinking, mean = mean(both_sexes_2012)), 5)
print(paste("The mean rate of binge drinking in 2012 was", mean_bingedrinking))

# Calculates the minimum and maximum and prints them
min_max_2012 <- summarise(county_drinking, min_both_binge = min(both_sexes_2012), max_both_binge = max(both_sexes_2012))
print(paste("Lowest Level =", min_max_2012$min_both_binge ,"  Highest Level =", min_max_2012$max_both_binge))

# Binds the rows and writes the file to csv file
group_by(county_drinking, state) %>% 
  summarise(minimum = min(both_sexes_2012), maximumn = max(both_sexes_2012)) %>% 
  write.csv("data/state_binge_drinking.csv")

# Percentage increase in males drinking
county_male_increase <- filter(county_drinking, males_diff > 0)
print(paste("Male binging increased in ", round(nrow(county_male_increase)/nrow(county_drinking) *100),
            "% of counties.", sep = "", collapse = NULL))

# Percentage increase in females drinking
county_female_increase <- filter(county_drinking, females_diff > 0)
print(paste("Female binging increased in ", round(nrow(county_female_increase)/nrow(county_drinking) *100),
            "% of counties.", sep = "", collapse = NULL))

# Percentage of counties with increase in female drinking but decrease in male drinking
female_increase_male_decrease <- filter(county_drinking, females_diff > 0 & males_diff < 0)
print(paste("Female binging increased & male binging decreased in ",
            round(nrow(female_increase_male_decrease)/nrow(county_drinking)*100), "% of counties.",sep = "",
            collapse = NULL))

# State with the largest median increase in male binge drinking
median_states_grouped <- group_by(county_drinking, state) %>% 
  summarise(male_median_2012 = median(males_2012), male_median_2002 = median(males_2002)) %>%
  mutate(male_median_increase = (male_median_2012 - male_median_2002)) %>%
  filter(male_median_increase == max(male_median_increase)) %>%
  select(state, male_median_increase)
print(median_states_grouped)

#States with the largest median increase in female binge drinking
median_states_grouped <- group_by(female_increase_male_decrease, state) %>% 
  summarise(female_median_2012 = median(females_2012), female_median_2002 = median(females_2002)) %>%
  mutate(female_median_increase = (female_median_2012 - female_median_2002)) %>%
  filter(female_median_increase == max(female_median_increase)) %>%
  select(state, female_median_increase)
print(median_states_grouped)

#################
#### PART 3 #####
#################







#################
#### PART 4 #####
#################

# Your script for Part 4 goes here (and delete this comment!)




