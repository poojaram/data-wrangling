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
largest_median_male_increase <- county_drinking %>% 
  group_by(state) %>% summarize(median = median(males_diff)) %>%
  filter(median == max(median))
print(largest_median_male_increase)

#States with the largest median increase in female binge drinking
largest_median_female_increase <- county_drinking %>% 
  filter(females_2002 < females_2012, males_2002 > males_2012) %>%
  group_by(state) %>% summarize(median_females = median(females_diff), median_both_sexes = median(both_sexes_diff)) %>% 
  filter(median_females == max(median_females))
print(largest_median_female_increase)

#################
#### PART 3 #####
#################

# adds the "binge_" to column names in binge.drinking
colnames(binge_drinking)[3:35] <- paste0("binge_", colnames(binge_drinking)[3:35])
# adds the "any_" to column names in alc.consumption
colnames(alcohol_consumption)[3:35] <- paste0("any_", colnames(alcohol_consumption)[3:35])

# joinds the two data frame
all_drinking <- full_join(binge_drinking, alcohol_consumption, by = c("location", "state"))

# Adding a non_binge_2012 column
all_drinking <- mutate(all_drinking, non_binge_2012 = any_both_sexes_2012 - binge_both_sexes_2012)

# Prints the average rate of drinking that was not binge drinking in 2012
filter(all_drinking, location != state, location != "United States") %>%
  summarise(mean = mean(non_binge_2012))

# Filters location and finds the minimum for non_binge_2012
# Prints a data frame with the name of the state along with its 'any drinking' rate, 'binge drinking' rate
# and the difference between them
filter(all_drinking, location == state) %>%
  filter(non_binge_2012 == min(non_binge_2012)) %>%
  select(state, any_both_sexes_2012, binge_both_sexes_2012, non_binge_2012)

# Filters based on county-level, state with the smallest amount of non-binge drinking
# Prints a data frame with the name of the state along with its 'any drinking' rate, 'binge drinking' rate
# and the difference between them



# State that was binge drinking the largest percentage of all drinking


# Challenge Problem
# This function writes a new data frame to csv file 
# It takes in state and year as parameters and return data frame of drinking in that state during that year
ExportStateYear <- function(input_state, input_year) {
  input_year <- toString(input_year)
  filter(all_drinking, state == input_state) %>%
    select(location, state, contains(input_year)) %>%
    arrange_(paste0("any_both_sexes_", input_year)) %>%
    write.csv(file = paste0("data/drinking_", input_state, input_year,".csv"))
}
ExportStateYear("Washington", 2011)
ExportStateYear("Florida", 2007)

#################
#### PART 4 #####
#################

# This function taken in state, gender, year and type of drinking data as inputs and returns
# the exact data for year, state and type of drinking
# Creates the search term by pasting the type and year as string together, and then it
# filters the state and removes the counties, and selects the final required data.

ExportSpecifics <- function(input_state, input_sex, input_year, input_type) {
  input_year <- toString(input_year)
  term <- paste0(input_sex, "_", input_year)
  check_for <- paste0(input_type,"_",term) 
  data_to_return <- filter(all_drinking, input_state == state, input_state == location) %>%  
  select(state, check_for) 
  data_to_return
}

run1 <- ExportSpecifics("New York", "males", 2007, "binge")
print(run1)

run2 <- ExportSpecifics("Virginia", "females", 2005, "any")
print(run2)

