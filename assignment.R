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

#





#################
#### PART 2 #####
#################

# Your script for Part 2 goes here (and delete this comment!)





#################
#### PART 3 #####
#################

# Your script for Part 3 goes here (and delete this comment!)





#################
#### PART 4 #####
#################

# Your script for Part 4 goes here (and delete this comment!)




