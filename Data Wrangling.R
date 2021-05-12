#Data Wrangling Part 1 ####

# install.packages("datasauRus")
# install.packages("gganimate")
# install.packages("ggplot2")
# install.packages("gifski")
# install.packages("purrr")
# install.packages("rmarkdown")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("magrittr")


library(datasauRus)
library(gganimate)
library(ggplot2)
library(gifski)
library(purrr)
library(rmarkdown)
library(tidyverse)
# part of the tidyverse:
library(dplyr)
library(magrittr)





#Data Wrangling Part 1 ####
 
#checking R version
version

#browse vignettes of a package
browseVignettes("tidyverse")

#load data sets directly from GitHub
library(readr)

urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"

participants_data <- read_csv(url(urlfile))

#load data sets from R project folder
read.csv("participants_data.csv")
participants_data <- read.csv("participants_data.csv")

#using head function to look into "x" rows of all colums
head(participants_data) #default: looking at the first 6 rows
head(participants_data, n = 4)  #looking at the first 4 rows

#using names function to look into all column names
names(participants_data)

#overview to the structure of a dataset
str(participants_data)



# wrangling dplyr ####
library(dplyr)
library(tidyr)
library(magrittr)

select(participants_data, 
       batch, 
       age) #select columns batch and age
select(participants_data, 
       -batch, 
       -age) #selection without batch and age


filter(participants_data, 
       working_hours_per_day > 5) #select those who work more than 5 hours per day

filter(participants_data, 
       working_hours_per_day > 5 &
         letters_in_first_name > 3  ) #select those who work mor than 5 hours per day and names are longer than 3 letters

rename(participants_data,
       commute = km_home_to_office) #rename variable km_home_to_office as commute

mutate(participants_data,
       age_mean = age*
                  mean(age))  # Mutate a new column named age_mean that is a function of the age multiplied by the mean of all ages in the group

mutate(participants_data, 
       response_speed = 
         ifelse(days_to_email_response > 1, 
                "slow", "fast"))   #Mutate new column named response_speed populated by 'slow' if it took you more than a day to answer my email and 'fast' for others

summarize(participants_data,
          mean(number_of_siblings),
          median(years_of_study))  # Create a summary of the participants_mutate data with the mean number of siblings and median years of study


#wrangling: magrittr use ####

# Use the magrittr pipe to summarize 
# the mean days to email response, 
# median letters in first name, 
# and maximum years of study by gender

participants_data %>%
  group_by(gender) %>%
  summarize(mean(days_to_email_response),
            median(letters_in_first_name),
            max(years_of_study))

# Use the magrittr pipe to create a new column 
# called commute, where those who travel 
# more than 10km to get to the office 
# are called "commuter" and others are "local". 
# Summarize the mean days to email response, 
# median letters in first name, 
# and maximum years of study.

participants_data %>%
  mutate(commute = ifelse(
    km_home_to_office > 10,
    "commuter", "local")) %>%
  group_by(commute) %>%
  summarize(mean(days_to_email_response),
            median(letters_in_first_name),
            max(years_of_study))






# purrr ####

library(purrr)

# Split the data frame by batch, 
# fit a linear model formula 
# (days to email response as dependent 
# and working hours as independent) 
# to each batch, compute the summary, 
# then extract the R^2.
participants_data %>%
  split(.$batch) %>%
  map(~
      lm(days_to_email_response ~ 
           working_hours_per_day,
            data = .)) %>%
  map(summary) %>% 
  map_dbl("r.squared")







# work on dataset "diamnonds" ####
library(ggplot2)
diamonds
names(diamonds)


diamonds %>%
  select(carat, price) %>%   #select: carat and price
  filter(carat > 0.5)%>%   #filter: only where carat is >0.5
  rename(cost = price) %>%   #rename: rename price as cost
  mutate(value = ifelse (
        cost > mean(cost),
        "expensive", "cheap")) %>%   #mutate: name expensive if greater than mean of price/cost or cheap otherwise
  group_by(value) %>%   #group_by: split into cheap and expensive
  summarize(max(cost),  
            mean(carat))  # summarize: give some summary statistics of your choice





