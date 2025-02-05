#Data Analysis

library(tidyverse)


#grab the data for our analysis
sample_data <- read_csv("data/sample_data.csv")
glimpse(sample_data)


#Summarize
summarize(sample_data, avg_cells = mean(cells_per_ml))

#Syntax/style
sample_data %>% 
  # Group the data by environmental group
  group_by(env_group) %>%
  # Calculate the mean
  summarize(avg_cells = mean(cells_per_ml))

