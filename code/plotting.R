#Plotting Lake Ontario Microbial Cell Abundances
#By: Hilal Samut
#Date: Jan 29, 2025

#Install pacakgaes first
install.packages("tidyverse")
library(tidyverse)


#Load in the data
sample_data = read_csv("sample_data.csv")

Sys.Date()
getwd()

round(3.1415)
round(3.1415,3)
round(3.1415, digits = 2)
round(digits = 2, 3.1415)
round(2, 3.1415)

#What does our data look like?
View(sample_data)
str(sample_data)

#Plotting 
ggplot(data = sample_data) +
  aes(x=temperature, y = cells_per_ml/1000000, 
      color=env_group, size = chlorophyll) +
  labs(x = "Temp (C)",y = "Cell abundance (millions cells/ml)", 
       color="Environmental Group", size = "Chlorophyll (ug/L)",
       title = "Does temperature affect microbial abundance?") +
  geom_point()





#BUOY DATA
buoy_data = read_csv("buoy_data.csv")
dim(buoy_data)
str(buoy_data)
glimpse(buoy_data)
unique(buoy_data$sensor)



#Plot buoy_data
ggplot(data=buoy_data) + 
  aes(x = day_of_year, y = temperature, group = sensor, color = depth) +
  geom_line()


#Facet plot
ggplot(data=buoy_data) + 
  aes(x = day_of_year, y = temperature, group = sensor, color = depth) +
  geom_line() +
  facet_wrap(~buoy)


ggplot(data=buoy_data) + 
  aes(x = day_of_year, y = temperature, group = sensor, color = depth) +
  geom_line() +
  facet_wrap(~buoy, scales = "free")


ggplot(data=buoy_data) + 
  aes(x = day_of_year, y = temperature, group = sensor, color = depth) +
  geom_line() +
  facet_grid(rows=vars(buoy))


#Cell abundances by environmental groups
ggplot(data=sample_data) +
  aes(x=env_group, y = cells_per_ml, 
      color =env_group, fill =env_group) +
  geom_jitter(aes(size = chlorophyll)) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  theme_classic()


ggsave("cells_per_envGroup.png", width = 6, height = 4)



##-------------Briefwork-Feb3--------
sample_data = read.csv("sample_data.csv")
library(tidyverse)

#cell abundances vs total nitrogen 
ggplot(data=sample_data) +
  aes(x=total_nitrogen, y= cells_per_ml/1000000,
      size=temperature, color=env_group) +
  labs(x="Total Nitrogen (mg/L)", y="Million cells per mL",
       size="Temperature (C)", color="Environmental group",
       title="Does total nitrogen concentration affect microbial abundance?") +
  geom_point() +
  geom_smooth(aes(group=1), method = "lm") 
ggsave("cells_per_nitrogen.png", width = 6, height = 4)

#cell abundances vs total phosphorus  
ggplot(data=sample_data) +
  aes(x=total_phosphorus, y= cells_per_ml/1000000,
      size=temperature, color=env_group) +
  labs(x="Total Phosphorus (mg/L)", y="Million cells per mL",
       size="Temperature (C)", color="Environmental group",
       title="Does total phosphorus concentration affect microbial abundance?") +
  geom_point() +
  geom_smooth(aes(group=1), method = "lm") 
ggsave("cells_per_phosphorus.png", width = 6, height = 4)


