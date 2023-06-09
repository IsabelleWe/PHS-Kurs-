#Assignment
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

dat <- read_csv("insurance_with_date.csv")
str(dat)

dataselect <- dat |>
  select(sex, bmi, smoker, charges, region) |>
  mutate(sex = factor(sex, levels=c("male", "female"), labels=c("m","f")))

#Descriptive Statistics

#Boxplot 1: Smokers vs. Non-smokers - Who has more charges?
ggplot(dataselect, aes(x=smoker, y=charges)) + 
  geom_boxplot() 

#Boxplot sex vs. charges
ggplot(dataselect, aes(x=sex, y=charges)) + 
  geom_boxplot() 

#bmi vs charges 
ggplot(dataselect, aes(x=bmi, y=charges, color=smoker)) +
  geom_point()


