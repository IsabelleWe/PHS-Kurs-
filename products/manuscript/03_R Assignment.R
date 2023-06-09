#Assignment
install.packages("tidyverse")
install.packages("gtsummary")
library(tidyverse)
library(ggplot2)
library(gtsummary)

dat <- read_csv("data/raw/insurance_with_date.csv")
str(dat)

dataselect <- dat |>
  select(sex, bmi, smoker, charges, region) |>
  mutate(sex = factor(sex, levels=c("male", "female"), labels=c("m","f")),
         smoker = factor(smoker))


#Descriptive Statistics

tabl_summary <- dat |>   
  tbl_summary(
    by = smoker,
    type = all_continuous() ~ "continuous2",
    include =c("charges","bmi","age"),
    statistic = all_continuous() ~ c("{mean} ({sd})", 
                                     "{median} ({p25}, {p75})", 
                                     "{min}, {max}")
)
tabl_summary

#Boxplot 1: Smokers vs. Non-Smokers - Who has more charges?
ggplot(dataselect, aes(x=smoker, y=charges, fill = smoker)) + 
  geom_boxplot() +
  ggtitle(label = "Smokers vs. Non-Smokers") +
  xlab(label = "") +
  ylab(label = "Charges from insurance") +
  theme_bw() + theme(legend.position="bottom") 

#Boxplot 2: sex vs. charges
ggplot(dataselect, aes(x=sex, y=charges)) + 
  geom_boxplot() 

#Boxplot 3: bmi vs charges 
ggplot(dataselect, aes(x=bmi, y=charges, color=smoker)) +
  geom_point()

#histogram of charges 
ggplot(dat, aes(x=charges)) + 
  geom_histogram(bins=50)

#Testing normality for smoker 
dat %>%
  ggplot(aes(sample = charges, fill = smoker, colour = smoker)) +
  geom_qq_line(distribution =  stats::qnorm) +
  geom_qq(distribution =  stats::qnorm) +
  xlab("theoretical") +
  ylab("sample") +
  theme_classic() +
  facet_wrap("sex")

#Charges log
  dat$chargeslog <- log(dat$charges)
chargeslog

#histogram of charges 
ggplot(dat, aes(x=chargeslog, fill = smoker)) + 
  geom_histogram(bins=50) +
  theme_bw() + theme(legend.position="bottom") 

#Testing normality for smoker 
dat %>%
  ggplot(aes(sample = chargeslog, fill = smoker, colour = smoker)) +
  geom_qq_line(distribution =  stats::qnorm) +
  geom_qq(distribution =  stats::qnorm) +
  xlab("theoretical") +
  ylab("sample") +
  theme_classic() +
  facet_wrap("sex")

#Ttest charges smoker vs. non-smokers - does smoking influence the insurance charges?

t.test(chargeslog ~ smoker, data = dat)


