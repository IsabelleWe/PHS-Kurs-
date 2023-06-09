#R-Studio Einführung 
id<-c(1,2,3,4,5)
id<-seq(1,5,1)

names<-c("Mark","Jack","Jill","Anna","Tom")
gender<-c(0,0,1,1,0)

a<-1:5
b<-6:10

matc<-cbind(a,b)
matr<-rbind(a,b)

dat<-data.frame(ID=id,Name=names,Gender=gender)

dat[1,]
dat[2,]
dat[,3]
names[3]

dat[,c("ID","Gender")]
dat$ID
dat$Gender

sel<-dat$Gender == 1

dat[sel,]


#Open data
#Set Working directory in Session
data<-read.csv("perulung_ems.csv")

#structure data
str(data)
#show me the first 10
head(data,10)
#show me the last 10
tail(data,10)
#show all data
View(data)
#summarize data
summary(data)

#defining sex as binary numbers
data$sex_f <- factor(data$sex,levels=c(0,1),labels=c("f","m"))
head(data)
table(data$sex)
summary(data$sex_f)

library(tidyverse)

data %>% select(sex, sex_f) %>% filter(sex_f=="m") %>% table



