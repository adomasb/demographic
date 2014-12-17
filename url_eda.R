library(data.table)
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)
library(Hmisc)
library(scales)

## read train files only
history <- function(path){
  files <- dir(path, full.names=TRUE,ignore.case = T)
  files <- files[grepl("cookie_history",files)]
  files <- files[grepl("parsed_train", files)]
  history <- data.table()
  for (f in 1:length(files)){
    file <- fread(files[f], colClasses = rep("character", 3))
    history <- rbindlist(list(history, file))
  }
  return(history)
}
history <- history("/home/adomas/Learning/logistic/")

##########
## URLS ##
##########

# unique pages
length(history[, unique(URL)])

# top pages
sort(table(history[, URL]), decreasing=TRUE)[1:20]

# 1, 5, 10, 25, 50, 75, 95 & 99 percentiles of URLs visits
group_by(history, URL) %>%
  summarise(visits = n()) %>%
  summarise(quantile(visits, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95, 0.99)))

# how many pages are visited less than 10 times
group_by(history, URL) %>%
  summarise(visits = n()) %>%
  filter(visits >= 12000) %>%
  summarise(sum(visits))
  nrow

# how many pages are visited once
group_by(history, URL) %>%
  summarise(visits = n()) %>%
  filter(visits == 1) %>%
  nrow

# average visits per page
group_by(history, URL) %>%
  summarise(visits = n()) %>%
  summarise(mean(visits))

# visits histogram
group_by(history, URL) %>%
  summarise(visits = n()) %>%
  ggplot(aes(visits))+geom_histogram()

#
visitsGroups <- group_by(history, URL) %>%
  summarise(visits = n()) %>%
  mutate(group = cut2(visits, cuts = c(1, 10, 20, 30, 40, 50)))

ggplot(visitsGroups, aes(group))+geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= percent)


#############
## cookies ##
#############

# unique cookies
length(history[, unique(cookieID)])

# 1, 5, 10, 25, 50, 75, 95 & 99 percentiles of users visits
group_by(history, cookieID) %>%
  summarise(visits = n()) %>%
  summarise(quantile(visits, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95, 0.99)))

# user visits distribution
group_by(history, cookieID) %>%
  summarise(visits = n()) %>%
  ggplot(aes(visits))+geom_histogram(binwidth=10)

# how many cookies did only one visit
group_by(history, cookieID) %>%
  summarise(visits = n()) %>%
  filter(visits == 1) %>%
  nrow

# average visits by user
group_by(history, cookieID) %>%
  summarise(visits = n()) %>%
  summarise(mean(visits))

# 
clickGroups <- group_by(history, cookieID) %>%
  summarise(visits = n()) %>%
  mutate(Clicks = cut2(visits, cuts = c(1, 50, 100, 150,200, 250)))

ggplot(clickGroups, aes(Clicks, fill=Clicks))+geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= percent)+xlab("Clicks")+ylab("Cookies, %")
ggsave("/home/adomas/Dropbox/browsing\ behavior/desc/cookieVisits_bar.png",width = 6, height =4)

##########
## TIME ##
##########

# adding hour column
history <- mutate(history, hour = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M", tz="GMT")) %>%
  mutate(hour = as.POSIXlt(hour)$hour)

ggplot(history, aes(factor(hour), fill=factor(hour)))+geom_bar()

ggplot(cookies, aes(factor(hour), fill=factor(hour)))+geom_bar()+facet_grid(gender~.)
ggplot(cookies, aes(factor(hour), fill=factor(hour)))+geom_bar()+facet_grid(age~.)
