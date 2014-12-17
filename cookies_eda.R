library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)
library(stringr)
library(scales)

# read
cookies <- fread("/home/adomas/Learning/logistic/cookiesParsed.csv",
                 colClasses = rep("character", 18))

# all cookieIDs unique?
identical(cookies[, length(unique(cookieID))], nrow(cookies))

# percentages of empty cells
sapply(cookies, function(x) mean(nchar(x)==0))

# mostly interesting columns
cookies <- select(cookies, cookieID, age, children, education,
                  employ, gender, household, income, device_family,
                  os_family, ua_family)

# change empty cells to NAs
for (i in names(cookies)){
  cookies[nchar(get(i))==0, i:=NA, with=FALSE]
}

##############################
## each segments bar plots ###
##############################

# age bars
ggplot(cookies, aes(age, fill=age))+geom_bar()

# children bars
ggplot(cookies, aes(children, fill=children))+geom_bar()

# education bars
ggplot(cookies, aes(education, fill=education))+geom_bar()

# employment bars
ggplot(cookies, aes(employ, fill=employ))+geom_bar()

# gender bars
ggplot(cookies, aes(gender, fill=gender))+geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= percent)+xlab("Gender")+ylab("Segment size, %")
ggsave("/home/adomas/Dropbox/browsing\ behavior/desc/gender_bar.png",width = 6, height =4)

# house bars
ggplot(cookies, aes(household, fill=household))+geom_bar()

# inc bars
ggplot(cookies, aes(income, fill=income))+geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= percent)+xlab("Income level")+ylab("Segment size, %")
ggsave("/home/adomas/Dropbox/browsing\ behavior/desc/income_bar.png",width = 6, height =4)


###########################
# inc, age, gender facets #
###########################

ggplot(cookies, aes(gender, fill=gender))+geom_bar()+
  facet_grid(inc~age, scales="free")

ggplot(cookies, aes(income, fill=income))+geom_bar()+
  facet_grid(gender~age, scales="free")

ggplot(cookies, aes(age, fill=age))+geom_bar()+
  facet_grid(gender~inc, scales="free")

###################
## by user agent ##
###################

# device
sort(table(cookies[, device_family]))
cookies <- mutate(cookies, device_family = ifelse(device_family == "Other",
                                                  "Other", "NotOther"))

ggplot(cookies, aes(gender))+geom_bar()+facet_grid(device_family~., scales="free")

# os
sort(table(cookies[, os_family]))
cookies <- cookies[os_family %in% cookies[, unique(os_family)][grep("Win", cookies[, unique(os_family)])],
                   os_family:= "Windows"]
cookies <- cookies[os_family %in% c("iOS", "Mac OS X"),
                   os_family:= "iOS/OS X"]
cookies <- cookies[!(os_family %in% c("Windows", "iOS/OS X")),
                   os_family:= "Other"]

ggplot(cookies, aes(os_family))+geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= percent)

ggplot(cookies, aes(gender))+geom_bar()+facet_grid(os_family~., scales="free")

# browser
sort(table(cookies[, ua_family]))

cookies <- cookies[ua_family %in% c("Chromium", "Chrome Frame", "Chrome", "Chrome Mobile"),
                   ua_family:="Chrome"]
cookies <- cookies[!(ua_family %in% c("Chrome", "IE", "Firefox", "Safari")),
                     ua_family:="Other"]

ggplot(cookies, aes(ua_family))+geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= percent)+facet_grid(gender~., scales="free")

ggplot(cookies, aes(ua_family))+geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels= percent)+facet_grid(income~., scales="free")
