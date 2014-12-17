library(data.table)
library(dplyr)
library(doMC)
library(glmnet)
library(ROCR)
library(SparseM)
# load functions from functions.R
source("/home/adomas/Learning/demography/functions.R")

################
### TRAINING ###
################

# a little bit of copy/paste...
# ok ok could be coded more efficiently and etc

###
### Gender
###

# 25 clicks
gender2_25 <- modelling(path = "/home/adomas/Learning/logistic/",
                        segment = "gender",
                        level = "none",
                        numberOfFiles = 4,
                        cookieLimit = 100,
                        urlLimit = 25)
save(gender2_25, file="/home/adomas/Dropbox/models/gender2_25.rda")
# 50 clicks
gender2_50 <- modelling(path = "/home/adomas/Learning/logistic/",
                       segment = "gender",
                       level = "none",
                       numberOfFiles = 4,
                       cookieLimit = 100,
                       urlLimit = 50)
save(gender2_50, file="/home/adomas/Dropbox/models/gender2_50.rda")
# 100 clicks
gender2_100 <- modelling(path = "/home/adomas/Learning/logistic/",
                       segment = "gender",
                       level = "none",
                       numberOfFiles = 4,
                       cookieLimit = 100,
                       urlLimit = 100)
save(gender2_100, file="/home/adomas/Dropbox/models/gender2_100.rda")
# 150 clicks
gender2_150 <- modelling(path = "/home/adomas/Learning/logistic/",
                        segment = "gender",
                        level = "none",
                        numberOfFiles = 4,
                        cookieLimit = 100,
                        urlLimit = 150)
save(gender2_150, file="/home/adomas/Dropbox/models/gender2_150.rda")
# 250 clicks
gender2_250 <- modelling(path = "/home/adomas/Learning/logistic/",
                        segment = "gender",
                        level = "none",
                        numberOfFiles = 4,
                        cookieLimit = 100,
                        urlLimit = 250)
save(gender2_250, file="/home/adomas/Dropbox/models/gender2_250.rda")



###
### INCOME LOW
###

# 25 clicks
income_low2_25 <- modelling(path = "/home/adomas/Learning/logistic/",
                       segment = "income",
                       level = "income_low",
                       numberOfFiles = 4,
                       cookieLimit = 100,
                       urlLimit = 25)
save(income_low2_25, file="/home/adomas/Dropbox/models/income_low2_25.rda")
# 50 clicks
income_low2_50 <- modelling(path = "/home/adomas/Learning/logistic/",
                       segment = "income",
                       level = "income_low",
                       numberOfFiles = 4,
                       cookieLimit = 100,
                       urlLimit = 50)
save(income_low2_50, file="/home/adomas/Dropbox/models/income_low2_50.rda")
# 100 clicks
income_low2_100 <- modelling(path = "/home/adomas/Learning/logistic/",
                        segment = "income",
                        level = "income_low",
                        numberOfFiles = 4,
                        cookieLimit = 100,
                        urlLimit = 100)
save(income_low2_100, file="/home/adomas/Dropbox/models/income_low2_100.rda")
# 150 clicks
income_low2_150 <- modelling(path = "/home/adomas/Learning/logistic/",
                        segment = "income",
                        level = "income_low",
                        numberOfFiles = 4,
                        cookieLimit = 100,
                        urlLimit = 150)
save(income_low2_150, file="/home/adomas/Dropbox/models/income_low2_150.rda")
# 250 clicks
income_low2_250 <- modelling(path = "/home/adomas/Learning/logistic/",
                        segment = "income",
                        level = "income_low",
                        numberOfFiles = 4,
                        cookieLimit = 100,
                        urlLimit = 250)
save(income_low2_250, file="/home/adomas/Dropbox/models/income_low2_250.rda")

###
### INCOME MEDIUM
###

# 25 clicks
income_medium2_25 <- modelling(path = "/home/adomas/Learning/logistic/",
                            segment = "income",
                            level = "income_medium",
                            numberOfFiles = 4,
                            cookieLimit = 100,
                            urlLimit = 25)
save(income_medium2_25, file="/home/adomas/Dropbox/models/income_medium2_25.rda")
# 50 clicks
income_medium2_50 <- modelling(path = "/home/adomas/Learning/logistic/",
                            segment = "income",
                            level = "income_medium",
                            numberOfFiles = 4,
                            cookieLimit = 100,
                            urlLimit = 50)
save(income_medium2_50, file="/home/adomas/Dropbox/models/income_medium2_50.rda")
# 100 clicks
income_medium2_100 <- modelling(path = "/home/adomas/Learning/logistic/",
                             segment = "income",
                             level = "income_medium",
                             numberOfFiles = 4,
                             cookieLimit = 100,
                             urlLimit = 100)
save(income_medium2_100, file="/home/adomas/Dropbox/models/income_medium2_100.rda")
# 150 clicks
income_medium2_150 <- modelling(path = "/home/adomas/Learning/logistic/",
                             segment = "income",
                             level = "income_medium",
                             numberOfFiles = 4,
                             cookieLimit = 100,
                             urlLimit = 150)
save(income_medium2_150, file="/home/adomas/Dropbox/models/income_medium2_150.rda")
# 250 clicks
income_medium2_250 <- modelling(path = "/home/adomas/Learning/logistic/",
                             segment = "income",
                             level = "income_medium",
                             numberOfFiles = 4,
                             cookieLimit = 100,
                             urlLimit = 250)
save(income_medium2_250, file="/home/adomas/Dropbox/models/income_medium2_250.rda")

###
### INCOME HIG
###

# 25 clicks
income_high2_25 <- modelling(path = "/home/adomas/Learning/logistic/",
                               segment = "income",
                               level = "income_high",
                               numberOfFiles = 4,
                               cookieLimit = 100,
                               urlLimit = 25)
save(income_high2_25, file="/home/adomas/Dropbox/models/income_high2_25.rda")
# 50 clicks
income_high2_50 <- modelling(path = "/home/adomas/Learning/logistic/",
                               segment = "income",
                               level = "income_high",
                               numberOfFiles = 4,
                               cookieLimit = 100,
                               urlLimit = 50)
save(income_high2_50, file="/home/adomas/Dropbox/models/income_high2_50.rda")
# 100 clicks
income_high2_100 <- modelling(path = "/home/adomas/Learning/logistic/",
                                segment = "income",
                                level = "income_high",
                                numberOfFiles = 4,
                                cookieLimit = 100,
                                urlLimit = 100)
save(income_high2_100, file="/home/adomas/Dropbox/models/income_high2_100.rda")
# 150 clicks
income_high2_150 <- modelling(path = "/home/adomas/Learning/logistic/",
                                segment = "income",
                                level = "income_high",
                                numberOfFiles = 4,
                                cookieLimit = 100,
                                urlLimit = 150)
save(income_high2_150, file="/home/adomas/Dropbox/models/income_high2_150.rda")
# 250 clicks
income_high2_250 <- modelling(path = "/home/adomas/Learning/logistic/",
                                segment = "income",
                                level = "income_high",
                                numberOfFiles = 4,
                                cookieLimit = 100,
                                urlLimit = 250)
save(income_high2_250, file="/home/adomas/Dropbox/models/income_high2_250.rda")
