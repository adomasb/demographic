#############################
### COOKIES AND URLS MAPS ###
#############################

# reads parsed files, drops not frequent items and saves url and cookie map
# cookieLimit - how many domains cookie should be visited at least
# urlLimit - how many cookies should have been in domains
mapTrain <- function(path, segment, numberOfFiles, cookieLimit, urlLimit){
  
  if (segment == "gender"){
    # bad cookies list, where gender is NA
    badCookies <- fread(paste0(path, "cookiesParsed.csv"), 
                        select = c("cookieID", "gender"),
                        colClasses = rep("character",18))
    badCookies <- badCookies[gender=="", cookieID]
  } else {
    # bad cookies list, where income is NA
    badCookies <- fread(paste0(path, "cookiesParsed.csv"), 
                        select = c("cookieID", "income"),
                        colClasses = rep("character",18))
    badCookies <- badCookies[income=="", cookieID]
  }
  
  # data files
  files <- dir(path, full.names=TRUE,ignore.case = T)
  files <- files[grepl("cookie_history",files)]
  files <- files[grepl("parsed_train", files)]
  if (numberOfFiles <= length(files)){
    files <- files[1:numberOfFiles]
  }

  
  # tables for visits
  cookies <- data.table()
  url <- data.table()
  
  for (f in 1:length(files)){
    dataFile <- fread(files[f], colClasses = rep("character", 3), 
                      drop = "timestamp")
    
    # filter badCookies if any
    dataFile <- dataFile[!(cookieID %in% badCookies)]
    
    # number of visits in url
    url_visits <- group_by(dataFile, URL) %>%
      summarise(visits = n())
    
    url <- rbindlist(list(url, url_visits))
    
    # number of clicks of cookies
    cookie_visits <- group_by(dataFile, cookieID) %>%
      summarise(visits = n())
    
    cookies <- rbindlist(list(cookies, cookie_visits))
  }
  
  # each cookie number of visits in different domains
  # filtering cookies which visited less than 20 urls
  cookies <- group_by(cookies, cookieID) %>%
    summarise(visits = sum(visits)) %>%
    arrange(-visits) %>%
    filter(visits > cookieLimit)
  cookies[, cID:=1:nrow(cookies)]
  
  # each url different number of visits
  url <- group_by(url, URL) %>%
    summarise(visits = sum(visits)) %>%
    arrange(-visits) %>%
    filter(visits > urlLimit)
  url[, uID:=1:nrow(url)]
  
  # to csv
  if (segment == "gender"){
    write.table(select(cookies, -visits), file = paste0(path, "cookiesGenderTrainMap.csv"), row.names = F)
    write.table(select(url, -visits), file = paste0(path, "urlGenderMap.csv"), row.names = F)
  } else {
    write.table(select(cookies, -visits), file = paste0(path, "cookiesIncomeTrainMap.csv"), row.names = F)
    write.table(select(url, -visits), file = paste0(path, "urlIncomeMap.csv"), row.names = F)
  } 
}

# maps test set
mapTest <- function(path, segment, numberOfFiles, cookieLimit){
  if (segment == "gender"){
    # bad cookies list, where gender is NA & those from cookieTrainMap
    badCookies <- fread(paste0(path, "cookiesParsed.csv"), 
                        select = c("cookieID", "gender"),
                        colClasses = rep("character", 18))
    trainCookies <- fread(paste0(path, "cookiesGenderTrainMap.csv"))
    badCookies <- c(badCookies[gender=="", cookieID], trainCookies[, cookieID])
  } else {
    # bad cookies list, where income is NA & those from cookieTrainMap
    badCookies <- fread(paste0(path, "cookiesParsed.csv"), 
                        select = c("cookieID", "income"),
                        colClasses = rep("character", 18))
    trainCookies <- fread(paste0(path, "cookiesIncomeTrainMap.csv"))
    badCookies <- c(badCookies[income=="", cookieID], trainCookies[, cookieID])
  }
  
  # data files
  files <- dir(path, full.names=TRUE,ignore.case = T)
  files <- files[grepl("cookie_history",files)]
  files <- files[grepl("parsed_test", files)]
  if (numberOfFiles <= length(files)){
    files <- files[1:numberOfFiles]
  }
  
  # known url map
  urlMap <- fread(paste0(path, "urlGenderMap.csv"))
  
  # tables for visits
  cookies <- data.table()
  
  for (f in 1:length(files)){
    dataFile <- fread(files[f], colClasses = rep("character", 3), 
                      drop = "timestamp")
    
    # filter badCookies if any
    dataFile <- dataFile[!(cookieID %in% badCookies)]
    
    # filter those url who didnt appear in training
    dataFile <- dataFile[URL %in% urlMap[, URL]]
    
    # number of visits of cookies
    cookie_visits <- group_by(dataFile, cookieID) %>%
      summarise(visits = n())
    
    cookies <- rbindlist(list(cookies, cookie_visits))
  }
  
  # each cookie number of visits in different domains
  # filtering cookies which visited less than 20 urls
  cookies <- group_by(cookies, cookieID) %>%
    summarise(visits = sum(visits)) %>%
    arrange(-visits) %>%
    filter(visits > cookieLimit)
  cookies[, cID:=1:nrow(cookies)]
  
  if (segment == "gender"){
    # to csv
    write.table(select(cookies, -visits), file = paste0(path, "cookiesGenderTestMap.csv"), row.names = F) 
  } else {
    write.table(select(cookies, -visits), file = paste0(path, "cookiesIncomeTestMap.csv"), row.names = F) 
  }
}

######################
#### TESTING MAPS ####
######################

path <- "/home/adomas/Learning/logistic/"

## Gender ##

testingGenderMap <- function(path, segment, numberOfFiles, cookieLimit, urlLimit){
  # create maps
  mapTrain(path, segment, numberOfFiles, cookieLimit, urlLimit)
  mapTest(path, segment, numberOfFiles, cookieLimit)
  # maps
  cookiesGenderTrain <- fread(paste0(path, "cookiesGenderTrainMap.csv")) %>% setkey(cID)
  cookiesGenderTest <- fread(paste0(path, "cookiesGenderTestMap.csv")) %>% setkey(cID)
  # train/test data
  cookiesModelTrain <- cookiesDataGender(path, "train") %>% setkey(cID)
  cookiesModelTest <- cookiesDataGender(path, "test") %>% setkey(cID)
  # original data
  cookies <- fread(paste0(path, "cookiesParsed.csv"), colClasses = rep("character", 18)) %>%
    select(cookieID, gender) %>%
    mutate(genderOriginal = ifelse(gender == "gender_female", 1, 0)) %>% 
    select(-gender) %>%
    setkey(cookieID)
  # setting keys & joining
  cookiesGenderTrain <-cookiesGenderTrain[cookiesModelTrain] %>% setkey(cookieID) 
  cookiesGenderTest <-cookiesGenderTest[cookiesModelTest] %>% setkey(cookieID)
  cookiesGenderTrain  <- cookies[cookiesGenderTrain]
  cookiesGenderTest  <- cookies[cookiesGenderTest]
  if(all(cookiesGenderTrain[,genderOriginal] == cookiesGenderTrain[,gender],
         cookiesGenderTest[,genderOriginal] == cookiesGenderTest[,gender])){
    print("Good")
  } else {
    print("We have a bug!")
  }
}

segment <- "income"
income_level <- "income_low"
numberOfFiles <- 2 
cookieLimit <- 25 
urlLimit <- 25


## income ##
testingIncomeMap <- function(path, segment, income_level, numberOfFiles, cookieLimit, urlLimit){
  # create maps
  mapTrain(path, segment, numberOfFiles, cookieLimit, urlLimit)
  mapTest(path, segment, numberOfFiles, cookieLimit)
  # maps
  cookiesIncomeTrain <- fread(paste0(path, "cookiesIncomeTrainMap.csv")) %>% setkey(cID)
  cookiesIncomeTest <- fread(paste0(path, "cookiesIncomeTestMap.csv")) %>% setkey(cID)
  # train/test data
  cookiesModelTrain <- cookiesDataIncome(path, "train", income_level) %>% setkey(cID)
  cookiesModelTest <- cookiesDataIncome(path, "test", income_level) %>% setkey(cID)
  # original data
  cookies <- fread(paste0(path, "cookiesParsed.csv"), colClasses = rep("character", 18)) %>%
    select(cookieID, income) %>%
    mutate(incomeOriginal = ifelse(income == income_level, 1, 0)) %>% 
    select(-income) %>%
    setkey(cookieID)
  # setting keys & joining
  cookiesIncomeTrain <-cookiesIncomeTrain[cookiesModelTrain] %>% setkey(cookieID) 
  cookiesIncomeTest <-cookiesIncomeTest[cookiesModelTest] %>% setkey(cookieID)
  cookiesIncomeTrain  <- cookies[cookiesIncomeTrain]
  cookiesIncomeTest  <- cookies[cookiesIncomeTest]
  if(all(cookiesIncomeTrain[,incomeOriginal] == cookiesIncomeTrain[,income],
         cookiesIncomeTest[,incomeOriginal] == cookiesIncomeTest[,income])){
    print("Good")
  } else {
    print("We have a bug!")
  }
}