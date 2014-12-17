source("/home/adomas/Learning/demography/cookieAndURLmap.R")
############################
##### GENDER FUNCTIONS #####
############################

# reads cookies datafile and cookieMap
# returns gender and mapped cookieID
# type - train or test
# female is 1, male is 0
cookiesDataGender <- function(path, type){
  cookies <- fread(paste0(path, "cookiesParsed.csv"), colClasses = rep("character", 18))
  cookies <- select(cookies, cookieID, gender)
  if (type == "train"){
    cookiesMap <- fread(paste0(path, "cookiesGenderTrainMap.csv"), colClasses = c('character',"integer"))
  } else {
    cookiesMap <- fread(paste0(path, "cookiesGenderTestMap.csv"), colClasses = c("character", "integer"))
  }
  # set key
  setkey(cookiesMap, cookieID)
  setkey(cookies, cookieID)
  # join
  cookies <- cookies[cookiesMap] %>%
    select(-cookieID) %>%
    mutate(gender = ifelse(gender=="gender_female", 1, 0)) %>%
    arrange(cID)
  return(cookies)
}

# reads history files and historymap
# returns mapped cookieid and url and freq
# type - train or test
historyDataGender <- function(path, type, numberOfFiles){
  # data files
  files <- dir(path, full.names=TRUE,ignore.case = T)
  files <- files[grepl("cookie_history",files)]
  # maps
  if (type == "train"){
    files <- files[grepl("parsed_train", files)]
    cookiesMap <- fread(paste0(path, "cookiesGenderTrainMap.csv"), colClasses = c("character", "integer"))
  } else {
    files <- files[grepl("parsed_test", files)]
    cookiesMap <- fread(paste0(path, "cookiesGenderTestMap.csv"), colClasses = c("character", "integer"))
  }
  if (numberOfFiles <= length(files)){
    files <- files[1:numberOfFiles]
  }
  urlMap <- fread(paste0(path, "urlGenderMap.csv"), colClasses = c("character", "integer"))


  # data
  data <- data.table()

  for (f in 1:length(files)){
    dataFile <- fread(files[f], colClasses = rep("character", 3),
                     drop = "timestamp")

    # only cookies and urls from maps
    dataFile <- filter(dataFile, cookieID %in% cookiesMap[, cookieID])
    dataFile <- filter(dataFile, URL %in% urlMap[, URL])

    # each page freq for cookie
    dataFile <- group_by(dataFile, cookieID, URL) %>%
      summarise(visits = n()) %>%
      group_by(cookieID) %>%
      mutate(freq = round(visits/sum(visits), 4)) %>%
      select(-visits)

    data <- rbindlist(list(data, dataFile))
  }
  # mapping values
  setkey(data, cookieID)
  setkey(cookiesMap, cookieID)
  setkey(urlMap, URL)

  # joining
  data <- cookiesMap[data] %>% select(-cookieID) %>% setkey(URL)
  data <- urlMap[data] %>% select(-URL)

  return(data)
}

# creating sparse data matrix
# takes some time to create !!!!
dataMatrixGender <- function(path, type, numberOfFiles){
  gender <- cookiesDataGender(path, type)
  history <- historyDataGender(path, type, numberOfFiles)

  # features number based on training data
  if (type == "train"){
    features <- history[, max(uID)]
  } else {
    features <- historyDataGender(path, "train", numberOfFiles)[, max(uID)]
  }

  # sparse matrix
  M <- Matrix(0, nrow=gender[, max(cID)], ncol=features, sparse=TRUE)

  # filling matrix
  for (col in 1:history[, max(uID)]){
    M[history[uID == col][order(cID), cID], col] <- history[uID == col][order(cID), freq]
    if (col == round(history[, max(uID)]/2)){
      print('Half done')
    }
  }

  return(M)
}

############################
#### INCOME FUNCTIONS ######
############################

# reads cookies datafile and cookieMap
# depending on type (train/test) and level (high, medium, low)
# returns mapped cookieID and selected income level as 1
cookiesDataIncome <- function(path, type, income_level){
  cookies <- fread(paste0(path, "cookiesParsed.csv"), colClasses = rep("character", 18))
  cookies <- select(cookies, cookieID, income)
  if (type == "train"){
    cookiesMap <- fread(paste0(path, "cookiesIncomeTrainMap.csv"), colClasses = c('character',"integer"))
  } else {
    cookiesMap <- fread(paste0(path, "cookiesIncomeTestMap.csv"), colClasses = c("character", "integer"))
  }
  # set key
  setkey(cookiesMap, cookieID)
  setkey(cookies, cookieID)
  # join
  cookies <- cookies[cookiesMap] %>%
    select(-cookieID) %>%
    mutate(income = ifelse(income==income_level, 1, 0)) %>%
    arrange(cID)
  return(cookies)
}

# reads history files and historymap
# returns mapped cookieid and url and freq
# type - train or test
historyDataIncome <- function(path, type, numberOfFiles){
  # data files
  files <- dir(path, full.names=TRUE,ignore.case = T)
  files <- files[grepl("cookie_history",files)]
  if (type == "train"){
    files <- files[grepl("parsed_train", files)]
    cookiesMap <- fread(paste0(path, "cookiesIncomeTrainMap.csv"), colClasses = c("character", "integer"))
  } else {
    files <- files[grepl("parsed_test", files)]
    cookiesMap <- fread(paste0(path, "cookiesIncomeTestMap.csv"), colClasses = c("character", "integer"))

  }
  if (numberOfFiles <= length(files)){
    files <- files[1:numberOfFiles]
  }
  urlMap <- fread(paste0(path, "urlIncomeMap.csv"), colClasses = c("character", "integer"))
                  # data
  data <- data.table()

  for (f in 1:length(files)){
    dataFile <- fread(files[f], colClasses = rep("character", 3),
                      drop = "timestamp")

    # only cookies and urls from maps
    dataFile <- filter(dataFile, cookieID %in% cookiesMap[, cookieID])
    dataFile <- filter(dataFile, URL %in% urlMap[, URL])

    # each page freq for cookie
    dataFile <- group_by(dataFile, cookieID, URL) %>%
      summarise(visits = n()) %>%
      group_by(cookieID) %>%
      mutate(freq = round(visits/sum(visits), 4)) %>%
      select(-visits)

    data <- rbindlist(list(data, dataFile))
  }
  # mapping values
  setkey(data, cookieID)
  setkey(cookiesMap, cookieID)
  setkey(urlMap, URL)

  # joining
  data <- cookiesMap[data] %>% select(-cookieID) %>% setkey(URL)
  data <- urlMap[data] %>% select(-URL)

  return(data)
}

# creates sparse data matrix for testing or training
dataMatrixIncome <- function(path, type, income_level, numberOfFiles){
  gender <- cookiesDataIncome(path, type, income_level)
  history <- historyDataIncome(path, type, numberOfFiles)

  # features number based on training data
  if (type == "train"){
    features <- history[, max(uID)]
  } else {
    features <- historyDataIncome(path, "train", numberOfFiles)[, max(uID)]
  }

  # sparse matrix
  M <- Matrix(0, nrow=gender[, max(cID)], ncol=features, sparse=TRUE)

  # filling matrix
  for (col in 1:history[, max(uID)]){
    M[history[uID == col][order(cID), cID], col] <- history[uID == col][order(cID), freq]
    if (col == round(history[, max(uID)]/2)){
      print('Half done')
    }
  }

  return(M)
}

#############################
#### MODELLING FUNCTIONS ####
#############################

# huge and clumsy function
# function used for modelling, which wraps everything
# path - were all data is stored
# segment - gender/income
# level - income_low, income_medium, income_high/anything for gender
# numberOfFiles - how many cookie_history files to use for training and testing
# cookielimit - how many domains cookie should be visited to appear in train/test matrix
# urlLimit - how many cookies should have visited each domain at least to be included to train/test matrices
# logistic regression with 10-folds validation is evaluated
# lasso regression applied
modelling <- function(path, segment, level, numberOfFiles, cookieLimit, urlLimit){

  #### creatin' maps
  #### maps are created as .csv files
  #### maps contain raw url domains and cookie ids
  #### they are used when huge sparse matrices are created
  if (segment == "gender") {
    ## Gender analysis maps
    mapTrain(path, "gender", numberOfFiles, cookieLimit, urlLimit)
    mapTest(path, "gender", numberOfFiles, cookieLimit)
    ## train/test matrices
    M_train <- dataMatrixGender(path, "train", numberOfFiles)
    M_test <- dataMatrixGender(path, "test", numberOfFiles)
    # incomes
    y_train <- cookiesDataGender(path, "train")[, gender]
    y_test <- cookiesDataGender(path, "test")[, gender]
    print("Data prepared")
  } else {
    ## Income analysis maps
    mapTrain(path, "income", numberOfFiles, cookieLimit, urlLimit)
    mapTest(path, "income", numberOfFiles, cookieLimit)
    ## train/test matrices
    M_train <- dataMatrixIncome(path, "train", level, numberOfFiles)
    M_test <- dataMatrixIncome(path, "test", level, numberOfFiles)
    # genders
    y_train <- cookiesDataIncome(path, "train", level)[, income]
    y_test <- cookiesDataIncome(path, "test", level)[, income]
    print("Data prepared")
  }

  # same number of features?
  if (!identical(ncol(M_train), ncol(M_test))) {
    print("FEATURES ERROR!")
    break
  }

  # glmnet
  registerDoMC(cores=4)
  model <- cv.glmnet(x = M_train, y = y_train,
                          nfolds = 10, type.measure = "auc",
                          family = "binomial", alpha = 1, parallel=TRUE)
  # returns list for evaluation
  list <- list()
  list$model <- model
  list$M_train <- M_train
  list$M_test <- M_test
  list$y_train <- y_train
  list$y_test <- y_test
  return(list)
}

#####################
#### EVALUATION #####
#####################
