##Install packages
install.packages("dplyr") ## Installing package dplyr

##Load libraries
library(dplyr) ## Loading package dplyr
library(tidyverse)

##Set work Directory
setwd("/Users/andrew.kim.24/Documents/Rfolder/IJASproject")

kagglePasswordSet <- read.csv("dataclean.csv", stringsAsFactors = FALSE) ## Reading in cleaned data set
kagglePasswordSubset <- kagglePasswordSet %>% filter(strength %in% c(0,2)) ## Filtering out passwords with a value of 1
kagglePasswordSubsetUnique <-kagglePasswordSubset[!duplicated(kagglePasswordSubset),] ## Creating a new list without duplicated passwords 

##Calculate features from passwords (See Table)
passwordLength <- sapply(kagglePasswordSubsetUnique$password, nchar) #total password length
lowerCaseLength <- sapply(kagglePasswordSubsetUnique$password, function(x) str_count(x, "[:lower:]")) #number of lower case characters
upperCaseLength <- sapply(kagglePasswordSubsetUnique$password, function(x) str_count(x, "[:upper:]")) #number of upper case characters
digitLength <- sapply(kagglePasswordSubsetUnique$password, function(x) str_count(x, "[0-9]")) #number of digits
specialCharLength <- sapply(kagglePasswordSubsetUnique$password, function(x) str_count(x, "[:punct:]")) #number of special characters
blankCharLength <- sapply(kagglePasswordSubsetUnique$password, function(x) str_count(x, "[:space:]")) #number of space/blank characters

uniqueCharInString <- function(inputString) {
  stringSplit <- unlist(strsplit(inputString, "")) #split string into individual characters; change to vector
  numChar <- length(unique(stringSplit)) #create vector of unique characters; calculate length
  return(numChar)}
numUniqueChar <- sapply(kagglePasswordSubsetUnique$password, function(x) uniqueCharInString(x))

getLongestRepeatedSubstring("aa")



##Construct dataframe from feature vectors
passwordFeatures.df <- data.frame(password = kagglePasswordSubsetUnique$password,
                                  passwordLength,
                                  lowerCaseLength,
                                  upperCaseLength,
                                  digitLength,
                                  specialCharLength,
                                  blankCharLength,
                                  numUniqueChar) 
rownames(passwordFeatures.df) <- NULL #Remove rownames (change to numbers)
save(passwordFeatures.df, file = "passwordFeatures.RData") #Save dataframe (as .csv, .RData) 


##Exploratory Data Analysis
load("passwordFeatures.RData")

dim(kagglePasswordSubset) ## Finding out the dimensions of the dataframe
tail(kagglePasswordSet) ## Outputting the last few parts of the list
head(kagglePasswordSet) ## Outputting the first few parts of the list
