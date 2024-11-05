##Load Libraries
library(dplyr)
library(tidyverse)
library(reshape2)
library(leaps)
library(glmnet)

##Set Working Directory
setwd("/Users/jihoonkim/Documents/IJAS")

##Load Dataset; Subset Strong/Weak password; Remove Duplicates
kagglePasswordSet <- read.csv("dataClean.csv", stringsAsFactors = FALSE)
kagglePasswordSubsetUnique <- kagglePasswordSet[!duplicated(kagglePasswordSet),]

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

longestRepeatedCharacter <- function(inputString) {
  stringSplit <- unlist(strsplit(inputString, "")) #split string into individual characters; change to vector
  charTable <- table(stringSplit) #create table of characters; frequencies
  maxNumRepeat <- max(charTable) #most frequently repeated characters
  return(maxNumRepeat)}
repeatCharMax <- sapply(kagglePasswordSubsetUnique$password, function(x) longestRepeatedCharacter(x))

##Construct dataframe from feature vectors
passwordFeatures.df <- data.frame(password = kagglePasswordSubsetUnique$password,
                                  strength = kagglePasswordSubsetUnique$strength,
                                  passwordLength,
                                  lowerCaseLength,
                                  upperCaseLength,
                                  digitLength,
                                  specialCharLength,
                                  blankCharLength,
                                  numUniqueChar,
                                  repeatCharMax)
rownames(passwordFeatures.df) <- NULL #Remove rownames (change to numbers)
save(passwordFeatures.df, file = "passwordFeatures.RData") #Save dataframe (as .csv, .RData) 

##Exploratory Data Analysis
load("passwordFeatures.RData")

#Plot correlation heatmap of features
passwordMelt <- melt(cor(passwordFeatures.df %>% select_if(is.numeric))) #calculate a correlation matrix of numeric features
pdf(file = "featureCorrelationHeatmap2.pdf", width = 10, height = 9) #create PDF
qplot(x = Var1,y = Var2, data = passwordMelt,fill = value, geom = "tile") + scale_fill_gradient(low="blue", high="red") +
  xlab("") + ylab("") + ggtitle("Feature Correlation Heatmap")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

passwordFeaturesOnly.df <- passwordFeatures.df[, -1] #remove first column (passwords); only features of passwords
passwordFeaturesOnlyUnique.df <- unique(passwordFeaturesOnly.df)

#Plot the relationship between features/strength
names(passwordFeaturesOnlyUnique.df) #get names of columns
featurePlot <- function(feature.df, feature, numMin, numMax){
  if(missing(numMin)){numMin <- 0} #if no user input set to 0
  if(missing(numMax)){numMax <- max(feature.df[, feature])} #if no user input set to maximum of feature slected
  ggplot(data = feature.df, (mapping = aes(x = !!ensym(feature), y = strength))) + geom_point(size = 0.7) + 
    ggtitle(paste(feature,  "  vs passwordStrength", sep = "")) + xlab(feature) + ylab("Password Strength") +
    xlim(numMin, numMax) + theme_bw() + theme(legend.position = "none")}

pdf(file = "passwordLength.pdf", width = 4, height = 4) #create PDF
featurePlot(passwordFeaturesOnlyUnique.df, "passwordLength", 0, 75)
dev.off()

pdf(file = "lowerCaseLength.pdf", width = 4, height = 4) #create PDF
featurePlot(passwordFeaturesOnlyUnique.df, "lowerCaseLength")
dev.off()

pdf(file = "upperCaseLength.pdf", width = 4, height = 4) #create PDF
featurePlot(passwordFeaturesOnlyUnique.df, "upperCaseLength")
dev.off()

pdf(file = "digitLength.pdf", width = 4, height = 4) #create PDF
featurePlot(passwordFeaturesOnlyUnique.df, "digitLength")
dev.off()

pdf(file = "specialCharLength.pdf", width = 4, height = 4) #create PDF
featurePlot(passwordFeaturesOnlyUnique.df, "specialCharLength")
dev.off()

pdf(file = "blankCharLength.pdf", width = 4, height = 4) #create PDF
featurePlot(passwordFeaturesOnlyUnique.df, "blankCharLength")
dev.off()

pdf(file = "numUniqueChar.pdf", width = 4, height = 4) #create PDF
featurePlot(passwordFeaturesOnlyUnique.df, "numUniqueChar")
dev.off()

pdf(file = "repeatCharMax.pdf", width = 4, height = 4) #create PDF
featurePlot(passwordFeaturesOnlyUnique.df, "repeatCharMax")
dev.off()

##Password Strength Regression
#Divide dataset into training set/test set
trainSize <- round(nrow(passwordFeaturesOnlyUnique.df) * 0.8, digits = -1) #calculate training set size (80% of total)
trainSetIndex <- sample(1:nrow(passwordFeaturesOnlyUnique.df), trainSize, replace = FALSE) #sample training dataset (indices)
testSetIndex <- setdiff(1:nrow(passwordFeaturesOnlyUnique.df), trainSetIndex)

trainSet <- passwordFeaturesOnlyUnique.df[trainSetIndex, ] #select trainig set w/ 
testSet <- passwordFeaturesOnlyUnique.df[testSetIndex, ]

#save(trainSet, file = "trainSet.RData") #Save dataframe (as .csv, .RData)
#save(testSet, file = "testSet.RData") #Save dataframe (as .csv, .RData)

load("trainSet.RData")
load("testSet.RData")

#Linear regression of all variables
fit.all.train <- glm(strength ~ ., data = trainSet)
summary(fit.all.train) 

#Model Selection (which features to use in the equation)
fit.exh <-regsubsets(strength ~ ., trainSet, nvmax = 8, method="exhaustive")
f.e <-summary(fit.exh)

#Compare different criterions for number of features:
par(mfrow=c(3,1),  mar = c(2.5, 4, 0.5, 1), mgp = c(1.5, 0.5, 0)) #prepare plot window (3 subplots)
plot(f.e$cp, xlab ="Number of features", ylab = "cp", col = "red", type = "p", pch = 16) #plot number of features  vs. Cp
plot(f.e$bic, xlab = "Number of features", ylab = "bic", col ="blue", type = "p", pch = 16) #plot number of features  vs. BIC
plot(f.e$adjr2, xlab = "Number of features", ylab = "adjr2", col = "green", type="p", pch = 16) #plot number of features  vs. R2

Y <- trainSet[, 1] #set reponse variable (strength)
X.fl <- model.matrix(strength ~ ., data = trainSet)[,-1] #create model matrix of all features

#LASSO regression (penalize large number of predictors); calculate appropriate lambda for number of features
fit.fl.cv <-cv.glmnet(X.fl, Y, alpha = 1, nfolds = 10) #calculate CV for alpha values
plot(fit.fl.cv$lambda, xlab = "Lambda Index" , ylab = "Lambda Value" ) #lambda values tested
plot(fit.fl.cv$lambda, fit.fl.cv$cvm, xlab = expression(lambda), ylab = "mean cv errors") #mean CV
plot(fit.fl.cv$lambda, fit.fl.cv$nzero, xlab = expression(lambda), ylab = "number of non-zeros") #number of features

fit.fl.lambda <-glmnet(X.fl, Y, alpha=1, lambda = 0.02) #perform lasso regression; lambda = 0.02
coef(fit.fl.lambda) #identify non-zero features 

#Linear regression with coefficients identified from lasso
fit.new.train <- glm(strength ~ passwordLength + digitLength + blankCharLength + numUniqueChar + repeatCharMax,
                 data = trainSet)
summary(fit.new.train)

#Test predicted model on test data set
predictedStrength <- predict(fit.new.train, testSet) #calculated predicted strengths from regression model
accuracy.df <- data.frame(observed = testSet$strength,
                          predicted = predictedStrength) #create data frame of predicted/observed strengths

pdf(file = "predictions.pdf", width = 4, height = 4) #create PDF
ggplot(data = accuracy.df, (mapping = aes(x = observed, y = predicted))) + geom_point(size = 0.7) + ylim(0, 4) + 
  xlab("Observed Strength") + ylab("Predicted Strength") + theme_bw() + theme(legend.position = "none") #plot predicted vs observed
dev.off()