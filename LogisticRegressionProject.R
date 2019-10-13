###
# Logistic Regression Project
# 01. Read in the adult_sal.csv file and set it to a data frame called adult. 
df.adult <- read.csv('adult_sal.csv')

# 02. Check the head of adult
head(df.adult)

# 03. You should notice the index has been repeated. Drop this column. 
drop <- df.adult[-c(1)]

df.adult <- drop

# 04. Check the head,str, and summary of the data now. 
head(df.adult)
str(df.adult)
summary(df.adult)


###
# Data Cleaning
###

# Notice that we have a lot of columns that are cateogrical factors, 
# however a lot of these columns have too many factors than may be necessary.
# In this data cleaning section we'll try to clean these columns up by 
# reducing the number of factors.

# 05. Use table() to check out the frequency of the type_employer column.
table(df.adult$type_employer)

# 06. How many Null values are there for type_employer?  
# 1836 Null Values

# 07. What are the two smallest groups?
# Never-worked and Without-pay
  
# 08. Combine these two smallest groups into a single group called 
# "Unemployed". There are lots of ways to do this, so feel free to get 
# creative. 
# Hint: It may be helpful to convert these objects into character data types 
# (as.character() and then use sapply with a custom function)
df.adult$type_employer <- as.character(df.adult$type_employer)
str(df.adult)
unemployed <- function(job) {
  job <- as.character(job)
  if (job =='Never-worked'| job=='Without-pay') {
    return("unemployed")
  } else {
    return(job)
  }
}

df.adult$type_employer <- sapply(df.adult$type_employer, unemployed)
table(df.adult$type_employer)
# 09. What other columns are suitable for combining? Combine State and 
# Local gov jobs into a category called SL-gov and combine self-employed 
# jobs into a category called self-emp.

df.adult$type_employer <- as.character(df.adult$type_employer)
GovEmployed<- function(job) {
  job <- as.character(job)
  if (job == 'State-gov' | job == 'Local-gov') {
    return("SL-gov")
  } else {
    return(job)
  }
}

df.adult$type_employer <- sapply(df.adult$type_employer, GovEmployed)
table(df.adult$type_employer)


# 10. Use table() to look at the marital column 
table(df.adult$marital)

# 11. Reduce this to three groups:
#     . Married
#     . Not-Married
#     . Never-Married
df.adult$marital <- as.character(df.adult$marital)
maritialStatus <- function(marStat) {
  marStat <- as.character(marStat)
  if (marStat == 'Divorced'| marStat == 'Separated' | marStat == "Widowed" | marStat == "Married-spouse-absent") {
    return("Not-Married")
  } else if(marStat == "Married-AF-spouse" | marStat == "Married-civ-spouse"){
    return("Married")
    } else{
    return(marStat)
  }
}

df.adult$marital <- sapply(df.adult$marital, maritialStatus)
table(df.adult$marital)

# 12. Check the country column using table()
table(df.adult$country)

# 13. Group these countries together however you see fit. 
# You have flexibility here because there is no right/wrong way to do this, 
# possibly group by continents. You should be able to reduce the number of 
# groups here significantly though.

df.asia <- c("Cambodia", "China", "Hong", "Iran", "Japan","India", "Laos","Philippines","Taiwan", "Thailand","Vietnam")

df.northAmerica <- c( "Canada", "United-States","Puerto-Rico", 'Mexico')

df.europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary','Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

df.southAmerica <- c('Columbia','Cuba','Dominican-Republic','Ecuador','El-Salvador','Guatemala','Haiti','Honduras','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru','Jamaica','Trinadad&Tobago')


continent <- function(country){
  if(country %in% df.asia) {
    return("Asia")
  } else if(country %in% df.europe) {
    return("Europe")
  } else if(country %in% df.northAmerica){
    return("North America")
  } else if (country %in% df.southAmerica){
    return("South America")
  } else{
    return("Other")
  }
}

df.adult$country <- sapply(df.adult$country, continent)

# 14. Use table() to confirm the groupings
table(df.adult$country)

# 15. Check the str() of adult again. Make sure any of the columns we 
# changed have factor levels with factor()
str(df.adult)

# Note: You could have also done something like:
# adult$type_employer <- factor(adult$type_employer)
df.adult$type_employer <- factor(df.adult$type_employer)
df.adult$marital <- factor(df.adult$marital)
df.adult$country <- factor(df.adult$country)

# 16. Convert any cell with a '?' or a ' ?' value to a NA value. 
# Hint: is.na() may be useful here or you can also use brackets with a 
# conditional statement. Refer to the solutions if you can't figure this 
# step out.
df.adult[df.adult == '?' | df.adult ==' ?'] <- NA

# 17. Using table() on a column with NA values should now not display those 
# NA values, instead you'll just see 0 for ?. 
# Optional: Refactor these columns (may take awhile). For example: 
table(df.adult$capital_gain)

# 18. Play around with the missmap function from the Amelia package. 
# Can you figure out what its doing and how to use it?
library(Amelia)

# 19. Use na.omit() to omit NA data from the adult data frame. 
# Note, it really depends on the situation and your data to judge whether 
# or not this is a good decision. You shouldn't always just drop NA values.
# May take awhile

missmap(df.adult, y.at=c(1), y.labels = c(''), legend = T, col = c("blue", "red"))

df.adult <- na.omit(df.adult)
# 20. Use missmap() to check that all the NA values were in fact dropped. 

missmap(df.adult, y.at=c(1), y.labels = c(''), legend = T, col = c("blue", "red"))

###
# EDA
###

# 21. Use ggplot2 to create a histogram of ages, colored by income. 
library(ggplot2)

ggplot(df.adult) + geom_density(mapping = aes(x = age, fill = as.factor(income)), alpha = 0.5) + theme_bw()
# 22. Plot a histogram of hours worked per week
ggplot(df.adult) + geom_density(mapping= aes(x = hr_per_week, fill = as.factor(income)), alpha=0.5) + theme_bw()
# 23. Rename the country column to region column to better reflect the 
# factor levels.
ggplot(df.adult, aes(country)) + geom_bar(aes(fill = income)) + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ geom_smooth(method = "loess", se = FALSE) 

# 24. Create a barplot of region with the fill color defined by income 
# class. Optional: Figure out how rotate the x axis text for readability

###
# Train Test Split

# 25. Split the data into a train and test set using the caTools library 
# as done in previous lectures. Reference previous solutions notebooks if 
# you need a refresher. 
library(caTools)
set.seed(145)

# Split up the sample, basically randomly assigns a booleans to 
# a new column "sample"
sample <- sample.split(df.adult$income, SplitRatio = 0.70)

# Training Data
train = subset(df.adult, sample == TRUE)
# Testing Data
test = subset(df.adult, sample == FALSE)

###
# Training the Model

# 26. Explore the glm() function with help(glm). Read through the 
# documentation.

# 27. Use all the features to train a glm() model on the training data set, 
# pass the argument family=binomial(logit) into the glm function.
model.train <- glm(income ~., family = binomial(logit), data = df.adult)

# If you get a warning, this just means that the model may have guessed 
# the probability of a class with a 0% or 100% chance of occuring.

# 28. Check the model summary
summary(model.train)

# We have still a lot of features! Some important, some not so much. 
# R comes with an awesome function called step(). The step() function 
# iteratively tries to remove predictor variables from the model in an 
# attempt to delete variables that do not significantly add to the fit. 
# How does it do this? It uses AIC. Read the wikipedia page for AIC if you 
# want to further understand this, you can also check out help(step). 
# This level of statistics is outside the scope of this project assignment 
# so let's keep moving along


# 29. Use new.model <- step(your.model.name) to use the step() function 
# to create a new model.
new.model <- step(model.train)

# 30. You should get a bunch of messages informing you of the process. 
# Check the new.model by using summary()
summary(new.model)

# 31. Create a confusion matrix using the predict function with 
# type='response' as an argument inside of that function.
test$income.prediction <- predict(model.train, newdata = test, type="response")
table(test$income, test$income.prediction > 0.5)

# Warning message:
# In predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type == : prediction 
# from a rank-deficient fit may be misleading
                                                                  
# 32. What was the accuracy of our model?
(6392+1414)/(528+881+1414+6392)

# 33. Calculate other measures of performance like, recall or precision.
#recall
6392/(6392+528)

# 34. precision
(6441)/(6441+892)
#Ref: www.pieriandata.com




