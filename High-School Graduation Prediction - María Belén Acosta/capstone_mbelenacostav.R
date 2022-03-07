##########################################################
# Data Analysis
##########################################################


##########################################################
## Load packages
##########################################################

if(!require(caret)) install.packages("caret", repos="http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos="http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos="http://cran.us.r-project.org")
if(!require(ggpmisc)) install.packages("ggpmisc", repos="http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos="http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos="http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos="http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos="http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos="http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos="http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos="http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos="http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos="http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos="http://cran.us.r-project.org")

##########################################################
## Load database
##########################################################

set.seed(1, sample.kind = "Rounding")
#2019 
ephc2019 <- read_sav("reg02_ephc2019.sav")

#2020
ephc2020 <- read_sav("reg02_ephc2020.sav")
```
##########################################################
## Variables in the data set
##########################################################

#We'll see how many variables and observations there are. 

ncol(ephc2019)
nrow(ephc2019)
ncol(ephc2020)
nrow(ephc2020)

##########################################################
# TRAIN AND VALIDATION SETS
##########################################################

# Let's create our train and validation sets. 

# The data from 2019 will be our train set
eph_2019 <- ephc2019 %>% 
  select(AREA, P02, P03, P06, E01A, 
         ED01, ED02, ED0504, añoest, ED09, pobnopoi)

# The data from 2020 will be our validation set
eph_2020 <- ephc2020 %>% 
  select(AREA, P02, P03, P06, E01A, 
         ED01, ED02, ED0504, añoest, ED09, pobnopoi)

##########################################################
# REMOVE NA
##########################################################

# Let's remove the NA values, since they will hinder our prediction.
eph_2019 <- eph_2019 %>%
  filter(!is.na(AREA)) %>%
  filter(!is.na(añoest)) %>%
  filter(!is.na(P02)) %>%
  filter(!is.na(pobnopoi)) %>%
  filter(!is.na(ED01))

eph_2020 <- eph_2020 %>%
  filter(!is.na(AREA)) %>%
  filter(!is.na(añoest)) %>%
  filter(!is.na(P02)) %>%
  filter(!is.na(pobnopoi)) %>%
  filter(!is.na(ED01))

##########################################################
## DATA PREPARATION
##########################################################

##########################################################
# create the 'glyst_hs' and 'graduate variables'
##########################################################

# Create 'glyst_hs' variable
eph_2019<- eph_2019 %>% 
  filter(!is.na(añoest)) %>% 
  filter(!añoest %in% c("99")) %>% 
  mutate(glyst_hs = ifelse(añoest %in%
                             c("1","2","3","4","5","6", "7", "8", "9", "10", "11"), "<12",
                           ifelse(añoest %in%
                                    c("12", "13", "14", "15", "16", "17", "18"), ">=12", "<12")))
eph_2019 <- eph_2019 %>% mutate(glyst_hs = as.factor(glyst_hs))

eph_2020<- eph_2020 %>% 
  filter(!is.na(añoest)) %>% 
  filter(!añoest %in% c("99")) %>% 
  mutate(glyst_hs = ifelse(añoest %in%
                             c("1","2","3","4","5","6", "7", "8", "9", "10", "11"), "<12",
                           ifelse(añoest %in%
                                    c("12", "13", "14", "15", "16", "17", "18"), ">=12", "<12")))
eph_2020 <- eph_2020 %>% mutate(glyst_hs = as.factor(glyst_hs))

#Creating the 'graduate' variable
eph_2019 <- eph_2019 %>% 
  filter(!is.na(añoest)) %>% 
  mutate(graduate = ifelse(añoest %in% c("12"), "HSgrad",
                           ifelse(añoest %in% c("1","2","3","4","5","6"), "EEB_1_2",
                                  ifelse(añoest %in% c("7", "8", "9"), "EEB3",
                                         ifelse(añoest %in% c("10", "11"), "EM",
                                                ifelse(añoest %in% 
                                                         c("13", "14", "15","16", "17", "18"), "HED", 0))))))

eph_2020 <- eph_2020 %>% 
  filter(!is.na(añoest)) %>%
  mutate(graduate = ifelse(añoest %in% 
                             c("12"), "HSgrad",ifelse(añoest %in% 
                                                        c("1","2","3","4","5","6"), "EEB_1_2", 
                                                      ifelse(añoest %in% c("7", "8", "9"), "EEB3",
                                                             ifelse(añoest %in% c("10", "11"), "EM", 
                                                                    ifelse(añoest %in% c("13", "14", "15","16", "17", "18"), "HED", 0))))))

##########################################################
# MODELS
##########################################################

##########################################################
## DATA PREPARATION
##########################################################

#Here we prepare our database for the modeling

#Set seed 

set.seed(1, sample.kind = "Rounding")

#Select columns and change 'sin instrucción' into 0.

eph_2019 <- eph_2019 %>% select(AREA, añoest, P02, pobnopoi, ED01, glyst_hs)
eph_2019 <- eph_2019 %>% mutate_at(
  vars("AREA", "añoest", "pobnopoi", "ED01", "glyst_hs"),
  funs(as_factor(.))
)
for (i in 1:length(eph_2019$añoest))
{
  eph_2019$añoest[i]<-ifelse(eph_2019$añoest[i]=="Sin instrucción", 0, eph_2019$añoest[i])
}
eph_2019$añoest<-as.numeric(eph_2019$añoest)
eph_2019$P02<-as.numeric(eph_2019$P02)
eph_2019<- eph_2019 %>%  # Create glyst_hs variable
  filter(!is.na(añoest))

# Here we split the 2019 data into train and test sets
eph_2019 <- eph_2019 %>% select(AREA, añoest, P02, pobnopoi, ED01, glyst_hs)
test_index <- createDataPartition(eph_2019$glyst_hs, times=1, p=0.2, list = F)
train_set <- eph_2019[-test_index,]
test_set <- eph_2019[test_index,]

#Given that our data is imbalanced, we have to 'weight' the models.
model_weights <- ifelse(train_set$glyst_hs == "<12",
                        (1/table(train_set$glyst_hs)[1]) * 0.5,
                        (1/table(train_set$glyst_hs)[2]) * 0.5)
sum(model_weights)#The sum MUST equal 1
rm(test_index)


##########################################################
## SVM Model
##########################################################

#Applying SVM to our data

svm.adult = svm(glyst_hs ~AREA+P02+pobnopoi+ED01, data = train_set)
test_set$pred.value = predict(svm.adult, newdata = test_set,type="response")
confusionMatrix(test_set$glyst_hs, test_set$pred.value)

##########################################################
# adding the results to the data.frame 'results'
##########################################################

results <- data.frame(
  Model="SVM (Support Vector Machine)",
  Accuracy=
    Accuracy(test_set$glyst_hs, test_set$pred.value),
  F1Score=
    F1_Score(test_set$glyst_hs, test_set$pred.value))
results
```
With SVM, we have really good accuracy and F1 score. 

##########################################################
# DECISION TREE
##########################################################

# Applying Decision Tree Model
detree <- rpart(glyst_hs ~
                  AREA + P02 + pobnopoi + ED01,
                data = train_set)
# Prediction of data and Confusion Matrix
test_set$pred.value2 = predict(detree, newdata = test_set, type="class")
confusionMatrix(test_set$glyst_hs, test_set$pred.value2)

#adding it to 'results' 

results<- bind_rows(
  results,
  data.frame(Model="Decision Tree",
             Accuracy=Accuracy(test_set$glyst_hs, test_set$pred.value2),
             F1Score=F1_Score(test_set$glyst_hs, test_set$pred.value2)))
results

##########################################################
# VALIDATION
##########################################################

#we follow the same process that we did with the train and test sets

## Data Preparation

#Set seed

set.seed(1, sample.kind = "Rounding")

#Select columns

eph_2020 <- eph_2020 %>% select(AREA, añoest, P02, pobnopoi, ED01, glyst_hs)
eph_2020 <- eph_2020 %>% mutate_at(
  vars("AREA", "añoest", "pobnopoi", "ED01", "glyst_hs"),
  funs(as_factor(.))
)
for (i in 1:length(eph_2020$añoest))
  {
  eph_2020$añoest[i]<-ifelse(eph_2020$añoest[i]=="Sin instrucción", 0, eph_2020$añoest[i])
}
eph_2020$añoest<-as.numeric(eph_2020$añoest)
eph_2020$P02<-as.numeric(eph_2020$P02)
eph_2020<- eph_2020 %>%  # Create glyst_hs variable
  filter(!is.na(añoest))

#Add weights to our imbalanced data

eph_2020 <- eph_2020 %>% select(AREA, añoest, P02, pobnopoi, ED01, glyst_hs)
model_weights <- ifelse(eph_2020$glyst_hs == "<12",
                        (1/table(eph_2020$glyst_hs)[1]) * 0.5,
                        (1/table(eph_2020$glyst_hs)[2]) * 0.5)
sum(model_weights)#The sum MUST equal 1

##########################################################
## SVM MODEL - VALIDATION
##########################################################

svm.adult = svm(glyst_hs ~
                  AREA +  P02 + pobnopoi + ED01,
                data = eph_2019)
eph_2020$pred.value = predict(svm.adult, newdata = eph_2020,type="response")

##adding it to results
 
results<- bind_rows(
  results,
  data.frame(Model="Validation - SVM",
             Accuracy=Accuracy(eph_2020$glyst_hs, eph_2020$pred.value),
             F1Score=F1_Score(eph_2020$glyst_hs, eph_2020$pred.value)))
results
```

##########################################################
## DECISION TREE - VALIDATION
##########################################################

# Applying Decision Tree Model
detree <- rpart(glyst_hs ~
                  AREA  + P02 + pobnopoi + ED01,
                data = eph_2019)
# Prediction of data and Confusion Matrix
eph_2020$pred.value2 = predict(detree, newdata = eph_2020, type="class")
ConfusionMatrix(eph_2020$glyst_hs, eph_2020$pred.value2)

## Adding it to results

results<- bind_rows(
  results,
  data.frame(Model="Validation - Decision Tree",
             Accuracy=Accuracy(eph_2020$glyst_hs, eph_2020$pred.value2),
             F1Score=F1_Score(eph_2020$glyst_hs, eph_2020$pred.value2)))
results
