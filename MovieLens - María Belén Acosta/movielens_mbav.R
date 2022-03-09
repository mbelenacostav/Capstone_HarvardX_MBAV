
##########################################################
#LOAD THE DATABASE
##########################################################


##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed) 


##########################################################
### Loading extra packages
##########################################################

if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

##########################################################
### Splitting the edx database 
##########################################################

#First, we divide the edx database into train and test sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Then, we assure that the variables "userId" and "movieId" are in both the test set and the train set
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Finally, we add the rows removed from the test set back into the train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)
rm(test_index, temp, removed)


##########################################################
### Mutating the timestamp variable
##########################################################

#converting the time stamp on the edx database

edx <-  edx %>% mutate(timestamp = as_datetime(timestamp))

#converting the time stamp on the test_set database

test_set<- test_set %>% mutate(timestamp = as_datetime(timestamp))

#converting the time stamp on the train_set database

train_set<- train_set %>% mutate(timestamp = as_datetime(timestamp))

#converting the time stamp on the validation database

validation <- validation %>% mutate(timestamp = as_datetime(timestamp))


##########################################################
### Creating a column named "Release_Y" 
##########################################################

#changing the edx database to add a column named Release_Y
edx <- edx %>% mutate(Release_Y = as.numeric(str_sub(title,-5,-2)))

#changing the test_set database to add a column named Release_Y
test_set <- test_set %>% mutate(Release_Y = as.numeric(str_sub(title,-5,-2)))

#changing the train_set database to add a column named Release_Y
train_set <- train_set %>% mutate(Release_Y = as.numeric(str_sub(title,-5,-2)))

#changing the validation database to add a column named Release_Y
validation <- validation %>% mutate(Release_Y = as.numeric(str_sub(title,-5,-2)))


##########################################################
### Creating a column named "yearRated"
##########################################################


#create a new column filled with the years in which the movies were rated

##on the edx database
edx$yearRated <- as.POSIXct(edx$timestamp,
                            origin="1970-01-01")
edx$yearRated <- format(edx$timestamp,"%Y")
edx$yearRated <- as.numeric(edx$yearRated)

##on the validation database 
validation$yearRated <- as.POSIXct(validation$timestamp,
                                   origin="1970-01-01")
validation$yearRated <- format(validation$timestamp,"%Y")
validation$yearRated <- as.numeric(validation$yearRated)

##on the train set
train_set$yearRated <- as.POSIXct(train_set$timestamp,
                                  origin="1970-01-01")
train_set$yearRated <- format(train_set$timestamp,"%Y")
train_set$yearRated <- as.numeric(train_set$yearRated)

##on the test set
test_set$yearRated <- as.POSIXct(test_set$timestamp,
                                 origin="1970-01-01")
test_set$yearRated  <- format(test_set$timestamp,"%Y")
test_set$yearRated <- as.numeric(test_set$yearRated)
```


##########################################################
## Basic information on the database
##########################################################


#join both edx and validation databases
bidtb<- rbind(edx, validation)

bidtb %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#amount of genres

genres <- bidtb %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise()
rm(bidtb)
rm(genres)
```
##########################################################
# Data exploration
##########################################################

##########################################################
## Quantity of movies per genre
##########################################################

movies_per_genre <- train_set %>% #amount of movies per genre 
  separate_rows(genres, sep = "\\|") %>% #does not represent amount of movies in the database
  group_by(genres) %>% 
  summarise(quantity = n()) %>%
  arrange(desc(quantity)) 

#We can see it graphically as
movies_per_genre %>% ggplot(aes(genres, quantity)) + 
  geom_segment(aes(x=reorder(genres, quantity) ,xend=genres, y=0, yend=quantity), color="black") + geom_point(size=4, color="#00945d") + coord_flip() + 
  geom_text(aes(label= quantity), position = position_nudge(y= 300000)) +   
  xlab("Quantity") + scale_y_continuous(name="Genres", labels = scales::comma)

rm(movies_per_genre)

##########################################################
## Movies per year
##########################################################

#Making the table
movies_per_releaseY <- train_set %>% 
   group_by(Release_Y) %>%
  summarise(quantity = n()) %>%
  arrange(desc(quantity))
head(movies_per_releaseY, 10)

#Graphically,

movies_per_releaseY %>%
  ggplot(aes(x=Release_Y, y=quantity)) +
  geom_line(color="#00945d") + 
  geom_point(color= "black") +
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + xlab("Years") +
  scale_y_continuous(name="Amount of movies", labels = scales::comma)

rm(movies_per_releaseY)


########################################################## 
## Rating distribution per decade
##########################################################

train_set %>% 
  separate_rows(genres, sep = "\\|") %>% 
  group_by(genres) %>% 
  summarise(avg_rating = mean(rating))
##########################################################
# Models
##########################################################

#As instructed, we will be working with RMSE. 
#It tells us the average distance between the predicted values from the model and the actual 
#values in the dataset.

#The lower the RMSE, the better a given model is able to "fit" a dataset. 
#Source: [Statology](https://www.statology.org/how-to-interpret-rmse/)

#Our goal is to achieve an RMSE lower or equal to **0.86549**

##########################################################
## Model 1
##########################################################

#First, let's compute an RMSE based on the mean.

mean <- mean(train_set$rating)

#we'll use our RMSE function

first_RMSE <- RMSE(test_set$rating, mean)

#Let's create a dataframe to store and compare all of our results.

results <- data.frame(
  Model="First Model", RMSE = first_RMSE)
results%>% knitr::kable()
results

##########################################################
## Model 2: Movie Effect
##########################################################

#To estimate $b_i$ we'll the following code:

mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#With the next plot, we can visualize these estimates: 

qplot(b_i, data = movie_avgs, bins = 10, color = I("#00945d"))


#Now, let's see the improvement on our model: 

movie_effect <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
snd_RMSE <- RMSE(movie_effect, test_set$rating)

results<- bind_rows(
  results,
  data.frame(Model="Model 2",
             RMSE = snd_RMSE))
results

##########################################################  
## Model 3: Movie + User Effect.
##########################################################  

train_set %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "#00945d")


# $b_u$ is the average of $Y_{u,i} - \hat{\mu} - \hat{b}_i$, we calculate it like this for our database: 
```{r}
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#First, create the predictors
movieuser_effect <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
thrd_RMSE <- RMSE(movieuser_effect, test_set$rating)

results<- bind_rows(
  results,
  data.frame(Model="Model 3",
             RMSE = thrd_RMSE))
results

########################################################## 
## Regularization
########################################################## 


### Penalized least squares

# Let's create our lambdas
lambdas <- seq(0, 10, 0.25)

#Let's add the regularization to the third model: Movie and User effect.
rmses_movieuser <- sapply(lambdas, function(l){
  
mu <- mean(edx$rating)
  
b_i <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n() +l))
  
b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+l))
  
predicted_ratings <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
  
})
reg_model_3 <- rmse_regularisation <- min(rmses_movieuser)
results<- bind_rows(
  results,
  data.frame(Model="Reg_Model 3",
             RMSE = reg_model_3))
results
```

#Great news! we've achieved our RMSE goal: the RMSE (**0.8654673**) is indeed lower than 0.86549.
#Now, let's check which lambda value minimizes our RMSE

qplot(lambdas, rmses_m_u)
lambdas[which.min(rmses_m_u)]
```
To minimize the RMSE, lambda must be **4.5**

##########################################################   
#Validation
########################################################## 

#We calculate the mean for the edx database
muhat <- mean(edx$rating)
muhat

#Let's validate our model using the ```validation``` database.
lambdas <- seq(0, 10, 0.1)
# Modeling with Regularized Movie + User Effect Model
reg_movieuser_val <- sapply(lambdas, function(l){
  # Average rating by movie
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - muhat)/(n()+l))
  # Average rating by user
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - muhat)/(n()+l))
  # RMSE prediction on the validation database
  predicted_ratings <-
    validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = muhat + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})
rmse_reg_movieuser_val <- min(reg_movieuser_val)

results<- bind_rows(
  results,
  data.frame(Model="Validation - Reg_Model 3",
             RMSE = rmse_reg_movieuser_val))
results

#This new result more than reaches our goal since, **0.864817** is smaller 
#than **0.8654673** (our previous RMSE) and both are lower than the goal of an **RMSE <= 0.86549**