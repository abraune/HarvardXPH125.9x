##############################################################################+
# Chapter 0: R Setup // install needed components                          ####
##############################################################################+

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(lubridate)

##############################################################################+
# Chapter 1: Intro // Load data, create train and validation data set      ####
##############################################################################+

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier: (which you obviously should not do)
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
train <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in train set
validation <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from validation set back into train set
removed <- anti_join(temp, validation)
train <- rbind(train, removed)

# clear workspace environment
rm(list=ls()[! ls() %in% c("train","validation")])


########################################################################+
# Chapter 2.1: Data Exploration // insights on variable dependencies ####
########################################################################+


# Dimensions of datasets
cat("# rows: ",dim(train)[1])
cat("# fields: ",dim(train)[2])


# Dimensions of datasets
cat("# rows: ",dim(validation)[1])
cat("# fields: ",dim(validation)[2])

#Check completeness of sets
anyNA(validation)
anyNA(train)

# Summary of dataset
head(train)%>%
  knitr::kable()
summary(train)%>%
  knitr::kable()


#add column with extracted release year to BOTH data sets. As explained from course officials in the forum: This is an allowed action, as it does NOT alter the validation data set. Adding a column with extracted yearly release dates is simply making all information readable that is already provided and no row is added or dropped.
train <- mutate(train, year= train$title %>% str_extract("(?<=\\()\\d{4}(?=\\))") %>% as.integer())
validation <- mutate(validation, year= validation$title %>% str_extract("(?<=\\()\\d{4}(?=\\))") %>% as.integer())



# Count unique entries
rapply(train,function(x) length(unique(x)))

#Show unique rating entries
cat("Ratings:",sort(unique(train$rating)))



# Describe the rating scale distribution
train %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, fill="lightgrey", color = "black") +
  ggtitle("Movie Ratings") +
  stat_bin(aes(y=..count.., label=..count..), binwidth=0.5, geom="text", vjust=-.5) +
  scale_x_discrete(limits = c(seq(0.5,5,0.5)))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Describe the number of ratings per movie
med <- round(as.numeric(
  train %>%
    count(movieId) %>% 
    summarize(med=median(n))),2)
train %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_density(color="darkblue", fill="lightblue") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Share of movies") +
  ggtitle("Number of ratings per movie")+
  geom_vline(xintercept=med,colour="darkblue", linetype = "longdash", lwd=1)+
    annotate("text",  
             x = 800,
             y = 0.4,
             label = paste("Median =", med),
             col = "darkblue",
             size = 4)

# Describe the number of ratings per user
med <- round(as.numeric(
  train %>%
    count(userId) %>% 
    summarize(med=median(n))),2)
train %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_density(color="darkblue", fill="lightblue") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Share of users") +
  ggtitle("Number of ratings per user")+
  geom_vline(xintercept=med,colour="darkblue", linetype = "longdash", lwd=1)+
    annotate("text",  
             x = 200,
             y = 0.85,
             label = paste("Median =", med),
             col = "darkblue",
             size = 4)

# Describe the rating scale distribution per user
avg <- round(as.numeric(
  train %>%
    group_by(userId) %>% 
    summarize(avg=mean(rating)) %>% summarize(mean(avg))),2)

train %>%
  group_by(userId) %>% 
  summarize(avg=mean(rating)) %>%
  ggplot(aes(avg)) +
  geom_histogram(binwidth = 0.1, fill="lightgrey", color = "black") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5)))+
  xlab("Average of ratings") +
  ylab("Number of users") +
  ggtitle("Average rating per user")+
  geom_vline(xintercept=avg,colour="darkblue", linetype = "longdash", lwd=1)+
  annotate("text",  
    x = 2.5,
    y = 6000,
    label = paste("Mean =", avg),
    col = "darkblue",
    size = 4)



# Describe the rating scale distribution per releaseyear
avg <- round(as.numeric(
  train %>%
    group_by(year) %>% 
    summarize(avg_rating=mean(rating)) %>% summarize(mean(avg_rating))),2)

train %>%
  group_by(year) %>% 
  summarize(avg_rating=mean(rating)) %>%
  ggplot(aes(year,avg_rating))+
  geom_line()+
  geom_point()+
  geom_smooth(method="loess", formula="y ~ x",color="darkblue", fill="lightblue",span = 0.15, method.args = list(degree=1)) +
  ylab("Average of ratings") +
  xlab("Release year") +
  ggtitle("Average rating per year")+
  geom_hline(yintercept=avg, colour="darkblue", linetype = "longdash", lwd=1)+
  annotate("text",  
         x = 1998,
         y = 3.75,
         label = paste("Total mean =", avg),
         col = "darkblue",
         size = 4)

# clear workspace environment
rm(list=ls()[! ls() %in% c("train","validation")])




########################################################################+
# Chapter 2.2: Modelling approach                                    ####
########################################################################+


#define the loss function to compare model performances: The root mean square error (gap between actual and predicted values)
rmse <- function(act, fc){
  sqrt(mean((act - fc)^2))}





## Baseline Model----
#Calculate input parameters for forecast function
avg_rating <- mean(train$rating)

#Calculate error measure to compare models
baseline_rmse <- RMSE(train$rating, avg_rating)

# Report Baseline results
rmse_results <- tibble("Prediction Model" = "Baseline (avg. rating)", RMSE = baseline_rmse) 
rmse_results%>% knitr::kable()





## Model 1: Movie effect Model----
#Calculate input parameters for forecast function
movie_avgs <- train %>% 
  group_by(movieId) %>% 
  summarize(movie_avgs = sum(rating - avg_rating)/(n()))

#Create Forecast
fc <- train %>%
  left_join(movie_avgs, by = "movieId") %>%
  mutate(fc = avg_rating + movie_avgs) %>% .$fc

#Calculate error measure to compare models
model_1_rmse <- RMSE(train$rating, fc)

#Report results
rmse_results <- bind_rows(rmse_results,
                          data_frame("Prediction Model"="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results%>% knitr::kable()





## Model 2: Movie + User effect Model----

#Calculate input parameters for forecast function
user_avgs <- train %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>% 
  summarize(user_avgs = sum(rating - avg_rating - movie_avgs)/(n()))

#Create Forecast
fc <- train %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(fc = avg_rating + movie_avgs + user_avgs) %>% .$fc

#Calculate error measure to compare models
model_2_rmse <- RMSE(train$rating, fc)

# Report results
rmse_results <- bind_rows(rmse_results,
                          data_frame("Prediction Model"="Movie + User Effect Model",
                                     RMSE = model_2_rmse ))
rmse_results%>% knitr::kable()





## Model 3: Movie + User + Year effect Model----
#Calculate input parameters for forecast function
year_avgs <- train %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  group_by(year) %>% 
  summarize(year_avgs = sum(rating - avg_rating - movie_avgs - user_avgs)/(n()))

#Create Forecast
fc <- train %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(year_avgs, by="year") %>%
  mutate(fc = avg_rating + movie_avgs + user_avgs + year_avgs) %>% .$fc

#Calculate error measure to compare models
model_3_rmse <- RMSE(train$rating, fc)

# Report results
rmse_results <- bind_rows(rmse_results,
                          data_frame("Prediction Model"="Movie + User + Year Effect Model",
                                     RMSE = model_3_rmse ))
rmse_results%>% knitr::kable()





## Model 4: Regularized Movie + User + Year effect Model----

#open regulator search, set first window from 0 to 5
regulator <- seq(0,5,.5)

#apply forecast function to all regulator values
rmse_reg <- sapply(regulator, function(y){

  #Calculate input parameters for forecast function
  movie_avgs <- train %>% 
  group_by(movieId) %>% 
  summarize(movie_avgs = sum(rating - avg_rating)/(n()+y))

user_avgs <- train %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>% 
  summarize(user_avgs = sum(rating - avg_rating - movie_avgs)/(n()+y))

year_avgs <- train %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  group_by(year) %>% 
  summarize(year_avgs = sum(rating - avg_rating - movie_avgs - user_avgs)/(n()+y))

#Create Forecast
fc <- train %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(year_avgs, by="year") %>%
  mutate(fc = avg_rating + movie_avgs + user_avgs + year_avgs) %>% .$fc

#Calculate error measure for all regulator values
  return(RMSE(train$rating,fc))
})

# show RMSE results per regulator
qplot(regulator, rmse_reg)

# define regulator by lowest rmse
model_4_reg <- regulator[which.min(rmse_reg)]
model_4_rmse <- min(rmse_reg)
cat("RMSE of ",model_4_rmse," with regulator of ",model_4_reg)


#narrow regulator search to window 0.01 accuracy
regulator <- seq(0.25,0.35,.01)

#apply forecast function to all regulator values
rmse_reg <- sapply(regulator, function(y){
  #Calculate input parameters for forecast function
    movie_avgs <- train %>% 
    group_by(movieId) %>% 
    summarize(movie_avgs = sum(rating - avg_rating)/(n()+y))
  
  user_avgs <- train %>%
    left_join(movie_avgs, by="movieId") %>%
    group_by(userId) %>% 
    summarize(user_avgs = sum(rating - avg_rating - movie_avgs)/(n()+y))
  
  year_avgs <- train %>%
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    group_by(year) %>% 
    summarize(year_avgs = sum(rating - avg_rating - movie_avgs - user_avgs)/(n()+y))
  
  #Create Forecast  
  fc <- train %>% 
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    left_join(year_avgs, by="year") %>%
    mutate(fc = avg_rating + movie_avgs + user_avgs + year_avgs) %>% .$fc
  
  return(RMSE(train$rating,fc))
})
# show RMSE results per regulator
qplot(regulator, rmse_reg)

# define regulator by lowest rmse
model_4_reg <- regulator[which.min(rmse_reg)]
model_4_rmse <- min(rmse_reg)
cat("RMSE of ",model_4_rmse," with regulator of ",model_4_reg)

# Report results
rmse_results <- bind_rows(rmse_results,
                          data_frame("Prediction Model"="Regularized Movie + User + Year Effect Model",
                                     RMSE = model_4_rmse ))
rmse_results%>% knitr::kable()






########################################################################+
# Chapter 3: Model results with validation data set                  ####
########################################################################+


## Baseline Model----
#Calculate input parameters for forecast function
avg_rating <- mean(validation$rating)

#Calculate error measure to compare models
baseline_rmse <- RMSE(validation$rating, avg_rating)

# Report Baseline results
rmse_results <- tibble("Prediction Model" = "Baseline (avg. rating)", RMSE = baseline_rmse) 
rmse_results%>% knitr::kable()





## Model 1: Movie effect Model----

#Calculate input parameters for forecast function
movie_avgs <- validation %>% 
  group_by(movieId) %>% 
  summarize(movie_avgs = sum(rating - avg_rating)/(n()))

#Create Forecast 
fc <- validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  mutate(fc = avg_rating + movie_avgs) %>% .$fc

#Calculate error measure to compare models
model_1_rmse <- RMSE(validation$rating, fc)

# Report results
rmse_results <- bind_rows(rmse_results,
                          data_frame("Prediction Model"="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results%>% knitr::kable()





## Model 2: Movie + User effect Model----

#Calculate input parameters for forecast function
user_avgs <- validation %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>% 
  summarize(user_avgs = sum(rating - avg_rating - movie_avgs)/(n()))

#Create Forecast
fc <- validation %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(fc = avg_rating + movie_avgs + user_avgs) %>% .$fc

#Calculate error measure to compare models
model_2_rmse <- RMSE(validation$rating, fc)

# Report results
rmse_results <- bind_rows(rmse_results,
                          data_frame("Prediction Model"="Movie + User Effect Model",
                                     RMSE = model_2_rmse ))
rmse_results%>% knitr::kable()





## Model 3: Movie + User + Year effect Model----

#Calculate input parameters for forecast function
year_avgs <- validation %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  group_by(year) %>% 
  summarize(year_avgs = sum(rating - avg_rating - movie_avgs - user_avgs)/(n()))

#Create Forecast
fc <- validation %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(year_avgs, by="year") %>%
  mutate(fc = avg_rating + movie_avgs + user_avgs + year_avgs) %>% .$fc

#Calculate error measure to compare models
model_3_rmse <- RMSE(validation$rating, fc)

# Report results
rmse_results <- bind_rows(rmse_results,
                          data_frame("Prediction Model"="Movie + User + Year Effect Model",
                                     RMSE = model_3_rmse ))
rmse_results%>% knitr::kable()





## Model 4: Regularized Movie + User + Year effect Model----

#set regularization parameter from previously trained model
y <- model_4_reg

#Calculate input parameters for forecast function
movie_avgs <- validation %>% 
  group_by(movieId) %>% 
  summarize(movie_avgs = sum(rating - avg_rating)/(n()+y))

user_avgs <- validation %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>% 
  summarize(user_avgs = sum(rating - avg_rating - movie_avgs)/(n()+y))

year_avgs <- validation %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  group_by(year) %>% 
  summarize(year_avgs = sum(rating - avg_rating - movie_avgs - user_avgs)/(n()+y))

#Create Forecast
fc <- validation %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(year_avgs, by="year") %>%
  mutate(fc = avg_rating + movie_avgs + user_avgs + year_avgs) %>% .$fc

# Report RMSE for validation data set with Model 4
model_4_rmse_validation <- RMSE(validation$rating, fc)
tibble("Prediction Model" = "Regularized Movie + User + Year Effect Model", RMSE = model_4_rmse_validation) %>% knitr::kable("simple")