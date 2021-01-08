##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(tidyr)
library(lubridate)
library(Matrix)
library(scales)

##########################################################
# Load Data
##########################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
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

# Remove variables
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Save variables in RData file
save(edx, validation, file = "movielens.RData")

##########################################################
# Data Exploration
##########################################################

# Summary data
summary(edx)
str(edx)
head(edx)

# Find how many users and movies are 
n_users <- n_distinct(edx$userId)
n_movies <- n_distinct(edx$movieId)

# Add to the train set the time of the movie and also other time variables that could be of use
edx_time <- edx %>% mutate(movie_title = str_sub(title, 1, -8), 
                           year_released = as.numeric(str_sub(title, -5, -2)), 
                           time_rating = as_datetime(timestamp)) %>% 
                    mutate(weekday = weekdays(time_rating), week = week(time_rating), 
                           day = lubridate::day(time_rating), month = month(time_rating), 
                           year = year(time_rating), hour = hour(time_rating),
                           delta_years_rating = year(time_rating) - year_released) %>%
                    select(-timestamp, -title)

# Apply the same for the validation set
validation_time <- validation %>% mutate(movie_title = str_sub(title, 1, -8), 
                                         year_released = as.numeric(str_sub(title, -5, -2)), 
                                         time_rating = as_datetime(timestamp)) %>% 
                                  mutate(weekday = weekdays(time_rating), week = week(time_rating), 
                                         day = lubridate::day(time_rating), month = month(time_rating), 
                                         year = year(time_rating), hour = hour(time_rating),
                                         delta_years_rating = year(time_rating) - year_released) %>%
                                  select(-timestamp, -title)


#Group the data to see how many ratings are given by each user
user_distribution <- edx_time %>% group_by(userId) %>% summarise(n = n()) 

# Calculate the median to plot as reference 
median_user_rating <- median(user_distribution$n)


##########################################################
# Data Visualization
##########################################################


### Users Distribution

# Plot the histogram of user distribution
user_distribution %>% ggplot(aes(n)) + geom_histogram(bins = 30, 
                                                      color = "black", 
                                                      fill = "steelblue") +                                               scale_x_continuous(trans = "log10") + 
                                        xlab("Number of ratings") + ylab("Number of users") + 
                                        geom_vline(xintercept = 62, col = "maroon",
                                                   lwd = 1, lty = 2)

# Show the number of ratings of the top users and the bottom
highest_raters <- user_distribution %>% top_n(10) %>% arrange(desc(n))
lowest_raters  <-  user_distribution %>% top_n(-10) %>% arrange(n)

# Group dataset by movie
movie_distribution <- edx_time %>% group_by(movieId) %>% summarise(n = n())

movie_distribution %>%  ggplot(aes(n)) + 
                        geom_histogram(bins = 30, color = "black", fill = "steelblue") +  
                        scale_x_continuous(trans = "log10") + 
                        xlab("Number of ratings") + ylab("Number of movies") 

# Show the number of ratings of the top users and the bottom
most_rated_movies <- movie_distribution %>% top_n(10) %>% arrange(desc(n)) 
least_rated_movies <-  movie_distribution%>% top_n(-10) %>% arrange(n)


## Ratings Distribution

# Calculate the average of the ratings
mu_rating <- mean(edx_time$rating)

# Histogram of the different ratings given
edx_time %>% ggplot(aes(rating)) + 
             geom_histogram(binwidth = 0.5, fill = "steelblue", col = "black") +
             scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
             scale_y_continuous(labels = number) +
             xlab("Movies Rating") +  ylab("Number of ratings") +
             # ggtitle("Histogram: Number of ratings for each rating") +
             geom_vline(xintercept = mu_rating, col = "maroon", lty = 2, lwd = 1)

## Genres Distribution

# First row corresponds to non genders listed, and therefore is removed
genres_exploration <- edx_time %>% group_by(genres) %>% 
                                   summarise(n = n(), avg_rating = mean(rating), 
                                              sd_rating = sd(rating)) %>% 
                                   slice(-1)

#Averages of movies per genre
avg_movie_genre <- mean(genres_exploration$n)

# Histogram of number of ratings per genre
genres_exploration %>% ggplot(aes(n)) +
                       geom_histogram(bins = 30, 
                                      fill = "steelblue", 
                                      col = "black") + 
                       scale_x_continuous(trans = "log10", labels = number) + 
                       xlab("Number of genres") +  
                       ylab("Number of movies") +
                       geom_vline(xintercept = avg_movie_genre, 
                                  col = "maroon", 
                                  lty = 2, 
                                  lwd = 1)


#Averages of movies per genre
avg_rating_genre <- mean(genres_exploration$avg_rating)

# Histogram of number of ratings per number of genres
genres_exploration %>% ggplot(aes(avg_rating)) +
                              geom_histogram(bins = 30, 
                                             fill = "steelblue", 
                                             col = "black") +   
                              xlab("Rating") +  
                              ylab("Number of genres") +
                              geom_vline(xintercept = avg_rating_genre, 
                                         col = "maroon", 
                                         lty = 2, 
                                         lwd = 1)

# We might consider filter those that have more than 25, 50 or 100k ratings
min_rates <- 75000

genres_exploration %>% filter(n > min_rates) %>%
                       ggplot(aes(genres, avg_rating)) +
                       geom_bar(stat = "identity", 
                                fill = "steelblue") + 
                       geom_errorbar(aes(ymin = avg_rating - sd_rating,
                                         ymax = avg_rating + sd_rating)) +   
                       geom_hline(yintercept = mu_rating, col = "maroon", 
                                  lty = 2, lwd = 1) + 
                       theme(axis.text.x = element_text(angle = 90))+   
                       xlab("Genres") +  
                       ylab("Average Rating")

## Time Distribution

# Plot of movies by year released
edx_time %>% group_by(year_released) %>% 
             summarize(r = mean(rating), n = n()) %>%
             ggplot(aes(as.numeric(year_released), r)) +
                    geom_point() + geom_smooth(method = 'loess') +
                    scale_x_continuous(breaks = seq(1910,2010, by= 10)) +
                    xlab("Year of the movies released") +
                    ylab("Average Rating per year")

# Plot of movies by year rated
edx_time %>% group_by(year) %>% 
             summarize(r = mean(rating), n = n()) %>%
             ggplot(aes(year, r)) + 
                    geom_point() + geom_smooth(method = 'loess') +
                    scale_x_continuous(breaks = seq(1990,2010, by= 2)) +
                    xlab("Year of the movie rating") +
                    ylab("Average Rating per year")


# Plot of number of rates per year
edx_time %>% group_by(year_released) %>% summarise(r = mean(rating), n = n()) %>%
                     ggplot(aes(as.numeric(year_released), n)) + 
                           geom_point() + geom_smooth(method = 'loess') +
                           scale_x_continuous(breaks = seq(1910,2010, by= 10)) + 
                           scale_y_continuous(breaks = seq(0, 800000, by= 200000), ) + 
                           # scale_y_continuous(trans = "log10") + 
                           xlab("Year of the movies released") + 
                           ylab("Number of ratings")  

edx_time %>% group_by(delta_years_rating) %>% 
             summarise(n = n(), mean = mean(rating)) %>% 
             ggplot(aes(delta_years_rating,mean)) + 
                    geom_point(col = "black", fill = "steelblue") +
                    geom_smooth(method = 'loess') + 
                     xlab("Difference (in years) between movie released and its rate") + 
                     ylab("Average rating")  

# Check distribution of years between movie released and time
edx_time %>% ggplot(aes(delta_years_rating)) + 
                    geom_histogram(col = "black", fill = "steelblue")


##########################################################
# Model Development and Results
##########################################################

### 0. Prepare the results ###

#  Define RMSE as a function as it is going to be our measurement variable
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2,na.rm=T))
}

#Minimum RMSE required
min_rmse <- 0.86490

# Initialize the tibble
rmse_results <- tibble()


### 1. Just The Average ###

# Use the average as the estimator
just_avg_rmse <- RMSE(mu_rating, validation$rating)

# Collect the results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Just the average",
                                 RMSE = just_avg_rmse,
                                 Meets.Goal = ifelse(just_avg_rmse < min_rmse, 
                                                     "Yes", "No")))
rmse_results

### 2. Movie Bias ###

# Calculate Movie Averages
movie_avgs <- edx_time %>% group_by(movieId) %>% 
                           summarise(b_m = mean(rating - mu_rating))

#Let's observe the distribution of the movie bias
movie_avgs %>% ggplot(aes(b_m)) + 
                geom_histogram(binwidth = 0.4, 
                               color = "black", 
                               fill = "steelblue") +
                xlab('Movie bias') + 
                ylab('Number of movies')


# To predict the ratings, we use mu_ratings (all movies) 
# + the expected b_i for the validation set movies
movie_bias_ratings <- mu_rating + validation %>% 
  left_join(movie_avgs, by='movieId') %>%.$b_m

# Calculate the RMSE for the movie bias
model_movie_bias_rmse <- RMSE(movie_bias_ratings, validation$rating)

# Collect the results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Movie Effect",
                                 RMSE = model_movie_bias_rmse,
                                 Meets.Goal = ifelse(model_movie_bias_rmse < min_rmse, 
                                                     "Yes", "No")))
rmse_results

### 3. User Bias ###

# Calculate User Averages
user_avgs <- edx_time %>% group_by(userId) %>% 
  summarise(b_u = mean(rating - mu_rating))

#Let's observe the distribution of the user bias
user_avgs %>% ggplot(aes(b_u)) +
              geom_histogram(binwidth = 0.4, 
                             color = "black", 
                             fill = "steelblue") +
              xlab('User bias') + 
              ylab('Number of movies')

# To predict the ratings, we use mu_ratings (all movies) 
# + the expected b_i for the validation set movies
user_bias_ratings <- mu_rating + validation %>% 
                                 left_join(user_avgs, by='userId') %>%.$b_u

model_user_bias_rmse <- RMSE(user_bias_ratings, validation$rating)


# Collect the results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="User Effect",
                                 RMSE = model_user_bias_rmse,
                                 Meets.Goal = ifelse(model_user_bias_rmse<min_rmse, 
                                                     "Yes", "No")))
# Print the results
rmse_results

### 4. Movie and User Bias ###

# Calculate User and Movie  Averages
user_movie_avgs <- edx_time %>% left_join(movie_avgs, by='movieId') %>%
                                group_by(userId) %>% 
                                summarise(b_um = mean(rating - mu_rating - b_m))

#Let's observe the distribution of the bias
user_movie_avgs %>% ggplot(aes(b_um)) +
                    geom_histogram(binwidth = 0.4, 
                                   color = "black", 
                                   fill = "steelblue") +
                    xlab('Movie and user bias') + 
                    ylab('Number of movies')

# To predict the ratings, we use mu_ratings (all movies) and
# the expected b_i for the validation set movies
user_movie_bias_ratings <-  validation %>% 
                            left_join(movie_avgs, by = 'movieId') %>% 
                            left_join(user_movie_avgs, by = 'userId')  %>% 
                            mutate(pred = mu_rating + b_um + b_m) %>% .$pred


model_movie_user_bias_rmse <- RMSE(user_movie_bias_ratings, validation$rating)

# Collect the results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Movie + User Effect",
                                 RMSE = model_movie_user_bias_rmse,
                                 Meets.Goal = ifelse(model_movie_user_bias_rmse<min_rmse, 
                                                     "Yes", "No")))
# Print the results
rmse_results


### 5. Movie, User and Genre Bias ###

# Calculate Genre Averages
genre_avgs <- edx_time %>% left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_movie_avgs,  by = 'userId') %>%
  group_by(genres) %>% 
  summarise(b_g = mean(rating - mu_rating - b_m - b_um))

#Let's observe the distribution of the bias
genre_avgs %>% ggplot(aes(b_g)) +
  geom_histogram(bins = 30, 
                 color = "black", 
                 fill = "steelblue") +
  xlab('Movie + User + Genre bias') + 
  ylab('Number of movies')

# To predict the ratings, we use mu_ratings (all movies) 
# + the expected b_i for the validation set movies
genre_bias_ratings <- validation %>%
                      left_join(movie_avgs, by = 'movieId') %>%
                      left_join(user_movie_avgs,  by = 'userId')  %>%
                      left_join(genre_avgs, by = 'genres')  %>%
                      mutate(pred = mu_rating + b_um + b_m + b_g)  %>% .$pred

model_genre_bias_rmse <- RMSE(genre_bias_ratings, validation$rating)
model_genre_bias_rmse

# Collect the results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Movie + User + Genre Effect",
                                 RMSE = model_genre_bias_rmse,
                                 Meets.Goal = ifelse(model_genre_bias_rmse<min_rmse, 
                                                     "Yes", "No")))
# Print the results
rmse_results

### 6. Movie, User, Genre and Year Released Effect ###

# Calculate Year Released Averages
time_avgs <- edx_time  %>% left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_movie_avgs,  by = 'userId')  %>%
  left_join(genre_avgs, by = 'genres') %>% 
  group_by(year_released) %>%
  summarise(b_yr = mean(rating - mu_rating - b_m - 
                          b_um - b_g))

#Let's observe the distribution of the bias
time_avgs %>% ggplot(aes(b_yr)) + 
  geom_histogram(bins = 30, 
                 color = "black", 
                 fill = "steelblue") +
  xlab('Movie + User + Genre + Year Released bias') + 
  ylab('Number of movies')

# To predict the ratings, we use mu_ratings (all movies) 
# + the expected b_i for the validation set movies
year_released_bias_ratings <- validation_time %>%
                                  left_join(movie_avgs, by = 'movieId') %>%
                                  left_join(user_movie_avgs,  by = 'userId') %>%
                                  left_join(genre_avgs, by = 'genres') %>%
                                  left_join(time_avgs, by = 'year_released')  %>%
                                  mutate(pred = mu_rating + b_um + b_m + b_g + b_yr) %>% 
                                  .$pred

model_year_bias_rmse <- RMSE(year_released_bias_ratings, validation_time$rating)
model_year_bias_rmse

# Collect the results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Movie + User + Genre + Year Released Effect",
                                 RMSE = model_year_bias_rmse,
                                 Meets.Goal = ifelse(model_year_bias_rmse<min_rmse,
                                                     "Yes", "No")))
# Print the results
rmse_results

### 7. Movie, User, Genre and Difference Year Rating - Year Released Effect ###

# Calculate Year Rating - Year Released Averages
time_rating_avgs <- edx_time  %>% left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_movie_avgs, by = 'userId') %>%
  left_join(genre_avgs, by = 'genres') %>% 
  group_by(delta_years_rating) %>%
  summarise(b_y = mean(rating - mu_rating - b_m - 
                         b_um - b_g))

#Let's observe the distribution of the bias
time_rating_avgs %>% ggplot(aes(b_y)) + 
  geom_histogram(bins = 30, 
                 color = "black", 
                 fill = "steelblue") +
  xlab('Movie + User + Genre bias + Year Rating bias') + 
  ylab('Number of movies')

# To predict the ratings, we use mu_ratings (all movies) 
#+ the expected b_i for the validation set movies
year_rating_bias_ratings <- validation_time %>% mutate(delta_years_rating = year - 
                                                         year_released) %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_movie_avgs,  by = 'userId') %>%
  left_join(genre_avgs, by = 'genres') %>%
  left_join(time_rating_avgs, by = 'delta_years_rating')  %>%
  mutate(pred = mu_rating + b_um + b_m + b_g + b_y)  %>% 
  .$pred

model_year_rating_bias_rmse <- RMSE(year_rating_bias_ratings, validation_time$rating)
model_year_rating_bias_rmse

# Collect the results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Movie + User + Genre + Year Rating Effect",
                                 RMSE = model_year_rating_bias_rmse,
                                 Meets.Goal = ifelse(model_year_rating_bias_rmse<min_rmse, 
                                                     "Yes", "No")))
# Print the results
rmse_results

##### 
# REGULARISATION

## 1. Regularisation  Movie and User Effect

#Declare function to be used to evaluate different lambdas (penalties)
movie_user_reg_rmse <- function(l){
  # Calculate the average of ratings
  mu_rating <- mean(edx$rating)
  # Calculate movie bias
  b_m <- edx_time %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_rating)/(n()+l))
  # Add user bias
  b_um <- edx_time %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_um = sum(rating - b_m - mu_rating)/(n()+l))
  # Predict ratings for validation set
  predicted_ratings <- validation %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_um, by = "userId") %>%
    mutate(pred = mu_rating + b_um + b_m) %>%
    .$pred
  # Calculate and return RMSE
  return(RMSE(predicted_ratings, validation$rating))
}

# Let's evaluate with different values of lamba
lambdas <- seq(0, 10, 0.5)
rmses <- sapply(lambdas, movie_user_reg_rmse)

# Plot the RMSE for different lamdas
qplot(lambdas, rmses, xlab = 'lambda' , ylab = 'RMSE')  

# Find out minimum lambda value
lambda_min <- lambdas[which.min(rmses)]

# We re-calculate the model with the lambda that gives us the minimum RMSE
model_movie_user_reg_bias_rmse <- movie_user_reg_rmse(lambda_min)

# Collect the results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Movie + User Regularisation Effect",
                                 RMSE = model_movie_user_reg_bias_rmse,
                                 Meets.Goal = ifelse(model_movie_user_reg_bias_rmse<min_rmse, 
                                                     "Yes", "No")))

# Print the results
rmse_results


### 2. Regularisation Movie, User and Genre Effect

#Declare function to be used to evaluate different lambdas (penalties)
movie_user_genre_reg_rmse <- function(l){
  # Calculate the average of ratings
  mu_rating <- mean(edx$rating)
  # Calculate movie bias
  b_m <- edx_time %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_rating) / (n()+l))
  # Calculate user bias
  b_um <- edx_time %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_um = sum(rating - b_m - mu_rating) / (n()+l))
  # Calculate genre bias
  b_g <- edx_time %>% 
    left_join(b_m,  by = 'movieId') %>% 
    left_join(b_um, by = 'userId') %>%
    group_by(genres) %>% 
    summarise(b_g = sum(rating  - b_m - b_um- mu_rating) / (n() + l))
  # Predict ratings for validation set
  predicted_ratings <- validation %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_um, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu_rating + b_um + b_m + b_g) %>%
    .$pred
  # Calculate and return RMSE
  return(RMSE(predicted_ratings, validation$rating))
}

# Analogously to the movie and user effect, we calculate different values of lambda
lambdas <- seq(0, 10, 0.5)
rmses_g <- sapply(lambdas, movie_user_genre_reg_rmse)

# Plot RMSE for different lambas
qplot(lambdas, rmses_g, xlab = 'lambda' , ylab = 'RMSE')  

# Find out minimum lambda value
lambda_min <- lambdas[which.min(rmses_g)]
lambda_min

# We re-calculate the model with the lambda that gives us the minimum RMSE
model_movie_user_gen_reg_bias_rmse <- movie_user_genre_reg_rmse(lambda_min)

# Collect the results
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Movie + User + 
                                          Genre Regularisation Effect",
                                 RMSE = model_movie_user_gen_reg_bias_rmse,
                                 Meets.Goal = ifelse(model_movie_user_gen_reg_bias_rmse<min_rmse,
                                                     "Yes", "No")))

# Print the results
rmse_results

### 3. Regularisation Movie, User, Genre and Year Effect

#Declare function to be used to evaluate different lambdas (penalties)
movie_user_genre_year_reg_rmse <- function(l){
  # Calculate the average of ratings
  mu_rating <- mean(edx$rating)
  # Calculate movie bias
    b_m <- edx_time %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_rating) / (n()+l))
  # Calculate user bias
  b_um <- edx_time %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_um = sum(rating - b_m - mu_rating) / (n()+l))
  # Calculate genre bias
  b_g <- edx_time %>% 
    left_join(b_m,  by = 'movieId') %>% 
    left_join(b_um, by = 'userId') %>%
    group_by(genres) %>% 
    summarise(b_g = sum(rating  - b_m - b_um- mu_rating) / (n() + l))
  # Calculate year bias (as delta of time between year rated and year released)
  b_yr <- edx_time  %>% 
    left_join(movie_avgs, by = 'movieId') %>%
    left_join(user_movie_avgs,  by = 'userId') %>%
    left_join(genre_avgs, by = 'genres') %>% 
    group_by(delta_years_rating) %>%
    summarise(b_yr = sum(rating - mu_rating - b_m - b_um - b_g) / (n() + l))
  # Predict ratings for validation set
  predicted_ratings <- validation_time %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_um, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_yr, by = "delta_years_rating") %>%
    mutate(pred = mu_rating + b_um + b_m + b_g + b_yr) %>%
    .$pred
  # Calculate and return RMSE
  return(RMSE(predicted_ratings, validation_time$rating))
}

# Find out which is the best lambda value that minimises RMSE
lambdas <- seq(0, 10, 0.5)
rmses_y <- sapply(lambdas, movie_user_genre_year_reg_rmse)

# Plot RMSE for different lambas
qplot(lambdas, rmses_y, xlab = 'lambda', ylab = 'rmse')  

# Find out minimum lambda value
lambda_min <- lambdas[which.min(rmses_y)]

# Re-calculate the model with the lambda that gives  the minimum RMSE
model_movie_user_gen_yr_reg_bias_rmse <- movie_user_genre_year_reg_rmse(lambda_min)
meet_goal <- ifelse(model_movie_user_gen_yr_reg_bias_rmse < min_rmse, 
                    "Yes", "No")

# Collect the results in the tibble
rmse_results <- bind_rows(rmse_results,
                          tibble(Method = "Movie + User + 
                                           Genre + Year Regularisation Effect",
                                 RMSE = model_movie_user_gen_yr_reg_bias_rmse,
                                 Meets.Goal = meet_goal))

# Print the results
rmse_results
