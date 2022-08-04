#title: "Anime Recommender"
#subtitle: "Exploratory Analysis & Collaborative Filtering & Shiny App"
#author: Ringgold Atienza
  
#Install required packages
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(qgraph)) install.packages("qgraph", repos = "http://cran.us.r-project.org")
if(!require(methods)) install.packages("methods", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

#Load dataset
rating <- read.csv("C://Users//ADMIN//Documents//GitHub//Collaborative Filtering Anime//rating.csv")
anime <- read.csv("C://Users//ADMIN//Documents//GitHub//Collaborative Filtering Anime//anime.csv")

anime <- anime %>%
  mutate(anime_rating = rating) %>%
  subset(select = -c(rating))

#Check for missing values in the rating dataset
na_count <- sapply(rating, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Check for missing values in the anime dataset
na_count <- sapply(anime, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

rm(na_count)

#Drop NA values in anime dataset
anime <- na.omit(anime)

#Drop users that watched anime but did not rate
rating <- subset(rating, rating != -1)

#Remove users with less than 3 ratings (for collaborative filtering)
rating %>% group_by(user_id) %>% mutate(n = n()) %>%
  subset(n > 3)
  
#Summarize number of users, movies, and ratings
rating %>% summarize(n_users = n_distinct(user_id),
                     n_anime = n_distinct(anime_id))

#Visualization

#Distribution of ratings
rating %>% 
  ggplot(aes(x = rating)) +
  geom_bar() +
  scale_x_discrete(limits = c(seq(1,10,1))) +
  labs(x = "Rating", y = "Count")

#Distribution of number of ratings per user
rating %>% 
  group_by(user_id) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  geom_histogram(binwidth = 20) +
  labs(x = "Numer of Ratings per User", y = "Count")

#Number of ratings per user table
rating %>% 
  group_by(user_id) %>% 
  count() %>%
  arrange(desc(n))

#Distribution of mean user rating
rating %>% 
  group_by(user_id) %>% 
  summarize(mean_user_rating = mean(rating)) %>% 
  ggplot(aes(mean_user_rating)) +
  geom_histogram(binwidth = .2) +
  labs(x = "Numer of Ratings per User", y = "Count")

#Distribution of number of ratings per anime
rating %>% 
  group_by(anime_id) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  geom_histogram(binwidth = 200) +
  labs(x = "Numer of Ratings per User", y = "Count")

#Number of ratings per user table
rating %>% 
  group_by(anime_id) %>% 
  count() %>%
  arrange(desc(n))

#Distribution of mean anime rating
rating %>% 
  group_by(anime_id) %>% 
  summarize(mean_anime_rating = mean(rating)) %>% 
  ggplot(aes(mean_anime_rating)) +
  geom_histogram(binwidth = .2) +
  labs(x = "Numer of Ratings per User", y = "Count")



#Genre
anime %>% separate_rows(genre, sep = "\\,") %>%
  group_by(genre) %>%
  filter(genre != "") %>%
  ggplot(aes(x = fct_infreq(genre))) +
  geom_bar() +
  labs(x = "Genre", y = "Count") +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.caption = element_text(size = 12, face = "italic"), 
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 60, hjust = 1))


list_genre <- anime %>% separate_rows(genre, sep = "\\,") %>%
  group_by(genre) %>%
  summarize(n = n()) %>% 
  subset(select = c("genre", "n")) %>%
  filter(genre != "") %>%
  unique()

  


dataset <- left_join(rating, anime, by = "anime_id") %>%
  











rm(anime, rating)

#Partition data into training, validation, and test set

set.seed(2022)
test_index <- createDataPartition(y = dataset$rating, times = 1, p = 0.6, list = FALSE)
trainingset <- dataset[-test_index,]
validation <- dataset[test_index,]
rm(test_index)

test_index <- createDataPartition(y = validation$rating, times = 1, p = 0.5, list = FALSE)
testset <- validation[-test_index,]
validation <- validation[test_index,]
rm(test_index)

