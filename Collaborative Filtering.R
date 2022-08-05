#title: "Anime Recommender"
#subtitle: "Exploratory Analysis & Collaborative Filtering & Shiny App"
#author: Ringgold Atienza

#References: https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

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

#Download data here
#https://www.kaggle.com/code/philippsp/book-recommender-collaborative-filtering-shiny/data
#Load dataset
anime <- read.csv("C://Users//ADMIN//Documents//GitHub//Collaborative Filtering Anime//anime.csv")
rating <- read.csv("C://Users//ADMIN//Documents//GitHub//Collaborative Filtering Anime//rating.csv")

#Change feature name
anime <- anime %>%
  mutate(anime_rating = rating) %>%
  subset(select = -c(rating))

#Link anime and rating
anime <- left_join(rating, anime, by = "anime_id")
rm(rating)

#Remove users who rates the same movies more than once
anime <- anime %>%
  distinct(user_id, anime_id, .keep_all = TRUE)

#Drop users that watched anime but did not rate
anime <- subset(anime, rating != -1)

################################################################################
#Collaborative Filtering
#Create matrix
dimension_names <- list(user_id = sort(unique(anime$user_id)), 
                        anime_id = sort(unique(anime$anime_id)))

ratingmat <- spread(select(anime, anime_id, user_id, rating), 
                    anime_id, rating) %>% select(-user_id)

ratingmat <- as.matrix(ratingmat)

dimnames(ratingmat) <- dimension_names

ratingmat[1:20, 1:20]

dim(ratingmat)

#Find similar users
current_user <- "1000"
rated_items <- which(!is.na(as.data.frame(ratingmat[current_user, ])))
rated_items <- as.matrix(rated_items)
selected_users <- names(which(apply(!is.na(ratingmat[,rated_items]), 1, sum) >= 3))
head(selected_users, 40)
                        
user1 <- data.frame(item=colnames(ratingmat), rating=ratingmat[current_user, ]) %>%
  filter(!is.na(rating))
user2 <- data.frame(item=colnames(ratingmat), rating=ratingmat[50, ]) %>%
  filter(!is.na(rating))
tmp <- merge(user1, user2, by = "item")
tmp

#Compute correlation
cor(tmp$rating.x, tmp$rating.y, use = "everything")

#Normalize user's rating to reduce the influence of interindividual differences in mean ratings
rmat <- ratingmat[selected_users, ]
user_mean_ratings <- rowMeans(rmat,na.rm=T)
rmat <- rmat - user_mean_ratings

#Calculate similarity of all other users with current_user
similarities <- cor(t(rmat[rownames(rmat)!=current_user, ]), 
                    rmat[current_user, ], use = 'pairwise.complete.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)
head(res, 40)

#Get predictions for other anime
#Set the number of similar user we want to get for the average (we set 10 most simlar users)
similar_users <- names(res[1:10])

#Create prediction for the similar users
similar_users_ratings <- 
  data.frame(item = rep(colnames(rmat), length(similar_users)),
             rating = c(t(as.data.frame(rmat[similar_users,])))) %>% 
  filter(!is.na(rating))

current_user_ratings <- 
  data.frame(item = colnames(rmat),
             rating = rmat[current_user,]) %>% filter(!is.na(rating))

predictions <- similar_users_ratings %>% 
  filter(!(item %in% current_user_ratings$item)) %>% 
  group_by(item) %>% summarize(mean_rating = mean(rating))

#View recommended top 10 anime for the current_user
predictions %>% 
  arrange(-mean_rating) %>% 
  top_n(10, wt = mean_rating) %>% 
  mutate(anime_id = as.numeric(as.character(item))) %>% 
  left_join(select(anime, name, genre, anime_id), by = "anime_id") %>% 
  select(-item) %>%
  unique() %>%
  datatable(class = "nowrap hover row-border", 
            options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

################################################################################
#RecommenderLab
#Most of the values in the rating matrix are missing, we set them to 0
ratingmat0 <- ratingmat
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
rm(ratingmat0)
gc()

#Convert sparse ratings to Recommenderlab sparse matrix variant.
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

#Run RecommenderLab set “UBCF” - user-based collaborative filtering (nn = 5, most similar users)
model <- Recommender(real_ratings, method = "UBCF", param = list(method = "pearson", nn = 5))

r <- Recommender(real_ratings[1:1000], method = "POPULAR")

#Run predictions
prediction <- predict(r, real_ratings[current_user,], type = "ratings")

#View recommended top 10 anime for the current_user
as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:10,] %>% 
  mutate(anime_id = as.numeric(as.character(item))) %>% 
  left_join(select(anime, name, genre, anime_id), by = "anime_id") %>% 
  select(-item) %>%
  unique() %>%
  datatable(class = "nowrap hover row-border", 
            escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

#Evaluate predicted ratings
scheme <- evaluationScheme(real_ratings[1:1000,], 
                           method = "cross-validation", k = 10, 
                           given = -1, goodRating = 5)

r1 <- Recommender(getData(scheme, "train"), "UBCF")
r2 <- Recommender(getData(scheme, "train"), "IBCF")

p1 <- predict(r1, getData(scheme, "known"), type = "ratings")
p2 <- predict(r2, getData(scheme, "known"), type = "ratings")

error <- rbind(UBCF = calcPredictionAccuracy(p1, getData(scheme, "unknown")),
               IBCF = calcPredictionAccuracy(p2, getData(scheme, "unknown")))
error


#Compare different algorithms in recommenderlab
algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL),
  UBCF = list(name = "UBCF", param = list(nn=50)),
  IBCF = list(name = "IBCF", param = list(k=50)),
  SVD = list(name = "SVD", param = list(k=50)),
  HYBRID = list(name = "HYBRID", param =
                  list(recommenders = list(
                    RANDOM = list(name = "RANDOM", param = NULL),
                    POPULAR = list(name = "POPULAR", param = NULL)))))

# evaluate the alogrithms with the given scheme            
evlist <- evaluate(scheme, algorithms, type = "ratings")
avg(evlist)
plot(evlist, ylim = c(0,20))
