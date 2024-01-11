# In our Data Science project, we will make use of these four packages
library(recommenderlab)
library(ggplot2)                       
library(data.table)
library(reshape2)
# We will now retrieve our data from movies.csv into movie_data dataframe and 
# ratings.csv into rating_data. We will use the str() function to display 
# information about the movie_data dataframe.
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")
str(movie_data)
summary(movie_data)
head(movie_data)
str(rating_data)
summary(rating_data)
head(rating_data)
# From the above table, we observe that the userId column, as well as the 
# movieId column, consist of integers. Furthermore, we need to convert the 
# genres present in the movie_data dataframe into a more usable format by the 
# users. 
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
library(data.table)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) 
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) 
    genre_mat1[index+1,gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) 
#remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) 
  #convert from characters to integers
} 
str(genre_mat2)
# In the next step of Data Pre-processing of R project, we will create a 
# ‘search matrix’ that will allow us to perform an easy search of the films by 
# specifying the genre present in our list.
SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])
head(SearchMatrix)    
# For our movie recommendation system to make sense of our ratings 
# through recommenderlabs, we have to convert our matrix into a sparse matrix 
# one. This new matrix is of the class ‘realRatingMatrix’.
ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
# Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")
# We will implement a single model in our R project – Item Based Collaborative 
# Filtering.
recommendation_model$IBCF_realRatingMatrix$parameters
# Collaborative Filtering involves suggesting movies to the users that are 
# based on collecting preferences from many other users. With the help of 
# recommenderlab, we can compute similarities using various operators like 
# cosine, pearson as well as jaccard.
similarity_mat <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)

image(as.matrix(similarity_mat), main = "User's Similarities")
# Delineate the similarity that is shared between the films
movie_similarity <- similarity(ratingMatrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(movie_similarity)

image(as.matrix(movie_similarity), main = "Movies similarity")
# Extract the most unique ratings –
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) # extracting unique ratings
Table_of_Ratings <- table(rating_values) # creating a count of movie ratings
Table_of_Ratings
# Explore the most viewed movies in our dataset - visualisation. 
movie_views <- colCounts(ratingMatrix) # count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # sort by number of views
table_views$title <- NA
for (index in 1:10325){
  table_views[index,3] <- as.character(subset(movie_data,
                                              movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,]
# Visualize a bar plot for the total number of views of the top films.
ggplot(table_views[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggtitle("Total Views of the Top Films")
# Visualize a heatmap of the movie ratings
image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")