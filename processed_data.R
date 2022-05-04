library(caret)

processed_data <- function() {
  # Importing the two datasets
  movies_orig <- read.csv("data/movies_metadata.csv", sep = ",", header = T)
  
  # Extracting the columns for analysis
  movies_new <- data.frame(movies_orig$original_title,movies_orig$belongs_to_collection,as.numeric(movies_orig$budget), as.numeric(movies_orig$revenue), as.numeric(movies_orig$popularity), movies_orig$original_language, movies_orig$runtime, as.Date(chron(movies_orig$release_date)), movies_orig$genres)
  movies_new <- setNames(movies_new, c("original_title","belongs_to_collection","budget","revenue","popularity","original_language","runtime","release_date","genres"))
  
  # Cleaning the data
  movies_intermediate <- na.omit(movies_new)
  movies_intermediate <- filter(movies_intermediate, revenue > 0 & budget > 0)
  movies <<- filter(movies_intermediate, release_date >= as.Date("1950-01-01", format = "%Y-%m-%d"))
  movies[,"profit_percentage"] <- movies$revenue / movies$budget
  
  # Creating dummy variables for success, language, and series
  movies$isSuccess <- ifelse(movies$profit_percentage > 3, 1, 0)
  movies$isEN <- ifelse(movies$original_language == "en", 1, 0)
  movies$isCollection <- ifelse(movies$belongs_to_collection == "", 0, 1)
  
  # Creating dummy variables for genre
  searchGenres <- c("Comedy", "Science Space Fiction", "Horror", "Romance", "Action", "Thriller", "Drama", "Mystery", "Crime", "Animation", "Adventure", "Fantasy")
  movies["Comedy"] <- c(0)
  movies["Science Space Fiction"] <- c(0)
  movies["Horror"] <- c(0)
  movies["Romance"] <- c(0)
  movies["Action"] <- c(0)
  movies["Thriller"] <- c(0)
  movies["Drama"] <- c(0)
  movies["Mystery"] <- c(0)
  movies["Crime"] <- c(0)
  movies["Animation"] <- c(0)
  movies["Adventure"] <- c(0)
  movies["Fantasy"] <- c(0)
  for (i in searchGenres) {
    for (j in movies$genres) {
      if (str_detect(j, i)) {
        movies[i][match(j, movies$genres),] <- 1
      }
      else {
        movies[i][match(j, movies$genres),] <- 0
      }
    }
  }
  
  # Setting up the training and validation data
  train <- createDataPartition(y = movies$isSuccess, p = .90, list = FALSE)
  movies.train <<- movies[train,]
  movies.test <<- movies[-train,]
  movies.ctrl <<- trainControl(method = "cv", number = 10)
}