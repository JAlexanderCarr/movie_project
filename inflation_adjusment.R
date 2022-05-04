library(dplyr)
library(chron)
library(stringr)

# Importing the two datasets
movies_orig <- read.csv("data/movies_metadata.csv", sep = ",", header = T)
inflation <- read.csv("data/inflation.csv", sep = ",", header = T, check.names = F)

# Setting the cutoff date for 1900s and 2000s
options(chron.year.expand = function (y, cut.off = 18, century = c(1900, 2000), ...) {
  chron:::year.expand(y, cut.off = cut.off, century = century, ...)
})

# Extracting the columns for analysis
movies_new <- data.frame(movies_orig$original_title,movies_orig$belongs_to_collection,as.numeric(movies_orig$budget), as.numeric(movies_orig$revenue), as.numeric(movies_orig$popularity), movies_orig$original_language, movies_orig$runtime, as.Date(chron(movies_orig$release_date)), movies_orig$genres)
movies_new <- setNames(movies_new, c("original_title","belongs_to_collection","budget","revenue","popularity","original_language","runtime","release_date","genres"))

# Cleaning the data
#FIXME: clean data better
inflation$year <- as.Date(as.character(inflation$year), format = "%Y")
movies_intermediate <- na.omit(movies_new)
movies_intermediate <- filter(movies_intermediate, revenue > 0 & budget > 0)
movies <- filter(movies_intermediate, release_date >= as.Date("1950-01-01", format = "%Y-%m-%d"))

# Adjusting monetary values for inflation
for (i in 1:nrow(movies)) {
  inflation_rate <- inflation[format(inflation$year, "%Y") == format(movies[i,"release_date"], "%Y"), 2]
  revenue <- as.numeric(movies[i, "revenue"])
  budget <- as.numeric(movies[i, "budget"])
  profit<- revenue - budget
  movies[i,"profit"] <- profit
  movies[i,"inflation_profit"] <- profit * inflation_rate
  movies[i,"inflation_revenue"] <- revenue * inflation_rate
  movies[i,"inflation_budget"] <- budget * inflation_rate
  movies[i,"profit_percentage"] <- (revenue * inflation_rate) / (budget * inflation_rate)
}

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