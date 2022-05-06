library(caret)
source("processed_data.R")


lr <- function(){
  processed_data()
  plsFit <- train(
    Class ~ .,
    data = movies.train,
    method = "glm",
    preProc = c("center", "scale"),
    ## added:
    tuneLength = 15
  )
}
lr()
