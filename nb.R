source("processed_data.R")

nb <- function() {
  processed_data()
  classifier <- naiveBayes(Success ~ Clump_Thickness+Uniformity_of_Cell_Size+Uniformity_of_Cell_Shape+Marginal_Adhesion+Single_Epithelial_Cell_Size+Bare_Nuclei+Bland_Chromatin+Normal_Nucleoli+Mitoses, data = trainData)
  y_pred <- predict(classifier, newdata = testData)
}

nb()

