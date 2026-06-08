# Set the working directory for input data and generated figures.
setwd('C:/Users/杨扬/Desktop/系统检索/Figure 1/')
# Load the R packages required for plotting and data processing.
library(cvms)
library(tibble)
library(ggplot2)
# Read the tabular input data used in this figure.
data <- read.csv('Confusion_GBDT.csv')
colnames(data) <- c('Reference','Prediction')
data$Reference[which(data$Reference==1)] <- 'Disease'
data$Reference[which(data$Reference==0)] <- 'Control'
data$Prediction[which(data$Prediction==1)] <- 'Disease'
data$Prediction[which(data$Prediction==0)] <- 'Control'
table <- as.tibble(table(data))
colnames(table)[3] <- 'N'
# Open a PDF graphics device for exporting the final figure.
pdf('GBDT.pdf',width = 5,height = 5)
plot_confusion_matrix(table, 
                      target_col = "Reference", 
                      prediction_col = "Prediction",
                      counts_col = "N",
                      palette = 'Purples')+
  xlab('Reference')+
  ylab('Prediction')
# Close the graphics device after writing the figure file.
dev.off()