setwd('C:/Users/杨扬/Desktop/系统检索/Figure 1/')
library(cvms)
library(tibble)
library(ggplot2)
data <- read.csv('Confusion_GBDT.csv')
colnames(data) <- c('Reference','Prediction')
data$Reference[which(data$Reference==1)] <- 'Disease'
data$Reference[which(data$Reference==0)] <- 'Control'
data$Prediction[which(data$Prediction==1)] <- 'Disease'
data$Prediction[which(data$Prediction==0)] <- 'Control'
table <- as.tibble(table(data))
colnames(table)[3] <- 'N'
pdf('GBDT.pdf',width = 5,height = 5)
plot_confusion_matrix(table, 
                      target_col = "Reference", 
                      prediction_col = "Prediction",
                      counts_col = "N",
                      palette = 'Purples')+
  xlab('Reference')+
  ylab('Prediction')
dev.off()