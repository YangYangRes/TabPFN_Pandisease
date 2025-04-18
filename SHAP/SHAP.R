setwd('C:/Users/杨扬/Desktop/系统检索/肺癌/')
library(dplyr)
data <- read.csv('data_TCGA.csv')
tumor <- data[which(data$Type==1),]
normal <- data[which(data$Type==0),]
rownames(tumor) <- NULL
rownames(normal) <- NULL
tumor <- tumor[sample(rownames(tumor),100,replace = FALSE),]
normal <- normal[sample(rownames(normal),100,replace = FALSE),]
hb <- as.data.frame(rbind(tumor,normal))
write.csv(hb,'data_SHAP.csv',row.names = FALSE)