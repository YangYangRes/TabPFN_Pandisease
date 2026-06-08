# Set the working directory for input data and generated figures.
setwd('C:/Users/杨扬/Desktop/系统检索/肺癌/')
# Load the R packages required for plotting and data processing.
library(dplyr)
# Read the tabular input data used in this figure.
data <- read.csv('data_TCGA.csv')
tumor <- data[which(data$Type==1),]
normal <- data[which(data$Type==0),]
# Use the first data column as row names before plotting.
rownames(tumor) <- NULL
rownames(normal) <- NULL
tumor <- tumor[sample(rownames(tumor),100,replace = FALSE),]
normal <- normal[sample(rownames(normal),100,replace = FALSE),]
hb <- as.data.frame(rbind(tumor,normal))
# Write processed results to a CSV file for downstream use.
write.csv(hb,'data_SHAP.csv',row.names = FALSE)