setwd('C:/Users/杨扬/Desktop/系统检索/Figure 1/Bar/')
library(tibble)
library(ggplot2)
library(RColorBrewer)
data <- read.csv('AUC.csv',header = FALSE)
rownames(data) <- data[,1]
data <- data[,-1]
df <- data.frame()
for(i in 1:132)
{
  get <- data[i,]
  get <- na.omit(as.numeric(get))
  mean <- mean(get)
  sd <- sd(get)
  confi_low <- mean-sd
  confi_high <- mean+sd
  tempo <- as.data.frame(cbind(mean,confi_low,confi_high))
  colnames(tempo) <- c('Mean','Low','High')
  df <- as.data.frame(rbind(df,tempo))
}
rownames(df) <- rownames(data)
df <- rownames_to_column(df,'ID')
df1 <- df[1:66,]
df2 <- df[67:132,]
df1$'Group' <- sapply(df1$ID,function(x){strsplit(x,'_')[[1]][1]})
df2$'Group' <- sapply(df2$ID,function(x){strsplit(x,'_')[[1]][1]})
df1$ID <- factor(df1$ID,levels = df1$ID)
df2$ID <- factor(df2$ID,levels = df2$ID)

pdf('Bar1.pdf',height = 10,width = 14)
ggplot(df1,mapping=aes(x=ID,y=Mean,fill=Group))+
  geom_col(width = 0.8) +
  guides(fill='none')+
  geom_errorbar(mapping=aes(ymax=High,
                            ymin=Low,
                            color=Group),
                width=0.5,
                position = position_dodge(width = 0.85)) +
  theme_classic()+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.12))+
  theme(axis.title.x = element_blank(),
        legend.position = 'None',
        axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5),
        axis.text.y = element_text(angle = 90,hjust = 0.5))+
  ylab('AUC')+
  scale_x_discrete(labels=c('TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR'))+
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Paired"))(11))+
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Paired"))(11))
dev.off()

pdf('Bar2.pdf',height = 10,width = 14)
ggplot(df2,mapping=aes(x=ID,y=Mean,fill=Group))+
  geom_col(width = 0.8) +
  guides(fill='none')+
  geom_errorbar(mapping=aes(ymax=High,
                            ymin=Low,
                            color=Group),
                width=0.5,
                position = position_dodge(width = 0.85)) +
  theme_classic()+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.16))+
  theme(axis.title.x = element_blank(),
        legend.position = 'None',
        axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5),
        axis.text.y = element_text(angle = 90,hjust = 0.5))+
  ylab('AUC')+
  scale_x_discrete(labels=c('TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR',
                            'TabPFN','RF','GBDT','SVM','KNN','LR'))+
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Paired"))(11))+
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Paired"))(11))
dev.off()