setwd('C:/Users/杨扬/Desktop/系统检索/Figure 1/')
library(ComplexHeatmap)
library(RColorBrewer)
data <- read.csv('AUCs.csv')
rownames(data) <- data[,1]
data <- data[,-1]
colnames(data) <- factor(colnames(data),levels = colnames(data))
average <- apply(data, 1, mean)
average <- as.numeric(format(average, digits = 3, nsmall = 3))
row_ha = rowAnnotation(bar = anno_barplot(average, bar_width = 0.8, border = FALSE,
                                          gp = gpar(fill = "steelblue", col = NA),
                                          add_numbers = T, numbers_offset = unit(-10, "mm"),
                                          axis_param = list("labels_rot" = 0),
                                          numbers_gp = gpar(fontsize = 9, col = "white"),
                                          width = unit(3, "cm")),
                       show_annotation_name = F)
getPalette = colorRampPalette(brewer.pal(12, 'Paired'))
CohortCol <- getPalette(22)
names(CohortCol) <- colnames(data)
col_ha = columnAnnotation("Diseases" = factor(colnames(data),levels = colnames(data)),
                          col = list("Diseases" = CohortCol),
                          show_annotation_name = F)
cellwidth = 1.3
cellheight = 0.8
hm <- Heatmap(as.matrix(data), name = "AUC",
              right_annotation = row_ha,
              top_annotation = col_ha,
              col = c("#4195C1", "#FFFFFF", "#CB5746"),
              rect_gp = gpar(col = "black", lwd = 1),
              cluster_columns = FALSE, cluster_rows = FALSE,
              show_column_names = FALSE,
              show_row_names = TRUE,
              row_names_side = "left",
              width = unit(cellwidth * ncol(data) + 2, "cm"),
              height = unit(cellheight * nrow(data), "cm"),
              column_split = factor(colnames(data), levels = colnames(data)),
              column_title = NULL,
              cell_fun = function(j, i, x, y, w, h, col) {
                grid.text(label = format(data[i, j], digits = 3, nsmall = 3),
                          x, y, gp = gpar(fontsize = 10))})
pdf(file.path('.', "AUC.pdf"), width = cellwidth * ncol(data), height = cellheight * nrow(data) * 1.5)
draw(hm)
invisible(dev.off())