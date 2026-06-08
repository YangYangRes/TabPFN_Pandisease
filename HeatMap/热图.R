# Set the working directory for input data and generated figures.
setwd('C:/Users/杨扬/Desktop/系统检索/Figure 1/')
# Load the R packages required for plotting and data processing.
library(ComplexHeatmap)
library(RColorBrewer)
# Read the tabular input data used in this figure.
data <- read.csv('AUCs.csv')
# Use the first data column as row names before plotting.
rownames(data) <- data[,1]
data <- data[,-1]
colnames(data) <- factor(colnames(data),levels = colnames(data))
# Compute summary values displayed alongside the heatmap.
average <- apply(data, 1, mean)
average <- as.numeric(format(average, digits = 3, nsmall = 3))
# Build row annotations that summarize each feature across cohorts.
row_ha = rowAnnotation(bar = anno_barplot(average, bar_width = 0.8, border = FALSE,
                                          gp = gpar(fill = "steelblue", col = NA),
                                          add_numbers = T, numbers_offset = unit(-10, "mm"),
                                          axis_param = list("labels_rot" = 0),
                                          numbers_gp = gpar(fontsize = 9, col = "white"),
                                          width = unit(3, "cm")),
                       show_annotation_name = F)
# Create a reusable color palette for cohort annotations.
getPalette = colorRampPalette(brewer.pal(12, 'Paired'))
CohortCol <- getPalette(22)
names(CohortCol) <- colnames(data)
# Build column annotations that identify disease or cohort groups.
col_ha = columnAnnotation("Diseases" = factor(colnames(data),levels = colnames(data)),
                          col = list("Diseases" = CohortCol),
                          show_annotation_name = F)
cellwidth = 1.3
cellheight = 0.8
# Construct the heatmap object and configure its visual layout.
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
              # Draw numeric labels inside each heatmap cell.
              cell_fun = function(j, i, x, y, w, h, col) {
                grid.text(label = format(data[i, j], digits = 3, nsmall = 3),
                          x, y, gp = gpar(fontsize = 10))})
# Open a PDF graphics device for exporting the final figure.
pdf(file.path('.', "AUC.pdf"), width = cellwidth * ncol(data), height = cellheight * nrow(data) * 1.5)
# Render the prepared heatmap to the active graphics device.
draw(hm)
# Close the graphics device after writing the figure file.
invisible(dev.off())