### read files 
library(data.table)
library(ggplot2)

#qupath <- read.delim("/Volumes/ARCS2022/microscopy/Ramp3_BRD509_IF/measurements/lobe_R3H04.txt")
qupath <- fread(file = "/Volumes/ARCS2022/microscopy/Ramp3_BRD509_IF/measurements/square_R3H04.txt", header = TRUE, check.names = FALSE, nThread = 4) ## data.table
colnames(qupath)


coords <- colnames(qupath)[grep("Cell\\:(.*)mean", colnames(qupath))]

ggplot(data = qupath, aes(x = .data[[coords[3]]], 
                          y = .data[[coords[5]]])) + geom_point() + scale_y_log10() + theme_classic()
