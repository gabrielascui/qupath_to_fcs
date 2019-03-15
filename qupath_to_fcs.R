###### Converter QuPath files to FCS ######
# Version 0.1.1
# Gabriel Ascui
# Based on https://github.com/sydneycytometry/CSV-to-FCS/blob/master/CSV-to-FCS%20v2.0.R

### QuPath TXT to CSV files

# Install packages if required
#if(!require('flowCore')) {install.packages('flowCore')} # <- not available in R 3.5.2
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("flowCore", version = "3.8")
if(!require('Biobase')) {install.packages('Biobase')}
if(!require('data.table')) {install.packages('data.table')}

# Load packages
library('flowCore')
library('Biobase')
library('data.table')

# Get Directory and TXT file list
PrimaryDirectory <- getwd()
TXTNames <- list.files(path=PrimaryDirectory, pattern = ".txt")     # see a list of TXT files
as.matrix(TXTNames) # matrix of TXT files
# Empty list
TXTList=list()
# Read and filter all TXT files 
for (File in TXTNames) { # Loop to read TXT files registered in TXTNames into the list
  tempdata <- fread(File, check.names = FALSE) #read File into tempdata
  tempdata <- tempdata[,5:75] # get rid of the classes and other informations not stored in FCS files
  File <- gsub(".txt", "", File)
  TXTList[[File]] <- tempdata
}
rm(tempdata)
AllTXTNames <- names(TXTList) # names of TXT files
# write CSV files from TXT files
for(i in c(1:length(AllTXTNames))){
  data_qupath <- TXTList[i]
  data_qupath <- rbindlist(as.list(data_qupath))
  dim(data_qupath)
  b <- names(TXTList)[i]
  write.csv(data_qupath, paste0(b, ".csv"))
  }

## Chech data quality
head(TXTList)

# CSV to FCS
# Coverting .csv file data into an .fcs file
# Thomas Ashhurst
# 2017-09-13
# github.com/sydneycytometry
# .fcs file reading and writing adapted from https://gist.github.com/yannabraham/c1f9de9b23fb94105ca5

##### END USER INPUT #####

x <- Sys.time()
x <- gsub(":", "-", x)
x <- gsub(" ", "_", x)

newdir <- paste0("Output_CSV-to-FCS", "_", x)

setwd(PrimaryDirectory)
dir.create(paste0(newdir), showWarnings = FALSE)
setwd(newdir)

for(i in c(1:length(AllTXTNames))){
  data_subset <- TXTList[i]
  data_subset <- rbindlist(as.list(data_subset))
  dim(data_subset)
  a <- names(TXTList)[i]
  
  metadata <- data.frame(name=dimnames(data_subset)[[2]],desc=paste('column',dimnames(data_subset)[[2]],'from dataset'))
  
  ## Create FCS file metadata - ranges, min, and max settings
  metadata$range <- apply(apply(data_subset,2,range),2,diff)
  metadata$minRange <- apply(data_subset,2,min)
  metadata$maxRange <- apply(data_subset,2,max)
  
  data_subset.ff <- new("flowFrame",exprs=as.matrix(data_subset), parameters=AnnotatedDataFrame(metadata)) # in order to create a flow frame, data needs to be read as matrix by exprs
  head(data_subset.ff)
  write.FCS(data_subset.ff, paste0(a, ".fcs"))
}
