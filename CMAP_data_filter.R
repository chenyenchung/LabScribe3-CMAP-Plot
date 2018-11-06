####### Loading required packages  #############
dplyrEx <- require(dplyr)
if(!dplyrEx){
  install.packages("dplyr")
  library(dplyr)
}
ggplotEx <- require(ggplot2)
if(!ggplotEx){
  install.packages("ggplot2")
  library(ggplot2)
}

pipeEx <- require("magrittr")
if(!pipeEx){
  install.packages("magrittr")
  library("magrittr")
}

#### Custom functions ####
fill_roi <- function(x, txt, pos, rep){
  for(i in c(1:length(pos))){
    x[c(pos[i]:(pos[i] + rep -1))] <- txt[i]
  }
  return(x)
}

fill_count <- function(x, pos, rep){
  for(i in c(1:length(pos))){
    x[c(pos[i]:(pos[i] + rep -1))] <- i
  }
  return(x)
}

fill_fnum <- function(x, pos, rep) {
  for(i in c(1:length(pos))){
    x[c(pos[i]:(pos[i] + rep -1))] <- seq(rep)
  }
  return(x)
}

posicheck <- function(data, seq) {
  return(sapply(seq,
                function(x) ((filter(data, data$Count == x)$EMG %>% which.max) -
                               (filter(data, data$Count == x)$EMG %>% which.min))))
}


balance <- function(data, seq) {
  return(sapply(c(1:seq),
                function(x) {
                  temp <- range(data$EMG[c(((x-1)*30+1):((x-1)*30+30))])
                  delta <- temp %>% sum %>% abs
                  norm <- abs(temp[1]-temp[2])
                  return(delta/norm)
                }
  ))
}

peakcheck <- function(data, seq) {
  return(sapply(c(1:seq),
                function(x) {
                  raw <- filter(data, data$Count == levels(data$Count)[x])
                  max(raw$EMG) - min(raw$EMG)
                }))
}

####### Start of Analysis ##########

# Setting Working Folder
if(!exists("pickFolder")){
  exFileName <- file.choose()
  setwd(dirname(exFileName))
  pickFolder <- TRUE
} else {
  warnings("Please clear the previous data first.")
}
# Varible type check
source("settings.R")
if(!is.numeric(frameNum)){
  warning("Frame number should be a number!")
  stop()
}
if(!is.numeric(prelude)){
  warning("Prelude should be a number!")
  stop()
}

# Loading raw data
fList <- list.files(pattern = "*.txt")
rawTable <- sapply(fList,
                   function(x) read.table(x, stringsAsFactors = FALSE, fill = TRUE, header = TRUE),
                   simplify = FALSE)
rawTable <- lapply(c(1:length(rawTable)), function(x){
  current_table <- rawTable[[x]]
  extra_col <- "X3" %in% colnames(current_table)
  if(extra_col){
    temp <- current_table[, !(colnames(current_table) == "X3")]
  } else {
    temp <- current_table
  }
  temp$FileName <- fList[x]
  return(temp)
})

rawTable <- do.call("rbind",rawTable)

# Removing repetitive header generated in data export
rawTable <- rawTable[grep("[^0-9\\.\\,-]", rawTable$Time, invert = TRUE),]

rawTable$Time <- as.numeric(rawTable$Time)
rawTable$EMG <- as.numeric(rawTable$EMG)
rawTable$Stim <- as.numeric(rawTable$Stim)

rStim <- which(!rawTable$Stim == 0)
rawTable$ROI <- 0  # Labeling the region of interest around stimulation
rawTable$fNum <- 0  # Counting the frame number in ROI
rawTable$Count <- 0 # Count number of peaks

# Filling ROIs for further analysis

rawTable$ROI <- fill_roi(rawTable$ROI,
                         rawTable$Stim[rStim],
                         rStim - prelude,
                         frameNum)
rawTable$Count <- fill_count(rawTable$Count,
                             rStim - prelude,
                            frameNum)
rawTable$fNum <- fill_fnum(rawTable$fNum, rStim - prelude, frameNum)
workTable <- rawTable[!(rawTable$ROI == 0),] # Removing everything out of ROI
workTable$Count <- factor(workTable$Count)

# Escaping preserved chars in regex
escape <- c(" ",".","(",")","^","$","+","?","|","\\","[" ,"]")
for (i in escape) {
  splitChr <- gsub(pattern = paste0("\\",i),
                   replacement = paste0("\\\\",i),
                   splitChr)
}

workTable$SampleName <- gsub(pattern = paste0(splitChr,".*$"), "", workTable$FileName)

# Selecting only waves that start with positive value
posi <- levels(workTable$Count)[posicheck(workTable, levels(workTable$Count)) < 0]
workTable <- workTable[workTable$Count %in% posi,]

# Take top N balanced waves
subtbl <- split(workTable, list(workTable$SampleName, workTable$ROI))
logfile <- NULL
workTable <- do.call(rbind,
        lapply(c(1:length(subtbl)),
       function(x) {
         data <- subtbl[[x]]
         itemname <- substr(names(subtbl)[x],1,nchar(names(subtbl)[x])-2)
         amp <- substr(names(subtbl)[x],nchar(names(subtbl)[x]),nchar(names(subtbl)[x]))
         #print(itemname)
         #print(amp)
         data$Count <- droplevels(data$Count)
         if(nlevels(data$Count) > 0){
           #print(nlevels(data$Count))
           logfile <<- c(logfile, paste("The", amp, "mA", "of", itemname,
                                        "has", nlevels(data$Count), "valid observations."))
           if(nlevels(data$Count) < pickBal){
             index <- head(order(balance(data,nlevels(data$Count)), decreasing = T), nlevels(data$Count))
             toplist <- levels(data$Count)[index]
             return(data[data$Count %in% toplist,])
           } else {
             index <- head(order(balance(data,nlevels(data$Count)), decreasing = T), n = pickBal)
             toplist <- levels(data$Count)[index]
             return(data[data$Count %in% toplist,])
           }
           
           } else {
           logfile <<- c(logfile, paste("**The", amp, "mA", "of", itemname,
                                        "has no valid observations."))
           return(NULL)
           }}))

# and then find the largest M waves
subtbl <- split(workTable, list(workTable$SampleName, workTable$ROI))
workTable <- do.call(rbind,
                     lapply(c(1:length(subtbl)),
                            function(x) {
                              data <- subtbl[[x]]
                              data$Count <- droplevels(data$Count)
                              if(nlevels(data$Count) > 0){
                                if(nlevels(data$Count) < pickPeak){
                                    index <- head(order(peakcheck(data,nlevels(data$Count)), decreasing = T), nlevels(data$Count))
                                  toplist <- levels(data$Count)[index]
                                  return(data[data$Count %in% toplist,])
                                } else {
                                  index <- head(order(peakcheck(data,nlevels(data$Count)), decreasing = T), n = pickPeak)
                                  toplist <- levels(data$Count)[index]
                                  return(data[data$Count %in% toplist,])
                                }
                                
                              } else {
                                return(NULL)
                              }}))

# Adding metadata
workTable$AssayDate <- as.Date(AssayDate,"%Y%m%d")
workTable$DOB <- gsub("\\(([^()]*)\\)|.", "\\1", workTable$FileName) %>% as.Date(., "%Y%m%d")
workTable <- mutate(workTable, Age = paste("Week",((AssayDate - DOB)/7) %>% round(., digits = 0)))
workTable$Type <- "Unknown"
workTable$Treat <- "None"

# Define sample types (Mock-WT, Mock-ALS, tg-OE, AAV-OE-WT, AAV-OE-ALS)
sapply(sampletype, function(x){
  workTable$Type[grep(x, workTable$SampleName)] <<- x
  return(NULL)
})
sapply(sampletreat, function(x){
  workTable$Treat[grep(x, workTable$SampleName)] <<- x
  return(NULL)
})

workTable <- mutate(workTable, Cat = paste(Type,Treat,sep = "_"))

# Saving files
if(!dir.exists("~/Desktop/CMAP_data/")){dir.create("~/Desktop/CMAP_data/")}
if(!file.exists(paste0("~/Desktop/CMAP_data/", AssayDate, "_Log.csv"))){
  write.csv(logfile, file = paste0("~/Desktop/CMAP_data/", AssayDate, "_Log.csv"))
} else {
  write.csv(logfile, file = paste0("~/Desktop/CMAP_data/", AssayDate, Sys.time(), "_Log.csv"))
}
if(!file.exists(paste0("~/Desktop/CMAP_data/", AssayDate, "_filtered.txt"))){
  write.table(workTable, file = paste0("~/Desktop/CMAP_data/", AssayDate, "_filtered.txt"))
} else {
  write.table(workTable, file = paste0("~/Desktop/CMAP_data/", AssayDate, Sys.time(), "_filtered.txt"))
}

