####### Experiment Paramenters ################
frameNum <- 25 #How many frames to plot after stimulation
mvoltS <- 3 #starting stimulation mV
mvoltE <- 8 #final stimulation mV

saveTable <- FALSE #If you want to save the table, type TRUE here

####### Graph Options #########################
lwl <- -15 #lower y-axis limit for graphs
upl <- 15 #uppler y-axis limit for graphs
idColor <- "pink" #line color for individual stimulation
meanColor <- "purple" #line color for mean signal per stimulation

#Loading required packages
dplyrEx <- require(dplyr)
if(!dplyrEx){
  install.packages("dplyr")
  library(dplyr)
}

ggplotEx <- require(ggplot2)
if(!dplyrEx){
  install.packages("ggplot2")
  library(ggplot2)
}

#Setting Working Folder
if(!exists("pickFolder")){
  exFileName <- file.choose()
  setwd(dirname(exFileName))
  pickFolder <- TRUE
}

#Loading raw data
fList <- Sys.glob("*.txt")
rawTable <- do.call("rbind",
                    lapply(fList,
                           function(fn) data.frame(FileName = fn,
                                                   read.table(fn, header = TRUE, fill = TRUE,
                                                              stringsAsFactors = FALSE))))
rawTable$Time <- as.numeric(rawTable$Time)
rawTable$EMG <- as.numeric(rawTable$EMG)
rawTable$Stim <- as.numeric(rawTable$Stim)

rStim <- which(!rawTable$Stim == 0)
rawTable$ROI <- rawTable$Stim
stimTable <- NULL
for (stimN in c(1:length(rStim))) {
  rawTable$ROI[rStim[stimN]:(rStim[stimN] + frameNum - 1)] <- rawTable$ROI[rStim[stimN]]
  stimTable <- rbind(stimTable, rawTable[rStim[stimN]:(rStim[stimN] + frameNum - 1),])
}
rm(rawTable) #Clearing imported rawdata to save memory

#Adding sample number and assay date to data frame
splitPos <- regexpr("\\ \\(", stimTable$FileName)
stimTable$SampleName <- factor(substr(stimTable$FileName, 1, splitPos-1))
stimTable$Date <- factor(substr(stimTable$FileName, splitPos + 2, splitPos + 9))

#Separate differnt mouse
splNum <- levels(stimTable$SampleName)
splLst <- NULL
for (i in c(1:length(splNum))) {
  splLst[[i]] <- stimTable[stimTable$SampleName == splNum[i],]
}

#Creating Relative Timeline to stimulation for plotting
timeList <- stimTable$Time[1:frameNum] - stimTable$Time[1]

#Plotting as you go
if (!dir.exists("./Graphs")) {
  dir.create("Graphs")
}
for (j in c(1:length(splNum))) {
  for (i in c(mvoltS:mvoltE)) {
    plotTemp <- as.data.frame(matrix(splLst[[j]]$EMG[(splLst[[j]]$ROI == i)], nrow = frameNum))
    plotTemp$relTime <- timeList
    plotTempSP <- apply(plotTemp, 2,spline, n = ncol(plotTemp)*10) #smoothen the line
    pdf(paste0("./Graphs/",paste("The",splNum[j],"on",i,"mV",".pdf", sep = "\ ")))
    plot(x = plotTempSP[["relTime"]][[2]], y = plotTempSP[[1]][[2]],
         ylim = c(lwl,upl),
         xlim = c(0,timeList[frameNum]*1.2),
         xlab = "Relative Time to Stimulation",
         ylab = "mV",
         cex.lab = 1.2,
         font.lab = 2,
         col = "white", frame = FALSE,
         main = paste("The",splNum[j],"on",i,"mV", sep = "\ "))
    for (k in c(1:(length(plotTempSP)-1))) {
      if (length(plotTempSP) > 1) {
        lines(x = plotTempSP[["relTime"]][[2]], y = plotTempSP[[k]][[2]], col = idColor)
      }
      if (length(plotTempSP) > 1) {
        lines(x = plotTempSP[["relTime"]][[2]],
              y = spline(rowMeans(plotTemp[,-ncol(plotTemp)]), n = ncol(plotTemp)*10)[[2]],
              col = meanColor, lwd = 1.8)}
    }
    dev.off()
  }}