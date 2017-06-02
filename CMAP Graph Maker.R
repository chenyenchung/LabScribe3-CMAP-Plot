####### Experiment Paramenters ################
frameNum <- 25 #How many frames to plot after stimulation
mvoltS <- 3 #starting stimulation mA
mvoltE <- 8 #final stimulation mA

saveTable <- TRUE #If you want to save the summary table, type TRUE here
drawGraphs <- TRUE #If you want to save the graphs, type TRUE here
batchTagging <- "20170603"
####### Graph Options #########################
lwl <- -15 #lower y-axis limit for graphs
upl <- 15 #uppler y-axis limit for graphs
smoothening <- 10 #To smoothen the curve or not (no smoothing = 0)
idColor <- "aliceblue" #line color for individual stimulation
meanColor <- "black" #line color for mean signal per stimulation
meanLwd <- 1.8 #line thickness for mean signal per stimulation

#Loading required packages
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

rs2Ex <- require(reshape2)
if(!rs2Ex){
  install.packages("reshape2")
  library(reshape2)
}

#Varible type check
if(!is.numeric(frameNum)){
  warning("Frame number should be a number!")
  stop()
}
if(!is.numeric(mvoltS)){
  warning("Stimulation should be a number!")
  stop()
}
if(!is.numeric(mvoltE)){
  warning("Stimulation should be a number!")
  stop()
}
if(!is.numeric(lwl)){
  warning("Graph lower limit should be a number!")
  stop()
}
if(!is.numeric(upl)){
  warning("Graph upper limit should be a number!")
  stop()
}
if(!is.numeric(smoothening)){
  warning("Level of smoothening should be a number! (Suggested: 10)")
  stop()
}
if(!is.logical(saveTable)){
  warning("If you want to save the tables, put TRUE in saveTable.")
  stop()
}
if(!is.logical(drawGraphs)){
  warning("If you want to save the graphs, put TRUE in drawGraphs.")
  stop()
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

#Adding sample number, assay date, and sample type to data frame
if(!"SampleName" %in% colnames(stimTable)){
  splitPos <- regexpr("\\ \\(", stimTable$FileName)
  stimTable$SampleName <- substr(stimTable$FileName, 1, splitPos-1)
}
stimTable$SampleName <- as.factor(stimTable$SampleName)
if (!"Batch" %in% colnames(stimTable)) {
  stimTable$Batch <-batchTagging
}
stimTable$Batch <- as.factor(stimTable$Batch)
if (!"msTime" %in% colnames(stimTable)) {
  stimTable$msTime <- stimTable$Time * 1000
}

#Separate differnt mouse
splNum <- levels(stimTable$SampleName)
splLst <- NULL
for (i in c(1:length(splNum))) {
  splLst[[i]] <- stimTable[stimTable$SampleName == splNum[i],]
}

#Creating Relative Timeline to stimulation for plotting
timeList <- stimTable$msTime[1:frameNum] - stimTable$msTime[1]

#Plotting as you go
if (!dir.exists("./Graphs")) {
  dir.create("Graphs")
}
if (drawGraphs) {
  for (j in c(1:length(splNum))) {
    for (l in c(1:length(levels(splLst[[j]]$Batch)))){
      for (i in c(mvoltS:mvoltE)){
        checkId <- (splLst[[j]]$ROI == i) &
                    (splLst[[j]]$Batch == levels(splLst[[j]]$Batch)[l])
        plotTemp <- as.data.frame(
          matrix(
            splLst[[j]]$EMG[checkId],
            nrow = frameNum))
        plotTemp$relTime <- timeList
        plotTempSP <- apply(plotTemp, 2,spline, n = ncol(plotTemp)*(smoothening+1)) #smoothen the line
        pdf(paste0("./Graphs/",paste("The",splNum[j],"on",i,"mA","-",
                                     levels(splLst[[j]]$Batch)[l],".pdf", sep = "\ ")))
        plot(x = plotTempSP[["relTime"]][[2]], y = plotTempSP[[1]][[2]],
             ylim = c(lwl,upl),
             xlim = c(0,timeList[frameNum]*1.2),
             xlab = "Relative Time to Stimulation (ms)",
             ylab = "Compound Muscle Action Potential (mV)",
             cex.lab = 1.2,
             font.lab = 2,
             col = "white", frame = FALSE,
             main = paste("The",splNum[j],"on",i,"mA","-",levels(splLst[[j]]$Batch)[l], sep = "\ "))
        for (k in c(1:(length(plotTempSP)-1))) {
          if (length(plotTempSP) > 1) {
            lines(x = plotTempSP[["relTime"]][[2]], y = plotTempSP[[k]][[2]], col = idColor)
            lines(x = plotTempSP[["relTime"]][[2]],
                  y = spline(rowMeans(plotTemp[,-ncol(plotTemp)]),
                             n = ncol(plotTemp)*(smoothening+1))[[2]],
                  col = meanColor, lwd = meanLwd)
          }
        }
        dev.off() 
      }
    }}
}

#Plot mean graph for every assay
if(drawGraphs){
  for (i in c(1:length(splNum))) {
    splLst[[i]]$relTime <- timeList
    for (k in c(1:length(levels(splLst[[i]]$Batch)))) {
      tempDf <- NULL
      cName <- NULL
      for (j in c(mvoltS:mvoltE)){
        if(j %in% splLst[[i]]$ROI){
          check <- splLst[[i]]$ROI == j &
            (splLst[[i]]$Batch == levels(splLst[[i]]$Batch)[k])
          tempDf <- cbind(tempDf,
                          tapply(splLst[[i]][check,]$EMG,
                                 splLst[[i]][check,]$relTime,
                                 mean))
          cName <- c(cName, paste0(j,"mA"))
        }
      }
    }
    colnames(tempDf) <- cName
    tempDf <- as.data.frame(tempDf)
    tempDf$Time <- rownames(tempDf)
    tempDfSp <- as.data.frame(sapply(
      apply(tempDf, 2, spline, n = nrow(tempDf)*(smoothening+1)), "[",2 ))
    
    if(drawGraphs){
      pdf(paste0("./Graphs/",paste("The",splNum[i],"-",
                                   levels(splLst[[i]]$Batch)[k],".pdf", sep = "\ ")))
      print(
        ggplot(melt(tempDfSp, id.vars = "Time.y"),
               aes(Time.y, value, colour = variable)) +
          geom_line() +
          scale_color_discrete(name = "Stimulation",
                               breaks = levels(melt(tempDfSp, id.vars = "Time.y")$variable),
                               labels = colnames(tempDf)[-length(colnames(tempDf))]) +
          xlab("Relative Time to Stimulation (ms)") +
          ylab("Compound Muscle Action Potential (mV)") +
          ggtitle(
            paste("CMAP","The", splNum[i]),
            subtitle = levels(splLst[[i]]$Batch)[k]
          ) +
          theme_minimal()
      )
      dev.off()
    }
  }
}

#Plot mean graph for defined mA for different type#####

#Save table
if (saveTable) {
  if (!dir.exists("./Tables")) {
    dir.create("Tables")
  }
  write.table(stimTable,
              paste0("./Tables/",batchTagging, "- Raw ROI table.txt")
              ) #Save raw ROI Table
  #Calculate amplitude and save table
  for (j in c(1:length(splNum))) {
    for (l in c(1:length(levels(splLst[[j]]$Batch)))){
      average <- NULL
      amp <- NULL
      stdev <- NULL
      ampsd <- NULL
      ampav <- NULL
      for (i in c(mvoltS:mvoltE)){
        checkIdsum <- (splLst[[j]]$ROI == i) &
          (splLst[[j]]$Batch == levels(splLst[[j]]$Batch)[l])
        plotTempsum <- as.data.frame(
          matrix(
            splLst[[j]]$EMG[checkIdsum],
            nrow = frameNum))
        if (ncol(plotTempsum) > 1) {
          average[[i]] <- rowMeans(plotTempsum)
          amp[[i]] <- (sweep(apply(plotTempsum, 2, range),2,apply(plotTempsum, 2, range)[1,])[2,])/2
          stdev[[i]] <- apply(plotTempsum, 1, sd)
          ampav[i] <- mean(amp[[i]])
          ampsd[i] <- sd(amp[[i]])
        }
      }
      fnSeed <- paste(splNum[j],"-",splLst[[j]]$Batch[l])
      sink(paste0("./Tables/",fnSeed,"-individual amplitude.txt"))
      print(amp)
      sink()
      sink(paste0("./Tables/",fnSeed,"-signal stdev.txt"))
      print(stdev)
      sink()
      sink(paste0("./Tables/",fnSeed,"-signal average.txt"))
      print(average)
      sink()
      write.table(
        data.frame("Average Amplitude" = ampav, "StdEv of Amplitude" = ampsd),
        paste0("./Tables/",fnSeed,"-amp and sd per assay.txt")
      )
    }}
}