savefolder <- "~/Desktop/CMAP_data/"

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

rsEx <- require("reshape2")
if(!rsEx){
  install.packages("reshape2")
  library(reshape2)
}

# Load filtered data
setwd(savefolder)
fList <- list.files(pattern = "*.txt")
rawTable <- do.call(rbind,
                    lapply(fList,
                           function(fn) read.table(fn, header = TRUE, fill = TRUE,
                                                   stringsAsFactors = FALSE, comment.char = "")))
rawTable <- tbl_df(rawTable)
rawTable2 <- group_by(rawTable, Cat, Age, ROI, Count)
sumTable2 <- summarise(rawTable2, peak = (max(EMG) -min(EMG)))
peakTable <- dcast(sumTable2,  Cat + ROI+ Count ~ Age, value.var = "peak")
names(peakTable)[2] <- "Stimulation (mA)"
names(peakTable)[1] <- "Category"
peakTable <- peakTable[,-3]
rawTable <- group_by(rawTable, Cat, Age, ROI, fNum)
sumTable <- summarise(rawTable, mV = mean(EMG), sd = sd(EMG))
sumTable$ROI <- factor(paste(sumTable$ROI, "mA"))

# Draw an overview graph
if(!dir.exists("./Graph")){
  dir.create("./Graph")
}

pdf(paste0("./Graph/CMAP_Overview_",Sys.time(),".pdf"), paper = "a4r")
print(ggplot(sumTable, aes(x = fNum*2, y = mV)) + 
  geom_line(aes(colour = Cat)) +
  facet_grid(ROI ~ Age) +
  scale_color_discrete(name = "Mouse Identity") +
  xlab("Time (ms)") +
  ylab("Compound Muscle Action Potential (mV)") +
  ggtitle("Summary") +
  theme_minimal())
dev.off()

sapply(levels(sumTable$ROI), function(x){
  pdf(paste0("./Graph/CMAP_by_Age_",x,"_",Sys.time(),".pdf"), paper = "a4r")
  print(ggplot(filter(sumTable, ROI == x), aes(x = fNum*2, y = mV)) + 
          geom_line(aes(colour = Age)) +
          scale_color_discrete(name = "Age When Assayed") +
          xlab("Time (ms)") + facet_grid(Cat ~ .) +
          ylab("Compound Muscle Action Potential (mV)") +
          ggtitle(x) +
          theme_minimal())
  dev.off()
  
  pdf(paste0("./Graph/CMAP_by_Cat",x,"_",Sys.time(),".pdf"), paper = "a4r")
  print(ggplot(filter(sumTable, ROI == x), aes(x = fNum*2, y = mV)) + 
          geom_line(aes(colour = Cat)) +
          scale_color_discrete(name = "Sample Type") +
          xlab("Time (ms)") + facet_grid(Age ~ .) +
          ylab("Compound Muscle Action Potential (mV)") +
          ggtitle(x) +
          theme_minimal())
  dev.off()
})

pdf(paste0("./Graph/Peak_Overview_",Sys.time(),".pdf"), width = 12, height = 8)
print(
  ggplot(data = sumTable2, aes(x = Age, y = peak)) +
    geom_jitter(aes(col = Cat), position = position_dodge(width = 0.9), size = 0.7, alpha = 0.7)+ 
    geom_boxplot(aes(fill = Cat), lwd = 0.2,
                 position = position_dodge(width = 0.9), outlier.shape = NA) +
    facet_wrap(~ROI) +
    theme_classic(base_size = 16)
)
dev.off()

if(!file.exists("~/Desktop/CMAP_data/summary.csv")){
  write.csv(sumTable, file = "~/Desktop/CMAP_data/summary.csv")
  write.csv(peakTable, file = "~/Desktop/CMAP_data/peaks.csv")
} else {
  write.csv(sumTable, file = paste0("~/Desktop/CMAP_data/summary_", Sys.time(), ".csv"))
  write.csv(peakTable, file = paste0("~/Desktop/CMAP_data/peaks_", Sys.time(), ".csv"))
}
