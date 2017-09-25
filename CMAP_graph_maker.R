source("settings.R")

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

# Load filtered data
setwd(savefolder)
fList <- list.files(pattern = "*.txt")
rawTable <- do.call(rbind,
                    lapply(fList,
                           function(fn) read.table(fn, header = TRUE, fill = TRUE,
                                                   stringsAsFactors = FALSE, comment.char = "")))
rawTable <- tbl_df(rawTable)
rawTable <- group_by(rawTable, Type, Age, ROI, fNum)
sumTable <- summarise(rawTable, mV = mean(EMG), sd = sd(EMG))
sumTable$ROI <- factor(paste(sumTable$ROI, "mA"))
sumTable$Age <- paste(AgeUnit, sumTable$Age)

# Draw an overview graph
pdf(paste0("./CMAP_Overview_",Sys.time(),".pdf"), paper = "a4r")
print(ggplot(sumTable, aes(x = fNum*2, y = mV)) + 
  geom_line(aes(colour = Type)) +
  facet_grid(ROI ~ Age) +
  scale_color_discrete(name = "Mouse Identity") +
  xlab("Time (ms)") +
  ylab("Compound Muscle Action Potential (mV)") +
  theme_minimal())
dev.off()

sapply(levels(sumTable$ROI), function(x){
  
})
