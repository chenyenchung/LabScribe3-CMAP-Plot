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
sumTable$Age <- factor(paste(AgeUnit, sumTable$Age))

# Draw an overview graph
if(!dir.exists("./Graph")){
  dir.create("./Graph")
}

pdf(paste0("./Graph/CMAP_Overview_",Sys.time(),".pdf"), paper = "a4r")
print(ggplot(sumTable, aes(x = fNum*2, y = mV)) + 
  geom_line(aes(colour = Type)) +
  facet_grid(ROI ~ Age) +
  scale_color_discrete(name = "Mouse Identity") +
  xlab("Time (ms)") +
  ylab("Compound Muscle Action Potential (mV)") +
  ggtitle("Summary") +
  theme_minimal())
dev.off()

sapply(levels(sumTable$ROI), function(x){
  pdf(paste0("./Graph/CMAP_",x,"_",Sys.time(),".pdf"), paper = "a4r")
  print(ggplot(filter(sumTable, ROI == x), aes(x = fNum*2, y = mV)) + 
          geom_line(aes(colour = Age)) +
          facet_grid(Type ~ .) +
          scale_color_discrete(name = "Age When Assayed") +
          xlab("Time (ms)") +
          ylab("Compound Muscle Action Potential (mV)") +
          ggtitle(x) +
          theme_minimal())
  dev.off()
})

sapply(levels(sumTable$Age), function(x){
  pdf(paste0("./Graph/CMAP_mA_on_Type_", x,"_",Sys.time(),".pdf"), paper = "a4r")
  print(ggplot(filter(sumTable, Age == x), aes(x = fNum*2, y = mV)) + 
          geom_line(aes(colour = ROI)) +
          facet_grid(Type ~ .) +
          scale_color_discrete(name = "Stimulation (mA)") +
          xlab("Time (ms)") +
          ylab("Compound Muscle Action Potential (mV)") +
          ggtitle(x) +
          theme_minimal())
  dev.off()
  sapply(levels(sumTable$ROI), function(y){
    pdf(paste0("./Graph/CMAP_Type_on_mA",y,"_",x,"_",Sys.time(),".pdf"), paper = "a4r")
    print(ggplot(filter(sumTable, ROI == y), aes(x = fNum*2, y = mV)) + 
            geom_line(aes(colour = Type)) +
            scale_color_discrete(name = "Mouse Identity") +
            xlab("Time (ms)") +
            ylab("Compound Muscle Action Potential (mV)") +
            ggtitle(paste(y,"on",x)) +
            theme_minimal())
    dev.off()
  })

})

if(!file.exists("~/Desktop/CMAP_data/summary.csv")){
  write.csv(sumTable, file = "~/Desktop/CMAP_data/summary.csv")
} else {
  write.csv(sumTable, file = paste0("~/Desktop/CMAP_data/summary_", Sys.time(), ".csv"))
}