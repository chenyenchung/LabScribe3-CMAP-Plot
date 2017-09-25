####### General Setting ########################
AssayDate <- "20170603"
Age <- 30 # Age of mouse when assayed
AgeUnit <- "Week"
splitChr <- " (" # Anything comes before this in the file name would be seen as mouse name

####### Experiment Paramenters  ################
sampletype <- c("WT","ALS")
rescue <- "Rescue"
frameNum <- 30  # How many frames to plot
prelude <- 5  # How many frames before stimulation
pickN <- 5 # Pick top N balanced wave

####### Output Setting #########################
savefolder <- "~/Desktop/CMAP_data/"