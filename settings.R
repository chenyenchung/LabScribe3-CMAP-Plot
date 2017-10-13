####### General Setting ########################
AssayDate <- "20170810"
splitChr <- " (" # Anything comes before this in the file name would be seen as mouse name

####### Experiment Paramenters  ################
sampletype <- c("WT","ALS")
sampletreat <- c("mir-17~92","GFP")
rescue <- "Rescue"
frameNum <- 30  # How many frames to plot
prelude <- 5  # How many frames before stimulation
pickBal <- 15 # Pick top N balanced wave
pickPeak <- 5 # # Pick top N wave in amplitude

####### Output Setting #########################
savefolder <- "~/Desktop/CMAP_data/"