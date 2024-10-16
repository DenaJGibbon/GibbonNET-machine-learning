# Part 3b. Sample size variation
# For each fold randomly select XX samples so have 5 repeats
PerformanceFolders1 <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds-3secsplit',full.names = T)
PerformanceFolders2 <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds',full.names = T)
PerformanceFolders3 <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds-TimeShift',full.names = T)

RandomSamples <- c(4,8,16,32)

