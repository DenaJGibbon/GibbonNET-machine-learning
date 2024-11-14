# Part 3b. Sample size variation
library(stringr)

# For each fold randomly select XX samples so have 5 repeats
PerformanceFolders1 <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds-3secsplit',full.names = T)
PerformanceFolders2 <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds',full.names = T)
PerformanceFolders3 <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds-TimeShift',full.names = T)

CombinedForRandomization <- c(PerformanceFolders1,PerformanceFolders2,PerformanceFolders3)

RandomSamples <- c(4,8,16,32)

for( a in 1:length(CombinedForRandomization)){
  
 TempListFiles <-  list.files(CombinedForRandomization[a],full.names = T)
   
 TempDir <- str_split_fixed(CombinedForRandomization[a],'OrxyGibbonAutomatedDetection/',n=2)[,2]
 WorkingDir <- '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/Randomization/'
 
 OutputDirTrain <- paste(WorkingDir, TempDir, sep='')
 dir.create(OutputDirTrain, recursive=T)
 
 TrainingFolders <- list.files(paste(CombinedForRandomization[a],'train', sep='/'),full.names = T)
 
 FolderNamesUnique <-  basename(TrainingFolders)
 
 
 for(b in 1:length(RandomSamples)){
   
 for(c in 1:length(FolderNamesUnique)){
   dir.create( paste(OutputDirTrain,'/', RandomSamples[b], '/', FolderNamesUnique[c],sep=''), recursive=T)
 }
 
 }
 
 
 for(d in 1:length(TrainingFolders)){
   for(e in 1:length(RandomSamples)){
  
  TempWavs <- list.files(TrainingFolders[d],full.names = T)
  TempWavsSub <- TempWavs[sample(1:length(TempWavs),RandomSamples[e],replace = F)]
  ShortWaves <- basename(TempWavsSub)
  
  file.copy(from= TempWavsSub, to=paste( OutputDirTrain, '/', RandomSamples[e],'/', basename(TrainingFolders[d]),'/',ShortWaves,sep='' ))
 }
 }
}
  
  
