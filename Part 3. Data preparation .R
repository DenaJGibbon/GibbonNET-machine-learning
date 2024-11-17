library(caret)
library(stringr)
library(tuneR)
library(seewave)
set.seed(13)

# Will perform k-fold validation, dividing evenly into 5 folds 
k <- 5

# Create the original k-fold split ----------------------------------------
# List the files
TrainingFolders <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Train/',
                              full.names = T,recursive = T)

TrainingFoldersShort <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Train/',
                              full.names = F,recursive = T)

ShortNames <- str_split_fixed(TrainingFoldersShort,pattern = '/', n=2)[,2]

FolderNames <- str_split_fixed(ShortNames,pattern = '/', n=2)[,1]
FolderNamesUnique <- unique(FolderNames )

# Create k-fold 
TrainingFolds <- createFolds(TrainingFolders, k = k, list = TRUE, returnTrain = FALSE)

# Loop to save files
for(a in 1:length(TrainingFolds)){
  
 TestFold <-  TrainingFolds[[a]]
 
 Removed <- setdiff(1:length(TrainingFolds) , a)

 TrainingFoldsMinusTest <-  TrainingFolds[Removed]
 
 OutputDirTest <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds/', 'fold_', a, '/test/', sep='')
 dir.create(OutputDirTest, recursive=T)
 
 for(b in 1:length(FolderNamesUnique)){
   dir.create( paste(OutputDirTest, FolderNamesUnique[b],sep=''), recursive=T)
 }
 
 file.copy(from= TrainingFolders[TestFold], to=paste( OutputDirTest,ShortNames[TestFold],sep='' ))
 
 OutputDirTrain <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds/', 'fold_', a, '/train/', sep='')
 dir.create(OutputDirTrain, recursive=T)
 
 for(b in 1:length(FolderNamesUnique)){
   dir.create( paste(OutputDirTrain, FolderNamesUnique[b],sep=''), recursive=T)
 }

 for(c in 1:length(TrainingFoldsMinusTest)){
 file.copy(from= TrainingFolders[ TrainingFoldsMinusTest[[c]]], to=paste( OutputDirTrain,ShortNames[ TrainingFoldsMinusTest[[c]]],sep='' ))
 }
}




# Create augmented data: 3-sec clips ----------------------------------------
Folds <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds/',
           full.names = T)

for(a in 1:length(Folds)){
  
 TrainingFolders <- list.files(paste(Folds[a],'train', sep='/'),full.names = T)
  
 OutputDirTrain <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds-3secsplit/', basename(Folds[a]),'/train/', sep='')
 dir.create(OutputDirTrain, recursive=T)
  
 FolderNamesUnique <-  basename(TrainingFolders)
 
 for(b in 1:length(FolderNamesUnique)){
   dir.create( paste(OutputDirTrain, FolderNamesUnique[b],sep=''), recursive=T)

 }
 
 for(c in 1:length(TrainingFolders)){
   
   TrainingWavslist <-list.files(TrainingFolders[c], full.names = T)
   
   OutputDir <-paste(OutputDirTrain, FolderNamesUnique[c],sep='')

   
   for(d in 1:length(TrainingWavslist)){
     print(d)
     TempWav <- readWave(TrainingWavslist[d])
     
     if(duration(TempWav) >= 3.1){
     Seq.length <- seq(0.1,duration(TempWav),3)
     

     Shortwav.files <- lapply(1:(length(Seq.length) - 1),
                              function(i)
                                extractWave(
                                  TempWav,
                                  from = Seq.length[i],
                                  to = Seq.length[i +1],
                                  xunit = c("time"),
                                  plot = F,
                                  output = "Wave"
                                ))
     
     ShortName <-  basename(TrainingWavslist[d])
     
     lapply(1:length(Shortwav.files),
            function(i)
              writeWave(
                Shortwav.files[[i]],
                filename = paste(
                  OutputDir, '/',
                  ShortName, '_',
                  Seq.length[i], '_',
                  '.wav',
                  sep = ''
                ),
                extensible = FALSE
              ))
     
     } else {
       
       writeWave(
         TempWav,
         filename = paste(
           OutputDir, '/',
           ShortName, '_',
           'original', '_',
           '.wav',
           sep = ''
         ),
         extensible = FALSE
       )
       
     }
     
   }
   
   
 }
 
   
   
   
 } 
 
# Create augmented data: time shift ----------------------------------------

Folds <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds/',
                    full.names = T)

for(a in 1:length(Folds)){
  
  TrainingFolders <- list.files(paste(Folds[a],'train', sep='/'),full.names = T)
  
  OutputDirTrain <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds-TimeShift/', basename(Folds[a]),'/train/', sep='')
  dir.create(OutputDirTrain, recursive=T)
  
  FolderNamesUnique <-  basename(TrainingFolders)
  
  for(b in 1:length(FolderNamesUnique)){
    dir.create( paste(OutputDirTrain, FolderNamesUnique[b],sep=''), recursive=T)
    
  }
  
  for(c in 1:length(TrainingFolders)){
    
    TrainingWavslist <-list.files(TrainingFolders[c], full.names = T)
    
    OutputDir <-paste(OutputDirTrain, FolderNamesUnique[c],sep='')
    
    
    for(d in 1:length(TrainingWavslist)){
      print(d)
      
      TempWav <- readWave(TrainingWavslist[d])
      CombinedWav1 <- Wave(left = TempWav@left, samp.rate = TempWav@samp.rate, bit = TempWav@bit, pcm = TempWav@pcm)

      N.samples..sec <- TempWav@samp.rate
      
      RandomShift <- runif(1,-N.samples..sec,N.samples..sec)
      
      if(RandomShift < 0){
        
        RandomShift <-  abs(RandomShift)
        CombinedWav1@left <-  c(rep(0,round(RandomShift,0)),TempWav@left)
        
      } else {
        
        CombinedWav1@left <-  c(TempWav@left,rep(0,round(RandomShift,0)))
        
      }
      
      Shortwav.files <- c(CombinedWav1)
      
      ShortName <-  basename(TrainingWavslist[d])
        
        lapply(1:length(Shortwav.files),
               function(i)
                 writeWave(
                   Shortwav.files[[i]],
                   filename = paste(
                     OutputDir, '/',
                     ShortName, '_',
                     Seq.length[i], '_',
                     '.wav',
                     sep = ''
                   ),
                   extensible = FALSE
                 ))
        
      
    }
    
    
  }
  
  
} 



# All training clips time shift -------------------------------------------

OutputDir  <- '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAllClips/Train_all_timeshift/'

WavDir <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Train',
                     full.names = T)

for(a in 7:length(WavDir)){
  
 TempList <-  list.files(WavDir[a],
             full.names = T)
  
  FolderNamesUnique <-  basename(TempList)
  FolderNamesUnique <- str_split_fixed(FolderNamesUnique,pattern = '_', n=2)[,1]
  
  for(b in 1:length(FolderNamesUnique)){
    dir.create( paste(OutputDir, FolderNamesUnique[b],sep=''), recursive=T)
    
  }
  
  for(c in 1:length(TempList)){
    
    TrainingWavslist <-list.files(TempList[c], full.names = T)
    
    OutputFolder <-paste(OutputDir, FolderNamesUnique[c],sep='')
    
    for(d in 1:length(TrainingWavslist)){
      print(d)
      
      TempWav <- readWave(TrainingWavslist[d])
      CombinedWav1 <- Wave(left = TempWav@left, samp.rate = TempWav@samp.rate, bit = TempWav@bit, pcm = TempWav@pcm)
      
      N.samples..sec <- TempWav@samp.rate
      
      RandomShift <- runif(1,-N.samples..sec,N.samples..sec)
      
      if(RandomShift < 0){
        
        RandomShift <-  abs(RandomShift)
        CombinedWav1@left <-  c(rep(0,round(RandomShift,0)),TempWav@left)
        
      } else {
        
        CombinedWav1@left <-  c(TempWav@left,rep(0,round(RandomShift,0)))
        
      }
      
      Shortwav.files <- c(CombinedWav1)
      
      ShortName <-  basename(TrainingWavslist[d])
      
      lapply(1:length(Shortwav.files),
             function(i)
               writeWave(
                 Shortwav.files[[i]],
                 filename = paste(
                   OutputFolder, '/',
                   ShortName, '_',
                   Seq.length[i], '_',
                   '.wav',
                   sep = ''
                 ),
                 extensible = FALSE
               ))
      
      
    }
    
    
  }
  
  
}



