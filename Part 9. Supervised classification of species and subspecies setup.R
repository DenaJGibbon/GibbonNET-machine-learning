library(caret)
library(stringr)
library(tuneR)
library(seewave)
set.seed(13)

# Will perform k-fold validation, dividing evenly into 5 folds 
k <- 5

# Create the original k-fold split ----------------------------------------
# List the files
TrainingFolders <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/northerngrey_muellers',
                              full.names = T,recursive = T)

TrainingFoldersShort <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/northerngrey_muellers',
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
  
  OutputDirTest <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/northerngrey_muellers/', 'fold_', a, '/test/', sep='')
  dir.create(OutputDirTest, recursive=T)
  
  for(b in 1:length(FolderNamesUnique)){
    dir.create( paste(OutputDirTest, FolderNamesUnique[b],sep=''), recursive=T)
  }
  
  file.copy(from= TrainingFolders[TestFold], to=paste( OutputDirTest,ShortNames[TestFold],sep='' ))
  
  OutputDirTrain <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/northerngrey_muellers/', 'fold_', a, '/train/', sep='')
  dir.create(OutputDirTrain, recursive=T)
  
  for(b in 1:length(FolderNamesUnique)){
    dir.create( paste(OutputDirTrain, FolderNamesUnique[b],sep=''), recursive=T)
  }
  
  for(c in 1:length(TrainingFoldsMinusTest)){
    file.copy(from= TrainingFolders[ TrainingFoldsMinusTest[[c]]], to=paste( OutputDirTrain,ShortNames[ TrainingFoldsMinusTest[[c]]],sep='' ))
  }
}


# Evaluate PerformanceHFHM ----------------------------------------------------

PerformanceHFHMFolders <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/northerngrey_muellers/randomization',
                                 full.names = T)

CombinedFoldPerformanceHFHM <- data.frame()

for(a in 1:length(PerformanceHFHMFolders)) {
  SingleFolder <- list.files(PerformanceHFHMFolders[a],full.names = T) 
  
  BirdNETOutput <- SingleFolder[str_detect(SingleFolder,'BirdNETOutput')]
  
  for(b in 1:length(BirdNETOutput)){
    FullPaths <-   list.files(BirdNETOutput[b],full.names = T,recursive = T)
    TrainingData <- str_split_fixed(BirdNETOutput[b],pattern = 'randomization/',n=2)[,2]  
    TrainingData <- str_split_fixed(TrainingData,pattern = '/',n=2)[,1]  
  
    ClipDetections <- FullPaths
    
    ClipDetectionsShort <-dirname(FullPaths)
    
    ActualLabelAll <-str_split_fixed(ClipDetectionsShort,pattern = 'BirdNETOutput',n=2)[,2]
    ActualLabelAll <-str_split_fixed(ActualLabelAll,pattern = '/',n=2)[,2]
    
    BirdNETPerformanceHFHMDF <- data.frame()
    
    for(c in 1: length(ClipDetections)){
      
      TempDF <- read.delim(ClipDetections[c])
      
      
      TempDF <-  subset(TempDF,Common.Name!='nocall' )
      
      # Find the highest confidence for each clip
      if(nrow(TempDF) > 0){
        ActualLabel <- ActualLabelAll[c]
        PredictedLabel <- TempDF$Species.Code
        Confidence <- TempDF$Confidence
      } else{
        TempDF <- read.delim(ClipDetections[c])
        
        
        Confidence <- 1-max(TempDF$Confidence)
        PredictedLabel <- 'noise'
        ActualLabel <- ActualLabelAll[c]
      }
      
      TempRow <- cbind.data.frame(Confidence, ActualLabel,PredictedLabel)
      TempRow$FileName <-ClipDetectionsShort[c]
      
      
      BirdNETPerformanceHFHMDF <- rbind.data.frame(BirdNETPerformanceHFHMDF,TempRow)
    }
    
    
    caretConf <- caret::confusionMatrix(
      as.factor(BirdNETPerformanceHFHMDF$PredictedLabel),
      as.factor(BirdNETPerformanceHFHMDF$ActualLabel),
      mode = 'everything')
    
    CaretDF <- caretConf$byClass[5:7]
    
    Accuracy <-    caretConf$overall[1]
    # Convert predicted probabilities to a matrix
    Fold <- TrainingData
    TempRow <- cbind.data.frame(t(CaretDF),Accuracy,Fold,TrainingData)
    TempRow$Class <- rownames(TempRow)
    
    CombinedFoldPerformanceHFHM <- rbind.data.frame(CombinedFoldPerformanceHFHM,TempRow )
  }
}

CombinedFoldPerformanceHFHM$N.samples <- as.factor(str_split_fixed(CombinedFoldPerformanceHFHM$Fold,pattern = '_',n=3)[,3])


# Convert to a factor and specify the desired order
CombinedFoldPerformanceHFHM$N.samples  <- factor(CombinedFoldPerformanceHFHM$N.samples , levels = c(4, 8, 16, 32))



ggboxplot(data=CombinedFoldPerformanceHFHM,x='Class', y='F1',facet.by   ='TrainingData',
          color = 'N.samples')

ggboxplot(data=CombinedFoldPerformanceHFHM,x='N.samples', y='Accuracy',color   ='TrainingData')


