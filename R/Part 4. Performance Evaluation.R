library(stringr)
library(ROCR)
library(pROC)
library(ggpubr)

PerformanceFolders1 <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds-3secsplit',full.names = T)
PerformanceFolders2 <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds',full.names = T)
PerformanceFolders3 <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/KFolds-TimeShift',full.names = T)
TrainingDataList <- c(rep('3secsplit',5),rep('Original',5),rep('TimeShift',5))

PerformanceFolders <- c(PerformanceFolders1,PerformanceFolders2,PerformanceFolders3)

CombinedFoldPerformance_allsamples <- data.frame()

for(a in 1:length(PerformanceFolders)) {
  SingleFolder <- list.files(PerformanceFolders[a],full.names = T) 
  
  BirdNETOutput <- SingleFolder[str_detect(SingleFolder,'BirdNETOutput')]
  
  
  ClipDetections <- list.files(BirdNETOutput,
                               recursive = T,full.names = T)
  
  ClipDetectionsShort <- list.files(BirdNETOutput,
                                    recursive = T,full.names = F)
  
  ActualLabelAll <-str_split_fixed(ClipDetectionsShort,pattern = '/',n=2)[,1]
  
  BirdNETPerformanceDF <- data.frame()
  
  for(b in 1: length(ClipDetections)){
    
    TempDF <- read.delim(ClipDetections[b])
   
    
    TempDF <-  subset(TempDF,Common.Name!='nocall' )
    
    # Find the highest confidence for each clip
    if(nrow(TempDF) > 0){
      ActualLabel <- ActualLabelAll[b]
      PredictedLabel <- TempDF$Species.Code
      Confidence <- TempDF$Confidence
    } else{
      TempDF <- read.delim(ClipDetections[b])
      
      
      Confidence <- 1-max(TempDF$Confidence)
      PredictedLabel <- 'noise'
      ActualLabel <- ActualLabelAll[b]
    }
    
    TempRow <- cbind.data.frame(Confidence, ActualLabel,PredictedLabel)
    TempRow$FileName <-ClipDetectionsShort[b]
    TempRow$uniquesamples <- a

    BirdNETPerformanceDF <- rbind.data.frame(BirdNETPerformanceDF,TempRow)
  }
  
 
  caretConf <- caret::confusionMatrix(
    as.factor(BirdNETPerformanceDF$PredictedLabel),
    as.factor(BirdNETPerformanceDF$ActualLabel),
    mode = 'everything')
    
  CaretDF <- caretConf$byClass[,5:7]
  
 Accuracy <-    caretConf$overall[1]
  # Convert predicted probabilities to a matrix

  roc_data <- multiclass.roc(BirdNETPerformanceDF$ActualLabel, BirdNETPerformanceDF$Confidence)
  auc_attribute <- attr(roc_data, "auc")
  
  auc_value <- auc(roc_data)
 
  auc_value <- as.numeric(auc_value)
 
  Fold <- basename(PerformanceFolders[a])
  TrainingData <- TrainingDataList[a]
  TempRow <- cbind.data.frame(CaretDF,auc_value,Accuracy,Fold,TrainingData)
  TempRow$Class <- rownames(TempRow)
  
  CombinedFoldPerformance_allsamples <- rbind.data.frame(CombinedFoldPerformance_allsamples,TempRow )
}


ggboxplot(data=CombinedFoldPerformance_allsamples,x='TrainingData', y='auc_value',color  ='TrainingData' )+ 
  ylim(0.5,1)+ylab("AUC")

ggboxplot(data=CombinedFoldPerformance_allsamples,x='Class', y='F1',color  ='TrainingData')

ggboxplot(data=CombinedFoldPerformance_allsamples,x='TrainingData', y='Accuracy',color  ='TrainingData')

