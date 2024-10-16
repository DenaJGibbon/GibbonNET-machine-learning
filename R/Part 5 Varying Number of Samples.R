# library(stringr)
library(ROCR)
library(pROC)
library(ggpubr)
library(plyr)

PerformanceFolders <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/Randomization/KFolds/BirdNET/',full.names = T)

CombinedFoldPerformance <- data.frame()

for(a in 1:length(PerformanceFolders)) {
  SingleFolder <- list.files(PerformanceFolders[a],full.names = T) 
  
  for(b in 1:length(SingleFolder)){
  FullPaths <-   list.files(SingleFolder[b],full.names = T)
  TrainingData <- str_split_fixed(SingleFolder[b],pattern = 'Randomization/',n=2)[,2]  
  TrainingData <- str_split_fixed(TrainingData,pattern = '/',n=2)[,1]  
  
  FoldAndN <- str_split_fixed(SingleFolder[b],pattern = 'BirdNET',n=2)[,2]  
  FoldAndN <- str_replace_all(FoldAndN,'//','')
  FoldAndN <- str_replace_all(FoldAndN,'/','_')
  
  BirdNETOutput <- FullPaths[str_detect(FullPaths,'BirdNETOutput')]
  
  
  ClipDetections <- list.files(BirdNETOutput,
                               recursive = T,full.names = T)
  
  ClipDetectionsShort <- list.files(BirdNETOutput,
                                    recursive = T,full.names = F)
  
  ActualLabelAll <-str_split_fixed(ClipDetectionsShort,pattern = '/',n=2)[,1]
  
  BirdNETPerformanceDF <- data.frame()
  
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
    TempRow$uniquesamples <- a
    TempRow$FoldAndN <- FoldAndN
    
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
  
  Fold <- FoldAndN
  TempRow <- cbind.data.frame(CaretDF,auc_value,Accuracy,Fold,TrainingData)
  TempRow$Class <- rownames(TempRow)
  
  CombinedFoldPerformance <- rbind.data.frame(CombinedFoldPerformance,TempRow )
}
}

CombinedFoldPerformance$N.samples <- as.factor(str_split_fixed(CombinedFoldPerformance$Fold,pattern = '_',n=3)[,3])


# Convert to a factor and specify the desired order
CombinedFoldPerformance$N.samples  <- factor(CombinedFoldPerformance$N.samples , levels = c(4, 8, 16, 32))

CombinedFoldPerformance$TrainingData <- revalue(CombinedFoldPerformance$TrainingData,
        c("KFolds"="Original"))

ggscatter(data=CombinedFoldPerformance,x='N.samples', y='auc_value',color  ='TrainingData' )+ 
  ylim(0.5,1)+ylab("AUC")

ggboxplot(data=CombinedFoldPerformance,x='Class', y='F1',facet.by   ='TrainingData',
          color = 'N.samples')

ggboxplot(data=CombinedFoldPerformance,x='N.samples', y='Accuracy',color   ='TrainingData')

# Combined dataframes
CombinedFoldPerformance_allsamples$N.samples <- 'all'

RandomAndAllDF <- rbind.data.frame(CombinedFoldPerformance,CombinedFoldPerformance_allsamples)

ggscatter(data=RandomAndAllDF,x='N.samples', y='auc_value',color  ='TrainingData' )+ 
  ylim(0.5,1)+ylab("AUC")

ggboxplot(data=RandomAndAllDF,x='Class', y='F1',facet.by   ='TrainingData',
          color = 'N.samples')

ggscatter(data=RandomAndAllDF,x='N.samples', y='Top-1 Accuracy',color   ='TrainingData')

