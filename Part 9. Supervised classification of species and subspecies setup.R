library(caret)
library(stringr)
library(tuneR)
library(seewave)
library(reshape2)
library(plyr)
set.seed(13)

#Will perform k-fold validation, dividing evenly into 5 folds
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

# # Create k-fold
# TrainingFolds <- createFolds(TrainingFolders, k = k, list = TRUE, returnTrain = FALSE)
# 
# # Loop to save files
# for(a in 1:length(TrainingFolds)){
# 
#   TestFold <-  TrainingFolds[[a]]
# 
#   Removed <- setdiff(1:length(TrainingFolds) , a)
# 
#   TrainingFoldsMinusTest <-  TrainingFolds[Removed]
# 
#   OutputDirTest <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/northerngrey_muellers/', 'fold_', a, '/test/', sep='')
#   dir.create(OutputDirTest, recursive=T)
# 
#   for(b in 1:length(FolderNamesUnique)){
#     dir.create( paste(OutputDirTest, FolderNamesUnique[b],sep=''), recursive=T)
#   }
# 
#   file.copy(from= TrainingFolders[TestFold], to=paste( OutputDirTest,ShortNames[TestFold],sep='' ))
# 
#   OutputDirTrain <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/northerngrey_muellers/', 'fold_', a, '/train/', sep='')
#   dir.create(OutputDirTrain, recursive=T)
# 
#   for(b in 1:length(FolderNamesUnique)){
#     dir.create( paste(OutputDirTrain, FolderNamesUnique[b],sep=''), recursive=T)
#   }
# 
#   for(c in 1:length(TrainingFoldsMinusTest)){
#     file.copy(from= TrainingFolders[ TrainingFoldsMinusTest[[c]]], to=paste( OutputDirTrain,ShortNames[ TrainingFoldsMinusTest[[c]]],sep='' ))
#   }
# }


# Evaluate PerformanceHFHM ----------------------------------------------------

PerformanceHFHMFolders <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/northerngrey_muellers/randomization',
                                 full.names = T)

CombinedFoldPerformanceHFHM <- data.frame()
CaretConfusionMatrix <- list()
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
    
    CaretConfusionMatrix[[a]] <-  caretConf$table
    
    CaretDF <- caretConf$byClass[5:7]
    
    Accuracy <-    caretConf$overall[1]
    # Convert predicted probabilities to a matrix
    Fold <- TrainingData
    TempRow <- cbind.data.frame(t(CaretDF),Accuracy,Fold,TrainingData)
    TempRow$Class <- rownames(TempRow)
    
    CombinedFoldPerformanceHFHM <- rbind.data.frame(CombinedFoldPerformanceHFHM,TempRow )
  }
}



ggboxplot(data=CombinedFoldPerformanceHFHM,x='Fold', y='F1',
          color = 'Fold')



# Confusion matrix hylobates ----------------------------------------------
# Use Reduce to sum up all the tables element-wise
sum_of_tables <- Reduce(`+`, CaretConfusionMatrix)

# Divide the sum by the number of tables to get the mean
mean_table <- sum_of_tables / length(CaretConfusionMatrix)

# View the result
MeanTable <- round(mean_table,0)

# Calculate row sums (Reference totals)
row_totals <- colSums(MeanTable)  # Exclude the first column (Predictions)

# Normalize the confusion matrix by dividing each element by the corresponding class total
normalized_confusion_matrix <- as.data.frame(t(apply(MeanTable, 1, function(x) x / row_totals)))

# Add the 'Prediction' column back to the normalized matrix
normalized_confusion_matrix$Prediction <- rownames(sum_of_tables)

# Reshape the data into long format for ggplot
confusion_matrix_long <- melt(normalized_confusion_matrix, id.vars = "Prediction")

# Rename columns for clarity
colnames(confusion_matrix_long) <- c("Reference", "Prediction", "Count")

confusion_matrix_long$Reference <- revalue(as.factor(confusion_matrix_long$Reference),
                                           c('HF_FG'= 'Northern grey','HM_FG'='Müller'))

confusion_matrix_long$Prediction <- revalue(as.factor(confusion_matrix_long$Prediction),
                                            c('HF_FG'= 'Northern grey','HM_FG'='Müller'))

hfhmmean <- round(mean(CombinedFoldPerformanceHFHM$Accuracy),2)

# Generate the heatmap
HMHFConfusion <- ggplot(confusion_matrix_long, aes(y = Reference, x = Prediction, fill = Count)) +
  geom_tile() +
  scale_fill_distiller(palette = "Greys", direction = 1, name = 'Proportion')+
  theme_minimal() +
  labs(title = paste("Müller + Northern Grey \n mean accuracy",hfhmmean), x = "Reference", y = "Prediction") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Lar gibbons -------------------------------------------------------------

# TrainingFolders <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/largibbons/training_data/',
#                               full.names = T,recursive = T)
# 
# TrainingFoldersShort <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/largibbons/training_data/',
#                                    full.names = F,recursive = T)
# 
# ShortNames <- TrainingFoldersShort
# 
# FolderNames <- str_split_fixed(TrainingFoldersShort,pattern = '/',n=2)[,1]
# FolderNamesUnique <- unique(FolderNames )
# 
# # Create k-fold
# TrainingFolds <- createFolds(TrainingFolders, k = k, list = TRUE, returnTrain = FALSE)
# 
# # Loop to save files
# for(a in 1:length(TrainingFolds)){
# 
#   TestFold <-  TrainingFolds[[a]]
# 
#   Removed <- setdiff(1:length(TrainingFolds) , a)
# 
#   TrainingFoldsMinusTest <-  TrainingFolds[Removed]
# 
#   OutputDirTest <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/largibbons/randomization/', 'fold_', a, '/test/', sep='')
#   dir.create(OutputDirTest, recursive=T)
# 
#   for(b in 1:length(FolderNamesUnique)){
#     dir.create( paste(OutputDirTest, FolderNamesUnique[b],sep=''), recursive=T)
#   }
# 
#   file.copy(from= TrainingFolders[TestFold], to=paste( OutputDirTest,ShortNames[TestFold],sep='' ))
# 
#   OutputDirTrain <- paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/largibbons/randomization/', 'fold_', a, '/train/', sep='')
#   dir.create(OutputDirTrain, recursive=T)
# 
#   for(b in 1:length(FolderNamesUnique)){
#     dir.create( paste(OutputDirTrain, FolderNamesUnique[b],sep=''), recursive=T)
#   }
# 
#   for(c in 1:length(TrainingFoldsMinusTest)){
#     file.copy(from= TrainingFolders[ TrainingFoldsMinusTest[[c]]], to=paste( OutputDirTrain,ShortNames[ TrainingFoldsMinusTest[[c]]],sep='' ))
#   }
# }

# Evaluate PerformanceHFHM ----------------------------------------------------

PerformancelarFolders <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/BehavioralEcologyClips/largibbons/randomization',
                                     full.names = T)

CombinedFoldPerformancelar <- data.frame()
CaretConfusionMatrixlar <- list()
for(a in 1:length(PerformancelarFolders)) {
  SingleFolder <- list.files(PerformancelarFolders[a],full.names = T) 
  
  BirdNETOutput <- SingleFolder[str_detect(SingleFolder,'BirdNETOutput')]
  
  for(b in 1:length(BirdNETOutput)){
    FullPaths <-   list.files(BirdNETOutput[b],full.names = T,recursive = T)
    TrainingData <- str_split_fixed(BirdNETOutput[b],pattern = 'randomization/',n=2)[,2]  
    TrainingData <- str_split_fixed(TrainingData,pattern = '/',n=2)[,1]  
    
    ClipDetections <- FullPaths
    
    ClipDetectionsShort <-dirname(FullPaths)
    
    ActualLabelAll <-str_split_fixed(ClipDetectionsShort,pattern = 'BirdNETOutput',n=2)[,2]
    ActualLabelAll <-str_split_fixed(ActualLabelAll,pattern = '/',n=2)[,2]
    
    BirdNETPerformancelarDF <- data.frame()
    
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
      
      
      BirdNETPerformancelarDF <- rbind.data.frame(BirdNETPerformancelarDF,TempRow)
    }
    
    
    caretConf <- caret::confusionMatrix(
      as.factor(BirdNETPerformancelarDF$PredictedLabel),
      as.factor(BirdNETPerformancelarDF$ActualLabel),
      mode = 'everything')
    
    CaretConfusionMatrixlar[[a]] <-  caretConf$table
    
    CaretDF <- caretConf$byClass[5:7]
    
    Accuracy <-    caretConf$overall[1]
    # Convert predicted probabilities to a matrix
    Fold <- TrainingData
    TempRow <- cbind.data.frame(t(CaretDF),Accuracy,Fold,TrainingData)
    TempRow$Class <- rownames(TempRow)
    
    CombinedFoldPerformancelar <- rbind.data.frame(CombinedFoldPerformancelar,TempRow )
  }
}


ggboxplot(data=CombinedFoldPerformancelar,x='Fold', y='F1',
          color = 'Fold')


# Confusion matrix hylobates ----------------------------------------------
# Use Reduce to sum up all the tables element-wise
sum_of_tables <- Reduce(`+`, CaretConfusionMatrixlar)

# Divide the sum by the number of tables to get the mean
mean_table <- sum_of_tables / length(CaretConfusionMatrixlar)

# View the result
MeanTable <- round(mean_table,0)

# Calculate row sums (Reference totals)
row_totals <- colSums(MeanTable)  # Exclude the first column (Predictions)

# Normalize the confusion matrix by dividing each element by the corresponding class total
normalized_confusion_matrix <- as.data.frame(t(apply(MeanTable, 1, function(x) x / row_totals)))

# Add the 'Prediction' column back to the normalized matrix
normalized_confusion_matrix$Prediction <- rownames(sum_of_tables)

# Reshape the data into long format for ggplot
confusion_matrix_long <- melt(normalized_confusion_matrix, id.vars = "Prediction")

# Rename columns for clarity
colnames(confusion_matrix_long) <- c("Reference", "Prediction", "Count")

confusion_matrix_long$Reference <- revalue(as.factor(confusion_matrix_long$Reference),
                                           c('LG_FG_Indonesia'= 'Lar (Indonesia)',
                                             'LG_FG_Malaysia'='Lar (Malaysia)'))

confusion_matrix_long$Prediction <- revalue(as.factor(confusion_matrix_long$Prediction),
                                            c('LG_FG_Indonesia'= 'Lar (Indonesia)',
                                              'LG_FG_Malaysia'='Lar (Malaysia)'))

larmean <- round(mean(CombinedFoldPerformancelar$Accuracy),2)
# Generate the heatmap
LarConfusion <- ggplot(confusion_matrix_long, aes(y = Reference, x = Prediction, fill = Count)) +
  geom_tile() +
  scale_fill_distiller(palette = "Greys", direction = 1, name = 'Proportion')+
  theme_minimal() +
  labs(title = paste("Lar Gibbon \n mean accuracy:",larmean), x = "Reference", y = "Prediction") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


cowplot::plot_grid(LarConfusion, HMHFConfusion)
