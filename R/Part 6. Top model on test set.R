library(tidyr)
library(viridis)
library(plyr)
library(ROCR)
library(caret)
library(ggpubr)
library(reshape2)
library(stringr)

# Based on best AUC
TestResults <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/BirdNETOutput_bestauc/',full.names = T)
#TestResults <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/BirdNETOutput_bestauc',full.names = T)

CombinedFoldPerformance_test <- data.frame()
CaretConfusionMatrix <- list()
ThresholdPerfDF <- data.frame()
for(a in 1:length(TestResults)){
  
  BirdNETOutput <-  TestResults[a]
  
  ClipDetections <- list.files(BirdNETOutput,
                               recursive = T,full.names = T)
  
  ClipDetectionsShort <- list.files(BirdNETOutput,
                                    recursive = T,full.names = F)
  
  ClipDetections <- ClipDetections[!str_detect(ClipDetections, 'Cambodia_KSWS_NG_Clink/noise')]
  ClipDetectionsShort <- ClipDetectionsShort[!str_detect(ClipDetectionsShort, 'Cambodia_KSWS_NG_Clink/noise')]

  
  ActualLabelAll <-str_split_fixed(ClipDetectionsShort,pattern = '/',n=3)[,2]
  
  BirdNETPerformanceDF <- data.frame()
  
  for(b in 1: length(ClipDetections)){
    
    TempDF <- read.delim(ClipDetections[b])
    
    
    TempDF <-  subset(TempDF,Common.Name!='nocall' )
    
    # Find the highest confidence for each clip
    if(nrow(TempDF) > 0){
      ActualLabel <- ActualLabelAll[b]
      Index <- which.max(TempDF$Confidence)
      PredictedLabel <- TempDF[Index,]$Species.Code
      Confidence <- TempDF[Index,]$Confidence
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
  

  BirdNETPerformanceDF$PredictedLabel <-  revalue(as.factor(BirdNETPerformanceDF$PredictedLabel),
          c('JG_FG'='noise'))

  caretConf <- caret::confusionMatrix(
    as.factor(BirdNETPerformanceDF$PredictedLabel),
    as.factor(BirdNETPerformanceDF$ActualLabel),
    mode = 'everything')
  
  CaretConfusionMatrix[[a]] <- (caretConf$table)
  
  
  CaretDF <- caretConf$byClass[,5:7]
  
  Accuracy <-    caretConf$overall[1]
  # Convert predicted probabilities to a matrix
  
  uniqueLabels <- unique(BirdNETPerformanceDF$ActualLabel)
  auc.list <- list()

  for(i in 1:length(uniqueLabels)){
    
    if(uniqueLabels[i] != 'noise'){
      binary_labels <- ifelse(BirdNETPerformanceDF$ActualLabel == uniqueLabels[i], 1, 0)
      roc_data_binary <- ROCR::prediction(BirdNETPerformanceDF$Confidence,as.factor(binary_labels))
      AUC_binary <- performance(roc_data_binary,"auc")
      AUC_binary <- AUC_binary@y.values[[1]]
      AUC <- as.numeric(AUC_binary)
      auc.list[[i]] <- AUC
      
      
      Thresholds <- seq(0.1,1,0.05)
      
      # Loop through each threshold value
      for(a in 1:length(Thresholds)){
        
        # Filter the subset based on the confidence threshold
        TopModelDetectionDF_single <-BirdNETPerformanceDF
        
        TopModelDetectionDF_single$PredictedClass <-
          ifelse(TopModelDetectionDF_single$Confidence  >=Thresholds[a] & TopModelDetectionDF_single$PredictedLabel ==uniqueLabels[i] , uniqueLabels[i], 'noise')
        
        TopModelDetectionDF_single$ActualLabel <- 
          ifelse( as.factor(TopModelDetectionDF_single$ActualLabel) ==uniqueLabels[i] , uniqueLabels[i], 'noise')
        
        # Calculate confusion matrix using caret package
        caretConf <- caret::confusionMatrix(
          as.factor(TopModelDetectionDF_single$PredictedClass),
          as.factor(TopModelDetectionDF_single$ActualLabel),
          mode = 'everything',positive =uniqueLabels[i]  )
        
        
        # Extract F1 score, Precision, and Recall from the confusion matrix
        F1 <- caretConf$byClass[7]
        Precision <- caretConf$byClass[5]
        Recall <- caretConf$byClass[6]
        FP <- caretConf$table[1,2]
        # TN <- caretConf$table[2,2]+JahooAdj
        # FPR <-  FP / (FP + TN)
        # # Create a row for the result and add it to the SampleSizeGreyGibbon
        #TrainingData <- training_data_type
        TempF1Row <- cbind.data.frame(F1, Precision, Recall,AUC)#,FPR
        TempF1Row$Thresholds <- Thresholds[a]
        TempF1Row$Class <- uniqueLabels[i]
        ThresholdPerfDF <- rbind.data.frame(ThresholdPerfDF,TempF1Row )
      }
      
    }
  }
  
  AUC <- mean(unlist(auc.list))
  
  
  Fold <- basename(TestResults[a])
  TrainingData <- '3secsplit '
  TempRow <- cbind.data.frame(CaretDF,AUC,Accuracy,Fold,TrainingData)
  TempRow$Class <- rownames(TempRow)
  
  CombinedFoldPerformance_test <- rbind.data.frame(CombinedFoldPerformance_test,TempRow )
}

CombinedFoldPerformance_test

ggboxplot(data=CombinedFoldPerformance_test,x='Fold', y='Accuracy',color   ='TrainingData')
ggboxplot(data=CombinedFoldPerformance_test,x='Class', y='F1',color   ='TrainingData')

# Convert to long format
CombinedFoldPerformance_test_long <- CombinedFoldPerformance_test %>%
  pivot_longer(cols = c(F1, AUC, Accuracy), 
               names_to = "Metric", 
               values_to = "Value")

ggboxplot(data=CombinedFoldPerformance_test_long,x='Metric', y='Value',color   ='Metric')


# Create confusion matrix heatmap -----------------------------------------

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
        c("SS_USSFB"='Siamang','AG_FG'='Agile','HA_FG'='White bearded',
          'HF_FG'= 'Northern grey','HM_FG'='Müller','LG_FG'='Lar','NG_FG'='Crested',
          'noise'='Noise'))

confusion_matrix_long$Prediction <- revalue(as.factor(confusion_matrix_long$Prediction),
                                           c("SS_USSFB"='Siamang','AG_FG'='Agile','HA_FG'='White bearded',
                                             'HF_FG'= 'Northern grey','HM_FG'='Müller','LG_FG'='Lar','NG_FG'='Crested',
                                             'noise'='Noise'))

# Generate the heatmap
ggplot(confusion_matrix_long, aes(y = Reference, x = Prediction, fill = Count)) +
  geom_tile() +
  scale_fill_distiller(palette = "Greys", direction = 1, name = 'Proportion')+
  theme_minimal() +
  labs(title = "Gibbon Species Confusion Matrix", x = "Reference", y = "Prediction") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Find best F1 by threshold

ThresholdPerfDF$Class <- revalue(as.factor(ThresholdPerfDF$Class),
                                            c("SS_USSFB"='Siamang','AG_FG'='Agile','HA_FG'='White bearded',
                                              'HF_FG'= 'Northern grey','HM_FG'='Müller','LG_FG'='Lar','NG_FG'='Crested',
                                              'noise'='Noise'))

UniqueClass <- unique(ThresholdPerfDF$Class)

BestF1DF <- data.frame()

for(j in 1:length(UniqueClass)){
  
  TempF1 <- subset(ThresholdPerfDF,Class==UniqueClass[j])
  TempF1 <- TempF1[which.max(TempF1$F1),]
  BestF1DF <- rbind.data.frame(BestF1DF,TempF1 )
}

BestF1DF[,c(1:4)] <- round(BestF1DF[,c(1:4)],2)

write.csv(BestF1DF[,c(6,5,1:4)],'data/BestF1DF.csv')

CombinedFoldPerformance_test$AUC

