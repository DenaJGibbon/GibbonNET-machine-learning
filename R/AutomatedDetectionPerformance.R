library(ROCR)

start.time.buffer <- 3
end.time.buffer <- 12


# Binary automated detection ----------------------------------------------

# White bearded gibbon ----------------------------------------------------

BirdNETFilesHA <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/AllTest/binary_timeshift/HAclean',
                             full.names = TRUE)

AnnotatedFilesHA <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Indonesia_Tahawa_HA_Morrow_full/Annotations/',
                             full.names = TRUE)


TopModelDetectionDFHA <- data.frame()

for(a in 1:length(BirdNETFilesHA)){
  
 TempTopModelTable <-  read.delim(BirdNETFilesHA[a])
 BaseName <- basename(BirdNETFilesHA[a])
 FileName <- str_split_fixed(BaseName,pattern = '.BirdNET',n=2)[,1]
 testDataIndex <- which(str_detect(AnnotatedFilesHA,FileName))
  
 if(length(testDataIndex) > 0 ){
   
   TestDataTable <- read.delim2(AnnotatedFilesHA[testDataIndex])
   
   # Round Begin.Time..s. and End.Time..s. columns to numeric
   TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
   TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))
   
   DetectionList <- list()
   # Loop through each row in TempTopModelTable
   for (c in 1:nrow(TempTopModelTable)) {
     TempRow <- TempTopModelTable[c,]
     
     # Check if Begin.Time..s. is not NA
     if (!is.na(TempRow$Begin.Time..s.)) {
       # Convert Begin.Time..s. and End.Time..s. to numeric
       TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
       TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)
       
       # Determine if the time of the detection is within the time range of an annotation
       TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                          TestDataTable$Begin.Time..s. - start.time.buffer,
                                          TestDataTable$End.Time..s. + end.time.buffer)
       
       # Extract the detections matching the time range
       matched_detections <- TestDataTable[TimeBetween, ]
       
       if (nrow(matched_detections) > 0) {
         # Set Species.Code based on the Call.Type in matched_detections
         TempRow$Species.Code <- 'HA'
         DetectionList[[length( unlist(DetectionList))+1]] <-  which(TimeBetween == TRUE)
       } else {
         # Set Species.Code to 'Noise' if no corresponding annotation is found
         TempRow$Species.Code <- 'noise'
       }
       
       TempRow$File.Name <- BaseName
       TempRow$model.type <- 'BirdNET'
       
       TempRow <-  TempRow[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.",
                   "File.Name","model.type","Confidence" ,"Species.Code")]
       
       # Append TempRow to TopModelDetectionDFHA
       TopModelDetectionDFHA <- rbind.data.frame(TopModelDetectionDFHA, TempRow)
     }
   }
   
   # Identify missed detections
   
   if (length( unlist(DetectionList)) > 0 &  length( unlist(DetectionList)) < nrow(TestDataTable) ) {
     
     missed_detections <- TestDataTable[-unlist(DetectionList), ]
     # Prepare missed detections data
     missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
     missed_detections <- missed_detections
     missed_detections$File.Name <- BaseName
     missed_detections$model.type <- 'BirdNET'
     missed_detections$Confidence <- 0
     missed_detections$Species.Code <- 'HA'
     
     # Append missed detections to TopModelDetectionDFHA
     TopModelDetectionDFHA <- rbind.data.frame(TopModelDetectionDFHA, missed_detections)
   }
   
   if (length( unlist(DetectionList)) == 0) {
     
     missed_detections <- TestDataTable
     # Prepare missed detections data
     missed_detections <- missed_detections
     missed_detections$File.Name <- BaseName
     missed_detections$model.type <- 'BirdNET'
     missed_detections$Confidence <- 0
     missed_detections$Species.Code <- 'HA'
     
     # Append missed detections to TopModelDetectionDFHA
     TopModelDetectionDFHA <- rbind.data.frame(TopModelDetectionDFHA, missed_detections)
     
   }
   
 }
}

head(TopModelDetectionDFHA)
nrow(TopModelDetectionDFHA)
table(TopModelDetectionDFHA$Species.Code)

# Convert Species.Code column to a factor variable
TopModelDetectionDFHA$Species.Code <- as.factor(TopModelDetectionDFHA$Species.Code)

# Display unique values in the Species.Code column
unique(TopModelDetectionDFHA$Species.Code)

# Define a vector of confidence Thresholds
Thresholds <-seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameHABinary.ML <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDFHA_single <-TopModelDetectionDFHA
  
  TopModelDetectionDFHA_single$PredictedSpecies.Code <-
    ifelse(TopModelDetectionDFHA_single$Confidence  <=Thresholds[a], 'noise','HA')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDFHA_single$PredictedSpecies.Code),
    as.factor(TopModelDetectionDFHA_single$Species.Code),positive = 'HA',
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  TN <- sum(caretConf$table[2,])#+JahooAdj
  FPR <-  FP / (FP + TN)
  # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameHABinary.ML <- rbind.data.frame(BestF1data.frameHABinary.ML, TempF1Row)
}

BestF1data.frameHABinary.ML

HAMax <- round(max(na.omit(BestF1data.frameHABinary.ML$F1)),2)

HApred <- prediction( TopModelDetectionDFHA$Confidence, TopModelDetectionDFHA$Species.Code)
HAperf <- performance(HApred,"auc")
HAAUC <- HAperf@y.values[[1]]
HAAUC

# Metric plot
HABinaryPlot <- ggplot(data = BestF1data.frameHABinary.ML, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("White bearded gibbon (binary) \n max F1:",HAMax),
       x = "Threshold",
       y = "Metric Score") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")

HABinaryPlot


# Northern grey gibbon ----------------------------------------------------
BirdNETFilesHF <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/AllTest/binary_timeshift/maliau/',
                             full.names = TRUE)

AnnotatedFilesHF <- list.files('/Users/denaclink/Desktop/BirdNETOutput_automated_detection/AnnotatedFiles/',
                               full.names = TRUE)


TopModelDetectionDFHF <- data.frame()

for(a in 1:length(BirdNETFilesHF)){
  
  TempTopModelTable <-  read.delim(BirdNETFilesHF[a])
  BaseName <- basename(BirdNETFilesHF[a])
  FileName <- str_split_fixed(BaseName,pattern = '.BirdNET',n=2)[,1]
  testDataIndex <- which(str_detect(AnnotatedFilesHF,FileName))
  
  if(length(testDataIndex) > 0 ){
    
    TestDataTable <- read.delim2(AnnotatedFilesHF[testDataIndex])
    
    TestDataTable <- subset(TestDataTable,Call.type=='female.gibbon')
    
    # Round Begin.Time..s. and End.Time..s. columns to numeric
    TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
    TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))
    
    DetectionList <- list()
    # Loop through each row in TempTopModelTable
    for (c in 1:nrow(TempTopModelTable)) {
      TempRow <- TempTopModelTable[c,]
      
      # Check if Begin.Time..s. is not NA
      if (!is.na(TempRow$Begin.Time..s.)) {
        # Convert Begin.Time..s. and End.Time..s. to numeric
        TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
        TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)
        
        # Determine if the time of the detection is within the time range of an annotation
        TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                           TestDataTable$Begin.Time..s. - start.time.buffer,
                                           TestDataTable$End.Time..s. + end.time.buffer)
        
        # Extract the detections matching the time range
        matched_detections <- TestDataTable[TimeBetween, ]
        
        if (nrow(matched_detections) > 0) {
          # Set Species.Code based on the Call.Type in matched_detections
          TempRow$Species.Code <- 'HF'
          DetectionList[[length( unlist(DetectionList))+1]] <-  which(TimeBetween == TRUE)
        } else {
          # Set Species.Code to 'Noise' if no corresponding annotation is found
          TempRow$Species.Code <- 'noise'
        }
        
        TempRow$File.Name <- BaseName
        TempRow$model.type <- 'BirdNET'
        
        TempRow <-  TempRow[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.",
                                "File.Name","model.type","Confidence" ,"Species.Code")]
        
        # Append TempRow to TopModelDetectionDFHF
        TopModelDetectionDFHF <- rbind.data.frame(TopModelDetectionDFHF, TempRow)
      }
    }
    
    # Identify missed detections
    
    if (length( unlist(DetectionList)) > 0 &  length( unlist(DetectionList)) < nrow(TestDataTable) ) {
      
      missed_detections <- TestDataTable[-unlist(DetectionList), ]
      # Prepare missed detections data
      missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
      missed_detections <- missed_detections
      missed_detections$File.Name <- BaseName
      missed_detections$model.type <- 'BirdNET'
      missed_detections$Confidence <- 0
      missed_detections$Species.Code <- 'HF'
      
      # Append missed detections to TopModelDetectionDFHF
      TopModelDetectionDFHF <- rbind.data.frame(TopModelDetectionDFHF, missed_detections)
    }
    
    if (length( unlist(DetectionList)) == 0) {
      
      missed_detections <- TestDataTable
      # Prepare missed detections data
      missed_detections <- missed_detections
      missed_detections$File.Name <- BaseName
      missed_detections$model.type <- 'BirdNET'
      missed_detections$Confidence <- 0
      missed_detections$Species.Code <- 'HF'
      
      # Append missed detections to TopModelDetectionDFHF
      TopModelDetectionDFHF <- rbind.data.frame(TopModelDetectionDFHF, missed_detections)
      
    }
    
  }
}

head(TopModelDetectionDFHF)
nrow(TopModelDetectionDFHF)
table(TopModelDetectionDFHF$Species.Code)

# Convert Species.Code column to a factor variable
TopModelDetectionDFHF$Species.Code <- as.factor(TopModelDetectionDFHF$Species.Code)

# Display unique values in the Species.Code column
unique(TopModelDetectionDFHF$Species.Code)

# Define a vector of confidence Thresholds
Thresholds <-seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameHFBinary.ML <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDFHF_single <-TopModelDetectionDFHF
  
  TopModelDetectionDFHF_single$PredictedSpecies.Code <-
    ifelse(TopModelDetectionDFHF_single$Confidence  <=Thresholds[a], 'noise','HF')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDFHF_single$PredictedSpecies.Code),
    as.factor(TopModelDetectionDFHF_single$Species.Code),positive = 'HF',
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  TN <- sum(caretConf$table[2,])#+JahooAdj
  FPR <-  FP / (FP + TN)
  # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameHFBinary.ML <- rbind.data.frame(BestF1data.frameHFBinary.ML, TempF1Row)
}

BestF1data.frameHFBinary.ML

HFMax <- round(max(na.omit(BestF1data.frameHFBinary.ML$F1)),2)

HFpred <- prediction( TopModelDetectionDFHF$Confidence, TopModelDetectionDFHF$Species.Code)
HFperf <- performance(HFpred,"auc")
HFAUC <- HFperf@y.values[[1]]
HFAUC

# Metric plot
HFBinaryPlot <- ggplot(data = BestF1data.frameHFBinary.ML, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Northern grey gibbon (binary) \n max F1:",HFMax),
       x = "Threshold",
       y = "Metric Score") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", sHFpe = "Guide name")

HFBinaryPlot


# Lar gibbon --------------------------------------------------------------

BirdNETFilesLG <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/AllTest/binary_timeshift/Audio Files/',
                             full.names = TRUE)

AnnotatedFilesLG <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Malaysia_Kenyir_LG_George/Selection Table/',
                               full.names = TRUE)


TopModelDetectionDFLG <- data.frame()

for(a in 1:length(BirdNETFilesLG)){
  
  TempTopModelTable <-  read.delim(BirdNETFilesLG[a])
  BaseName <- basename(BirdNETFilesLG[a])
  FileName <- str_split_fixed(BaseName,pattern = '.BirdNET',n=2)[,1]
  testDataIndex <- which(str_detect(AnnotatedFilesLG,FileName))
  
  if(length(testDataIndex) > 0 ){
    
    TestDataTable <- read.delim2(AnnotatedFilesLG[testDataIndex])
    
    TestDataTable <- subset(TestDataTable,Call.Type=='LGFG')
    
    if(nrow(TestDataTable) > 0 ){
    # Round Begin.Time..s. and End.Time..s. columns to numeric
    TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
    TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))
    
    DetectionList <- list()
    # Loop through each row in TempTopModelTable
    for (c in 1:nrow(TempTopModelTable)) {
      TempRow <- TempTopModelTable[c,]
      
      # Check if Begin.Time..s. is not NA
      if (!is.na(TempRow$Begin.Time..s.)) {
        # Convert Begin.Time..s. and End.Time..s. to numeric
        TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
        TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)
        
        # Determine if the time of the detection is within the time range of an annotation
        TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                           TestDataTable$Begin.Time..s. - start.time.buffer,
                                           TestDataTable$End.Time..s. + end.time.buffer)
        
        # Extract the detections matching the time range
        matched_detections <- TestDataTable[TimeBetween, ]
        
        if (nrow(matched_detections) > 0) {
          # Set Species.Code based on the Call.Type in matched_detections
          TempRow$Species.Code <- 'LG'
          DetectionList[[length( unlist(DetectionList))+1]] <-  which(TimeBetween == TRUE)
        } else {
          # Set Species.Code to 'Noise' if no corresponding annotation is found
          TempRow$Species.Code <- 'noise'
        }
        
        TempRow$File.Name <- BaseName
        TempRow$model.type <- 'BirdNET'
        
        TempRow <-  TempRow[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.",
                                "File.Name","model.type","Confidence" ,"Species.Code")]
        
        # Append TempRow to TopModelDetectionDFLG
        TopModelDetectionDFLG <- rbind.data.frame(TopModelDetectionDFLG, TempRow)
      }
    }
    
    # Identify missed detections
    
    if (length( unlist(DetectionList)) > 0 &  length( unlist(DetectionList)) < nrow(TestDataTable) ) {
      
      missed_detections <- TestDataTable[-unlist(DetectionList), ]
      # Prepare missed detections data
      missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
      missed_detections <- missed_detections
      missed_detections$File.Name <- BaseName
      missed_detections$model.type <- 'BirdNET'
      missed_detections$Confidence <- 0
      missed_detections$Species.Code <- 'LG'
      
      # Append missed detections to TopModelDetectionDFLG
      TopModelDetectionDFLG <- rbind.data.frame(TopModelDetectionDFLG, missed_detections)
    }
    
    if (length( unlist(DetectionList)) == 0) {
      
      missed_detections <- TestDataTable
      # Prepare missed detections data
      missed_detections <- missed_detections
      missed_detections$File.Name <- BaseName
      missed_detections$model.type <- 'BirdNET'
      missed_detections$Confidence <- 0
      missed_detections$Species.Code <- 'LG'
      
      # Append missed detections to TopModelDetectionDFLG
      TopModelDetectionDFLG <- rbind.data.frame(TopModelDetectionDFLG, missed_detections)
      
    }
    
    }
  }
}

head(TopModelDetectionDFLG)
nrow(TopModelDetectionDFLG)
table(TopModelDetectionDFLG$Species.Code)

# Convert Species.Code column to a factor variable
TopModelDetectionDFLG$Species.Code <- as.factor(TopModelDetectionDFLG$Species.Code)

# Display unique values in the Species.Code column
unique(TopModelDetectionDFLG$Species.Code)

# Define a vector of confidence Thresholds
Thresholds <-seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameLGBinary.ML <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDFLG_single <-TopModelDetectionDFLG
  
  TopModelDetectionDFLG_single$PredictedSpecies.Code <-
    ifelse(TopModelDetectionDFLG_single$Confidence  <=Thresholds[a], 'noise','LG')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDFLG_single$PredictedSpecies.Code),
    as.factor(TopModelDetectionDFLG_single$Species.Code),positive = 'LG',
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  TN <- sum(caretConf$table[2,])#+JahooAdj
  FPR <-  FP / (FP + TN)
  # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameLGBinary.ML <- rbind.data.frame(BestF1data.frameLGBinary.ML, TempF1Row)
}

BestF1data.frameLGBinary.ML

LGMax <- round(max(na.omit(BestF1data.frameLGBinary.ML$F1)),2)

LGpred <- prediction( TopModelDetectionDFLG$Confidence, TopModelDetectionDFLG$Species.Code)
LGperf <- performance(LGpred,"auc")
LGAUC <- LGperf@y.values[[1]]
LGAUC

# Metric plot
LGBinaryPlot <- ggplot(data = BestF1data.frameLGBinary.ML, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Lar gibbon (binary) \n max F1:",LGMax),
       x = "Threshold",
       y = "Metric Score") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", sLGpe = "Guide name")

LGBinaryPlot

c(HAAUC,HFAUC,LGAUC)

cowplot::plot_grid(HABinaryPlot,HFBinaryPlot,LGBinaryPlot)


# Multiclass automated detection ------------------------------------------


# Multi white bearded -----------------------------------------------------
BirdNETFilesHA <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/AllTest/multiclass_timeshift/Indonesia_Tahawa_HA_Morrow_full/',
                             full.names = TRUE)

AnnotatedFilesHA <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Indonesia_Tahawa_HA_Morrow_full/Annotations/',
                               full.names = TRUE)


TopModelDetectionDFHAMulti <- data.frame()

for(a in 1:length(BirdNETFilesHA)){
  
  TempTopModelTable <-  read.delim(BirdNETFilesHA[a])
  TempTopModelTable <- subset(TempTopModelTable,Species.Code=='HA')
  BaseName <- basename(BirdNETFilesHA[a])
  FileName <- str_split_fixed(BaseName,pattern = '.BirdNET',n=2)[,1]
  testDataIndex <- which(str_detect(AnnotatedFilesHA,FileName))
  
  if(length(testDataIndex) > 0 ){
    
    TestDataTable <- read.delim2(AnnotatedFilesHA[testDataIndex])
    
    # Round Begin.Time..s. and End.Time..s. columns to numeric
    TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
    TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))
    
    DetectionList <- list()
    # Loop through each row in TempTopModelTable
    for (c in 1:nrow(TempTopModelTable)) {
      TempRow <- TempTopModelTable[c,]
      
      # Check if Begin.Time..s. is not NA
      if (!is.na(TempRow$Begin.Time..s.)) {
        # Convert Begin.Time..s. and End.Time..s. to numeric
        TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
        TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)
        
        # Determine if the time of the detection is within the time range of an annotation
        TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                           TestDataTable$Begin.Time..s. - start.time.buffer,
                                           TestDataTable$End.Time..s. + end.time.buffer)
        
        # Extract the detections matching the time range
        matched_detections <- TestDataTable[TimeBetween, ]
        
        if (nrow(matched_detections) > 0) {
          # Set Species.Code based on the Call.Type in matched_detections
          TempRow$Species.Code <- 'HA'
          DetectionList[[length( unlist(DetectionList))+1]] <-  which(TimeBetween == TRUE)
        } else {
          # Set Species.Code to 'Noise' if no corresponding annotation is found
          TempRow$Species.Code <- 'noise'
        }
        
        TempRow$File.Name <- BaseName
        TempRow$model.type <- 'BirdNET'
        
        TempRow <-  TempRow[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.",
                                "File.Name","model.type","Confidence" ,"Species.Code")]
        
        # Append TempRow to TopModelDetectionDFHAMulti
        TopModelDetectionDFHAMulti <- rbind.data.frame(TopModelDetectionDFHAMulti, TempRow)
      }
    }
    
    # Identify missed detections
    
    if (length( unlist(DetectionList)) > 0 &  length( unlist(DetectionList)) < nrow(TestDataTable) ) {
      
      missed_detections <- TestDataTable[-unlist(DetectionList), ]
      # Prepare missed detections data
      missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
      missed_detections <- missed_detections
      missed_detections$File.Name <- BaseName
      missed_detections$model.type <- 'BirdNET'
      missed_detections$Confidence <- 0
      missed_detections$Species.Code <- 'HA'
      
      # Append missed detections to TopModelDetectionDFHAMulti
      TopModelDetectionDFHAMulti <- rbind.data.frame(TopModelDetectionDFHAMulti, missed_detections)
    }
    
    if (length( unlist(DetectionList)) == 0) {
      
      missed_detections <- TestDataTable
      # Prepare missed detections data
      missed_detections <- missed_detections
      missed_detections$File.Name <- BaseName
      missed_detections$model.type <- 'BirdNET'
      missed_detections$Confidence <- 0
      missed_detections$Species.Code <- 'HA'
      
      # Append missed detections to TopModelDetectionDFHAMulti
      TopModelDetectionDFHAMulti <- rbind.data.frame(TopModelDetectionDFHAMulti, missed_detections)
      
    }
    
  }
}

head(TopModelDetectionDFHAMulti)
nrow(TopModelDetectionDFHAMulti)
table(TopModelDetectionDFHAMulti$Species.Code)

# Convert Species.Code column to a factor variable
TopModelDetectionDFHAMulti$Species.Code <- as.factor(TopModelDetectionDFHAMulti$Species.Code)

# Display unique values in the Species.Code column
unique(TopModelDetectionDFHAMulti$Species.Code)

# Define a vector of confidence Thresholds
Thresholds <-seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameHAMulti.ML <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDFHAMulti_single <-TopModelDetectionDFHAMulti
  
  TopModelDetectionDFHAMulti_single$PredictedSpecies.Code <-
    ifelse(TopModelDetectionDFHAMulti_single$Confidence  <=Thresholds[a], 'noise','HA')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDFHAMulti_single$PredictedSpecies.Code),
    as.factor(TopModelDetectionDFHAMulti_single$Species.Code),positive = 'HA',
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  TN <- sum(caretConf$table[2,])#+JahooAdj
  FPR <-  FP / (FP + TN)
  # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameHAMulti.ML <- rbind.data.frame(BestF1data.frameHAMulti.ML, TempF1Row)
}

BestF1data.frameHAMulti.ML

HAMax <- round(max(na.omit(BestF1data.frameHAMulti.ML$F1)),2)

HApred <- prediction( TopModelDetectionDFHAMulti$Confidence, TopModelDetectionDFHAMulti$Species.Code)
HAperf <- performance(HApred,"auc")
HAAUC <- HAperf@y.values[[1]]
HAAUC

# Metric plot
HAMultiPlot <- ggplot(data = BestF1data.frameHAMulti.ML, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("White bearded gibbon (Multi) \n max F1:",HAMax),
       x = "Threshold",
       y = "Metric Score") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")

HAMultiPlot


# Multi grey gibbon -------------------------------------------------------

BirdNETFilesHF <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/AllTest/multiclass_timeshift/Malaysia_Maliau_HF_Clink_full/',
                             full.names = TRUE)

AnnotatedFilesHF <- list.files('/Users/denaclink/Desktop/BirdNETOutput_automated_detection/AnnotatedFiles/',
                               full.names = TRUE)


TopModelDetectionDFHFMulti <- data.frame()

for(a in 1:length(BirdNETFilesHF)){
  
  TempTopModelTable <-  read.delim(BirdNETFilesHF[a])
  TempTopModelTable <- subset(TempTopModelTable,Species.Code=='HF')
  BaseName <- basename(BirdNETFilesHF[a])
  FileName <- str_split_fixed(BaseName,pattern = '.BirdNET',n=2)[,1]
  testDataIndex <- which(str_detect(AnnotatedFilesHF,FileName))
  
  if(length(testDataIndex) > 0 ){
    
    TestDataTable <- read.delim2(AnnotatedFilesHF[testDataIndex])
    
    TestDataTable <- subset(TestDataTable,Call.type=='female.gibbon')
    
    # Round Begin.Time..s. and End.Time..s. columns to numeric
    TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
    TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))
    
    DetectionList <- list()
    # Loop through each row in TempTopModelTable
    for (c in 1:nrow(TempTopModelTable)) {
      TempRow <- TempTopModelTable[c,]
      
      # Check if Begin.Time..s. is not NA
      if (!is.na(TempRow$Begin.Time..s.)) {
        # Convert Begin.Time..s. and End.Time..s. to numeric
        TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
        TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)
        
        # Determine if the time of the detection is within the time range of an annotation
        TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                           TestDataTable$Begin.Time..s. - start.time.buffer,
                                           TestDataTable$End.Time..s. + end.time.buffer)
        
        # Extract the detections matching the time range
        matched_detections <- TestDataTable[TimeBetween, ]
        
        if (nrow(matched_detections) > 0) {
          # Set Species.Code based on the Call.Type in matched_detections
          TempRow$Species.Code <- 'HF'
          DetectionList[[length( unlist(DetectionList))+1]] <-  which(TimeBetween == TRUE)
        } else {
          # Set Species.Code to 'Noise' if no corresponding annotation is found
          TempRow$Species.Code <- 'noise'
        }
        
        TempRow$File.Name <- BaseName
        TempRow$model.type <- 'BirdNET'
        
        TempRow <-  TempRow[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.",
                                "File.Name","model.type","Confidence" ,"Species.Code")]
        
        # Append TempRow to TopModelDetectionDFHFMulti
        TopModelDetectionDFHFMulti <- rbind.data.frame(TopModelDetectionDFHFMulti, TempRow)
      }
    }
    
    # Identify missed detections
    
    if (length( unlist(DetectionList)) > 0 &  length( unlist(DetectionList)) < nrow(TestDataTable) ) {
      
      missed_detections <- TestDataTable[-unlist(DetectionList), ]
      # Prepare missed detections data
      missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
      missed_detections <- missed_detections
      missed_detections$File.Name <- BaseName
      missed_detections$model.type <- 'BirdNET'
      missed_detections$Confidence <- 0
      missed_detections$Species.Code <- 'HF'
      
      # Append missed detections to TopModelDetectionDFHFMulti
      TopModelDetectionDFHFMulti <- rbind.data.frame(TopModelDetectionDFHFMulti, missed_detections)
    }
    
    if (length( unlist(DetectionList)) == 0) {
      
      missed_detections <- TestDataTable
      # Prepare missed detections data
      missed_detections <- missed_detections
      missed_detections$File.Name <- BaseName
      missed_detections$model.type <- 'BirdNET'
      missed_detections$Confidence <- 0
      missed_detections$Species.Code <- 'HF'
      
      # Append missed detections to TopModelDetectionDFHFMulti
      TopModelDetectionDFHFMulti <- rbind.data.frame(TopModelDetectionDFHFMulti, missed_detections[,c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", 
                                                                                                      "Low.Freq..Hz.", "High.Freq..Hz.", "File.Name", "model.type", 
                                                                                                      "Confidence", "Species.Code")])
      
    }
    
  }
}


head(TopModelDetectionDFHFMulti)
nrow(TopModelDetectionDFHFMulti)
table(TopModelDetectionDFHFMulti$Species.Code)

# Convert Species.Code column to a factor variable
TopModelDetectionDFHFMulti$Species.Code <- as.factor(TopModelDetectionDFHFMulti$Species.Code)

# Display unique values in the Species.Code column
unique(TopModelDetectionDFHFMulti$Species.Code)

# Define a vector of confidence Thresholds
Thresholds <-seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameHFMulti.ML <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDFHFMulti_single <-TopModelDetectionDFHFMulti
  
  TopModelDetectionDFHFMulti_single$PredictedSpecies.Code <-
    ifelse(TopModelDetectionDFHFMulti_single$Confidence  <=Thresholds[a], 'noise','HF')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDFHFMulti_single$PredictedSpecies.Code),
    as.factor(TopModelDetectionDFHFMulti_single$Species.Code),positive = 'HF',
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  TN <- sum(caretConf$table[2,])#+JahooAdj
  FPR <-  FP / (FP + TN)
  # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameHFMulti.ML <- rbind.data.frame(BestF1data.frameHFMulti.ML, TempF1Row)
}

BestF1data.frameHFMulti.ML

HFMax <- round(max(na.omit(BestF1data.frameHFMulti.ML$F1)),2)

HFpred <- prediction( TopModelDetectionDFHFMulti$Confidence, TopModelDetectionDFHFMulti$Species.Code)
HFperf <- performance(HFpred,"auc")
HFAUC <- HFperf@y.values[[1]]
HFAUC

# Metric plot
HFMultiPlot <- ggplot(data = BestF1data.frameHFMulti.ML, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Northern grey gibbon (Multi) \n max F1:",HFMax),
       x = "Threshold",
       y = "Metric Score") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", sHFpe = "Guide name")

HFMultiPlot


# Multi lar gibbon --------------------------------------------------------

BirdNETFilesLG <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TestResults/AllTest/multiclass_timeshift/Malaysia_Kenyir_LG_George/Audio Files/',
                             full.names = TRUE)

AnnotatedFilesLG <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Malaysia_Kenyir_LG_George/Selection Table/',
                               full.names = TRUE)


TopModelDetectionDFLG <- data.frame()

for(a in 1:length(BirdNETFilesLG)){
  
  TempTopModelTable <-  read.delim(BirdNETFilesLG[a])
  TempTopModelTable <- subset(TempTopModelTable, Species.Code =='LG')
  BaseName <- basename(BirdNETFilesLG[a])
  FileName <- str_split_fixed(BaseName,pattern = '.BirdNET',n=2)[,1]
  testDataIndex <- which(str_detect(AnnotatedFilesLG,FileName))
  
  if(length(testDataIndex) > 0 & nrow(TempTopModelTable) > 0){
    
    TestDataTable <- read.delim2(AnnotatedFilesLG[testDataIndex])
    
    TestDataTable <- subset(TestDataTable,Call.Type=='LGFG')
    
    if(nrow(TestDataTable) > 0 ){
      # Round Begin.Time..s. and End.Time..s. columns to numeric
      TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
      TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))
      
      DetectionList <- list()
      # Loop through each row in TempTopModelTable
      for (c in 1:nrow(TempTopModelTable)) {
        TempRow <- TempTopModelTable[c,]
        
        # Check if Begin.Time..s. is not NA
        if (!is.na(TempRow$Begin.Time..s.)) {
          # Convert Begin.Time..s. and End.Time..s. to numeric
          TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
          TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)
          
          # Determine if the time of the detection is within the time range of an annotation
          TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                             TestDataTable$Begin.Time..s. - start.time.buffer,
                                             TestDataTable$End.Time..s. + end.time.buffer)
          
          # Extract the detections matching the time range
          matched_detections <- TestDataTable[TimeBetween, ]
          
          if (nrow(matched_detections) > 0) {
            # Set Species.Code based on the Call.Type in matched_detections
            TempRow$Species.Code <- 'LG'
            DetectionList[[length( unlist(DetectionList))+1]] <-  which(TimeBetween == TRUE)
          } else {
            # Set Species.Code to 'Noise' if no corresponding annotation is found
            TempRow$Species.Code <- 'noise'
          }
          
          TempRow$File.Name <- BaseName
          TempRow$model.type <- 'BirdNET'
          
          TempRow <-  TempRow[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.",
                                  "File.Name","model.type","Confidence" ,"Species.Code")]
          
          # Append TempRow to TopModelDetectionDFLG
          TopModelDetectionDFLG <- rbind.data.frame(TopModelDetectionDFLG, TempRow)
        }
      }
      
      # Identify missed detections
      
      if (length( unlist(DetectionList)) > 0 &  length( unlist(DetectionList)) < nrow(TestDataTable) ) {
        
        missed_detections <- TestDataTable[-unlist(DetectionList), ]
        # Prepare missed detections data
        missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
        missed_detections <- missed_detections
        missed_detections$File.Name <- BaseName
        missed_detections$model.type <- 'BirdNET'
        missed_detections$Confidence <- 0
        missed_detections$Species.Code <- 'LG'
        
        # Append missed detections to TopModelDetectionDFLG
        TopModelDetectionDFLG <- rbind.data.frame(TopModelDetectionDFLG, missed_detections)
      }
      
      if (length( unlist(DetectionList)) == 0) {
        
        missed_detections <- TestDataTable
        # Prepare missed detections data
        missed_detections <- missed_detections
        missed_detections$File.Name <- BaseName
        missed_detections$model.type <- 'BirdNET'
        missed_detections$Confidence <- 0
        missed_detections$Species.Code <- 'LG'
        
        # Append missed detections to TopModelDetectionDFLG
        TopModelDetectionDFLG <- rbind.data.frame(TopModelDetectionDFLG, missed_detections)
        
      }
      
    }
  }
}

head(TopModelDetectionDFLG)
nrow(TopModelDetectionDFLG)
table(TopModelDetectionDFLG$Species.Code)

# Convert Species.Code column to a factor variable
TopModelDetectionDFLG$Species.Code <- as.factor(TopModelDetectionDFLG$Species.Code)

# Display unique values in the Species.Code column
unique(TopModelDetectionDFLG$Species.Code)

# Define a vector of confidence Thresholds
Thresholds <-seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameLGMulti.ML <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDFLG_single <-TopModelDetectionDFLG
  
  TopModelDetectionDFLG_single$PredictedSpecies.Code <-
    ifelse(TopModelDetectionDFLG_single$Confidence  <=Thresholds[a], 'noise','LG')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDFLG_single$PredictedSpecies.Code),
    as.factor(TopModelDetectionDFLG_single$Species.Code),positive = 'LG',
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  TN <- sum(caretConf$table[2,])#+JahooAdj
  FPR <-  FP / (FP + TN)
  # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall,FPR)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameLGMulti.ML <- rbind.data.frame(BestF1data.frameLGMulti.ML, TempF1Row)
}

BestF1data.frameLGMulti.ML

LGMax <- round(max(na.omit(BestF1data.frameLGMulti.ML$F1)),2)

LGpred <- prediction( TopModelDetectionDFLG$Confidence, TopModelDetectionDFLG$Species.Code)
LGperf <- performance(LGpred,"auc")
LGAUC <- LGperf@y.values[[1]]
LGAUC

# Metric plot
LGMultiPlot <- ggplot(data = BestF1data.frameLGMulti.ML, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Lar gibbon (Multi) \n max F1:",LGMax),
       x = "Threshold",
       y = "Metric Score") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", sLGpe = "Guide name")+ylim(0,1)

LGMultiPlot

cowplot::plot_grid(HABinaryPlot,HFBinaryPlot,LGBinaryPlot,
                   HAMultiPlot,HFMultiPlot,LGMultiPlot,nrow=2,
                   labels = c('A)','B)','C)','D)','E)','F)'),label_x = 0.9)
