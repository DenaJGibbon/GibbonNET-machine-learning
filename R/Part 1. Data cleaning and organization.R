# Load required libraries
library(tuneR)      # For reading and writing WAV files
library(seewave)    # For sound analysis and manipulation
library(stringr)    # For string manipulation functions
library(dplyr)

# Set the main output directory for processed audio files
OutputDirectory <- '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/ClipsByProjectID/'

# Indonesia_Kukar_HM_Mukhlisi ---------------------------------------------
# Define the project ID and output directory for the specific project
ProjectID <- "Indonesia_Kukar_HM_Mukhlisi/"
ProjectOutputDir <- paste(OutputDirectory, ProjectID, sep='/')

# List folders containing annotation and sound files
FolderList <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/OriginalFiles/Indonesia_Kukar_HM_Mukhlisi/Sound',
                         full.names = TRUE, recursive = T)

OutputFolder <- '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/ClipsByProjectID/Indonesia_Kukar_HM_Mukhlisi/HM_FG/'
ShortNames <- basename(FolderList)

file.copy(from=FolderList,to=paste(OutputFolder,ShortNames,sep=''))
# Indonesia_Siberut_MG_Setiawan ---------------------------------------------
# Define the project ID and output directory for the specific project
ProjectID <- "Indonesia_Siberut_MG_Setiawan/"
ProjectOutputDir <- paste(OutputDirectory, ProjectID, sep='/')

# List folders containing annotation and sound files
FolderList <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/OriginalFiles/Indonesia_Siberut_MG_Setiawan',
                         full.names = TRUE)

# List annotation and sound files
Annotations <- list.files(FolderList[1], full.names = T,recursive = T)
Sounds  <- list.files(FolderList[3], full.names = T,recursive = T)

# Loop through each annotation file
for(a in 1:length(Annotations)){
  
  # Read the annotation file
  Selections <- read.delim(Annotations[a])
  
  # Get the base name of the annotation file
  BaseName <- basename(Annotations[a])  
  
  # Create WAV file name by splitting the base name
  WavName <- str_split_fixed(BaseName, pattern = '_MG', n = 3)[,1]
  
  
  # Find the corresponding sound file based on the WAV name
  FullWav <- Sounds[str_detect(Sounds, WavName)]
  
  # Read the full WAV file
  Tempwav <- readWave(FullWav)
  
  
  # Extract short sound files based on the selection times
  short.sound.files <- lapply(1:(nrow(Selections)), function(i) {
    extractWave(
      Tempwav,
      from = Selections$Begin.Time..s.[i],
      to = Selections$End.Time..s.[i],
      xunit = c("time"),
      plot = F,
      output = "Wave"
    )
  })
  
  # Loop through each extracted sound file to save it
  for(b in 1:length(short.sound.files)){
    TempSelection <- Selections[b,]
    
    # Create a directory for the call type if it doesn't exist
    CallTypeOutputDir <- paste(ProjectOutputDir, TempSelection$Species, TempSelection$Call.Type, sep='/')
    dir.create(CallTypeOutputDir, recursive = T)  
    
    # Construct the output WAV file name
    SingleWavName <- paste(CallTypeOutputDir, '/', WavName, '_', b, '_', '.wav', sep='')
    
    # Write the short sound file to the specified location
    writeWave(short.sound.files[[b]], SingleWavName, extensible = F)
  }
}


# Indonesia_Aceh_LG_Setiawan ---------------------------------------------
# Define the project ID and output directory for the specific project
ProjectID <- "Indonesia_Aceh_LG_Setiawan/"
ProjectOutputDir <- paste(OutputDirectory, ProjectID, sep='/')

# List folders containing annotation and sound files
FolderList <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/OriginalFiles/Indonesia_Aceh_LG_Setiawan',
                         full.names = TRUE)

outputFile <- "Temp.wav"

# List annotation and sound files
Annotations <- list.files(FolderList[1], full.names = T,recursive = T)
Sounds  <- list.files(FolderList[2], full.names = T,recursive = T)

# Loop through each annotation file
for(a in 1:length(Annotations)){
  
  # Read the annotation file
  Selections <- read.delim(Annotations[a])
  
  # Get the base name of the annotation file
  BaseName <- basename(Annotations[a])  
  
  # Create WAV file name by splitting the base name
  WavName <- str_split_fixed(BaseName, pattern = '.LG', n = 3)[,1]
  
  
  # Find the corresponding sound file based on the WAV name
  FullWav <- Sounds[str_detect(Sounds, WavName)]
  
  # Read the full WAV file
  Tempwav <- readWave(FullWav)
  

  # Extract short sound files based on the selection times
  short.sound.files <- lapply(1:(nrow(Selections)), function(i) {
    extractWave(
      Tempwav,
      from = Selections$Begin.Time..s.[i],
      to = Selections$End.Time..s.[i],
      xunit = c("time"),
      plot = F,
      output = "Wave"
    )
  })
  
  # Loop through each extracted sound file to save it
  for(b in 1:length(short.sound.files)){
    TempSelection <- Selections[b,]
    
    # Create a directory for the call type if it doesn't exist
    CallTypeOutputDir <- paste(ProjectOutputDir, TempSelection$Species, TempSelection$Call.Type, sep='/')
    dir.create(CallTypeOutputDir, recursive = T)  
    
    # Construct the output WAV file name
    SingleWavName <- paste(CallTypeOutputDir, '/', WavName, '_', b, '_', '.wav', sep='')
    
    # Write the short sound file to the specified location
    writeWave(short.sound.files[[b]], SingleWavName, extensible = F)
  }
}


# Indonesia_Tahawa_HA_Morrow ---------------------------------------------
# Define the project ID and output directory for the specific project
ProjectID <- "Indonesia_Tahawa_HA_Morrow/"
ProjectOutputDir <- paste(OutputDirectory, ProjectID, sep='/')

# List folders containing annotation and sound files
FolderList <- list.files("/Users/denaclink/Library/CloudStorage/GoogleDrive-denajane13@gmail.com/.shortcut-targets-by-id/1o_rBlAfxY7ITD4Zj-wtMB6CHFfk9D37k/Gibbon Automated Detection Analysis/Acoustic Data/Indonesia_Tahawa_HA_Morrow",
                         full.names = TRUE)

FolderListFull <- list.files(FolderList[3],
                         full.names = TRUE,recursive = T)


FolderListShort <- basename(FolderListFull)

file.copy(from=FolderListFull,
to=paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/Indonesia_Tahawa_HA_Morrow_full/',FolderListShort, sep=''))

outputFile <- "Temp.wav"

# List annotation and sound files
Annotations <- list.files(FolderList[1], full.names = T,recursive = T)
Sounds  <- list.files(FolderList[3], full.names = T,recursive = T)

# Loop through each annotation file
for(a in 1:length(Annotations)){
  
  # Read the annotation file
  Selections <- read.delim(Annotations[a])
  
  # Get the base name of the annotation file
  BaseName <- basename(Annotations[a])  
  
  # Create WAV file name by splitting the base name
  WavName <- str_split_fixed(BaseName, pattern = '.Table', n = 3)[,1]
  
  # Find the corresponding sound file based on the WAV name
  FullWav <- Sounds[str_detect(Sounds, WavName)]
  
  system(paste(
    "flac", "-d", shQuote(FullWav), "-o", shQuote(outputFile)
  ))
  
  # Read the full WAV file
  Tempwav <- readWave(outputFile)
  
  file.remove(outputFile)
  
  # Extract short sound files based on the selection times
  short.sound.files <- lapply(1:(nrow(Selections)), function(i) {
    extractWave(
      Tempwav,
      from = Selections$Begin.Time..s.[i],
      to = Selections$End.Time..s.[i],
      xunit = c("time"),
      plot = F,
      output = "Wave"
    )
  })
  
  # Loop through each extracted sound file to save it
  for(b in 1:length(short.sound.files)){
    TempSelection <- Selections[b,]
    
    # Create a directory for the call type if it doesn't exist
    CallTypeOutputDir <- paste(ProjectOutputDir, TempSelection$Species, TempSelection$Call.Type, sep='/')
    dir.create(CallTypeOutputDir, recursive = T)  
    
    # Construct the output WAV file name
    SingleWavName <- paste(CallTypeOutputDir, '/', WavName, '_', b, '_', '.wav', sep='')
    
    # Write the short sound file to the specified location
    writeWave(short.sound.files[[b]], SingleWavName, extensible = F)
  }
}


# Indonesia_Sokokembang_HM_Aoliya ---------------------------------------------
# NOTE: had to remove $ from file names
# Define the project ID and output directory for the specific project
ProjectID <- "Indonesia_Sokokembang_HM_Aoliya/"
ProjectOutputDir <- paste(OutputDirectory, ProjectID, sep='/')

# List folders containing annotation and sound files
FolderList <- list.files("/Users/denaclink/Library/CloudStorage/GoogleDrive-denajane13@gmail.com/.shortcut-targets-by-id/1o_rBlAfxY7ITD4Zj-wtMB6CHFfk9D37k/Gibbon Automated Detection Analysis/Acoustic Data/Indonesia_Sokokembang_HM_Aoliya",
                         full.names = TRUE)

# List annotation and sound files
Annotations <- list.files(FolderList[1], full.names = T,recursive = T)
Sounds  <- list.files(FolderList[3], full.names = T,recursive = T)

# Loop through each annotation file
for(a in 1:length(Annotations)){
  
  # Read the annotation file
  Selections <- read.delim(Annotations[a])
  
  # Get the base name of the annotation file
  BaseName <- basename(Annotations[a])  
  
  # Create WAV file name by splitting the base name
  WavName <- str_split_fixed(BaseName, pattern = '.Table', n = 3)[,1]
  

  # Find the corresponding sound file based on the WAV name
  FullWav <- Sounds[str_detect(Sounds, WavName)]
  
  # Read the full WAV file
  Tempwav <- readWave(FullWav)
  
  # Extract short sound files based on the selection times
  short.sound.files <- lapply(1:(nrow(Selections)), function(i) {
    extractWave(
      Tempwav,
      from = Selections$Begin.Time..s.[i],
      to = Selections$End.Time..s.[i],
      xunit = c("time"),
      plot = F,
      output = "Wave"
    )
  })
  
  # Loop through each extracted sound file to save it
  for(b in 1:length(short.sound.files)){
    TempSelection <- Selections[b,]
    
    # Create a directory for the call type if it doesn't exist
    CallTypeOutputDir <- paste(ProjectOutputDir, TempSelection$Species, TempSelection$Call.Type, sep='/')
    dir.create(CallTypeOutputDir, recursive = T)  
    
    # Construct the output WAV file name
    SingleWavName <- paste(CallTypeOutputDir, '/', WavName, '_', b, '_', '.wav', sep='')
    
    # Write the short sound file to the specified location
    writeWave(short.sound.files[[b]], SingleWavName, extensible = F)
  }
}

# Indonesia_Sikundur_SS_D'Agostino ---------------------------------------------
# Define the project ID and output directory for the specific project
ProjectID <- "Indonesia_Sikundur_SS_D'Agostino/"
ProjectOutputDir <- paste(OutputDirectory, ProjectID, sep='/')

# List folders containing annotation and sound files
FolderList <- list.files("/Users/denaclink/Library/CloudStorage/GoogleDrive-denajane13@gmail.com/.shortcut-targets-by-id/1o_rBlAfxY7ITD4Zj-wtMB6CHFfk9D37k/Gibbon Automated Detection Analysis/Acoustic Data/Indonesia_Sikundur_SS_D'Agostino",
                         full.names = TRUE)

# List annotation and sound files
Annotations <- list.files(FolderList[2], full.names = T)
Sounds  <- list.files(FolderList[3], full.names = T,recursive = T)

# Loop through each annotation file
for(a in 1:length(Annotations)){
  
  # Read the annotation file
  Selections <- read.delim(Annotations[a])
  
  # Get the base name of the annotation file
  BaseName <- basename(Annotations[a])  
  
  # Create WAV file name by splitting the base name
  WavName <- str_split_fixed(BaseName, pattern = '.Table', n = 3)[,1]
  
  # Find the corresponding sound file based on the WAV name
  FullWav <- Sounds[str_detect(Sounds, WavName)][1]
  
  # Read the full WAV file
  Tempwav <- readWave(FullWav)
  
  # Extract short sound files based on the selection times
  short.sound.files <- lapply(1:(nrow(Selections)), function(i) {
    extractWave(
      Tempwav,
      from = Selections$Begin.Time..s.[i],
      to = Selections$End.Time..s.[i],
      xunit = c("time"),
      plot = F,
      output = "Wave"
    )
  })
  
  # Loop through each extracted sound file to save it
  for(b in 1:length(short.sound.files)){
    TempSelection <- Selections[b,]
    
    # Create a directory for the call type if it doesn't exist
    CallTypeOutputDir <- paste(ProjectOutputDir, TempSelection$Species, TempSelection$Call.Type, sep='/')
    dir.create(CallTypeOutputDir, recursive = T)  
    
    # Construct the output WAV file name
    SingleWavName <- paste(CallTypeOutputDir, '/', WavName, '_', b, '_', '.wav', sep='')
    
    # Write the short sound file to the specified location
    writeWave(short.sound.files[[b]], SingleWavName, extensible = F)
  }
}

# Indonesia_HutanHarapan_AG_Setiawan-------------------------------------------------------------------------

# Define the project ID and output directory for the specific project
ProjectID <- 'Indonesia_HutanHarapan_AG_Setiawan/'
ProjectOutputDir <- paste(OutputDirectory, ProjectID, sep='/')

# List folders containing annotation and sound files
FolderList <- list.files('/Users/denaclink/Library/CloudStorage/GoogleDrive-denajane13@gmail.com/.shortcut-targets-by-id/1o_rBlAfxY7ITD4Zj-wtMB6CHFfk9D37k/Gibbon Automated Detection Analysis/Acoustic Data/Indonesia_HutanHarapan_AG_Setiawan',
                         full.names = TRUE)

# List annotation and sound files
Annotations <- list.files(FolderList[1], full.names = T)
Sounds  <- list.files(FolderList[2], full.names = T)

# Loop through each annotation file
for(a in 1:length(Annotations)){
  
  # Read the annotation file
  Selections <- read.delim(Annotations[a])
  
  # Get the base name of the annotation file
  BaseName <- basename(Annotations[a])  
  
  # Create WAV file name by splitting the base name
  WavName <- paste(str_split_fixed(BaseName, pattern = '_', n = 3)[,1], 
                   str_split_fixed(BaseName, pattern = '_', n = 3)[,2], 
                   sep='_')
  
  # Find the corresponding sound file based on the WAV name
  FullWav <- Sounds[str_detect(Sounds, WavName)]
  
  # Read the full WAV file
  Tempwav <- readWave(FullWav)
  
  # Extract short sound files based on the selection times
  short.sound.files <- lapply(1:(nrow(Selections)), function(i) {
    extractWave(
      Tempwav,
      from = Selections$Begin.Time..s.[i],
      to = Selections$End.Time..s.[i],
      xunit = c("time"),
      plot = F,
      output = "Wave"
    )
  })
  
  # Loop through each extracted sound file to save it
  for(b in 1:length(short.sound.files)){
    TempSelection <- Selections[b,]
    
    # Create a directory for the call type if it doesn't exist
    CallTypeOutputDir <- paste(ProjectOutputDir, TempSelection$Species, TempSelection$Call.Type, sep='/')
    dir.create(CallTypeOutputDir, recursive = T)  
    
    # Construct the output WAV file name
    SingleWavName <- paste(CallTypeOutputDir, '/', WavName, '_', b, '_', '.wav', sep='')
    
    # Write the short sound file to the specified location
    writeWave(short.sound.files[[b]], SingleWavName, extensible = F)
  }
}

# Indonesia_Pematang Gadung_HA_Kennedi-------------------------------------------------------------------------
# Define the project ID and output directory for the specific project
ProjectID <- 'Indonesia_Pematang Gadung_HA_Kennedi/'
ProjectOutputDir <- paste(OutputDirectory, ProjectID, sep='/')

# List folders containing annotation and sound files
FolderList <- list.files('/Users/denaclink/Library/CloudStorage/GoogleDrive-denajane13@gmail.com/.shortcut-targets-by-id/1o_rBlAfxY7ITD4Zj-wtMB6CHFfk9D37k/Gibbon Automated Detection Analysis/Acoustic Data/Indonesia_Pematang Gadung_HA_Kennedi',
                         full.names = TRUE)

# List annotation and sound files
Annotations <- list.files(FolderList[1], full.names = T)
Sounds  <- list.files(FolderList[2], full.names = T,recursive = F)
outputFile <- 'Temp.wav'

# Loop through each annotation file
for(a in 2:length(Annotations)){
  print(a)
  # Read the annotation file
  Selections <- read.delim(Annotations[a])
  Selections <- subset(Selections,Species=="HA" & Call.Type =='FG')
  
  if(nrow(Selections) > 0){
  # Get the base name of the annotation file
  BaseName <- basename(Annotations[a])  
  
  TempSplit <- str_split_fixed(BaseName, pattern = '_', n=9)
  
  Folder <- paste(TempSplit[,1],TempSplit[,3],TempSplit[,4],TempSplit[,5],sep='_')
  
  SoundFolder <- Sounds[str_detect(Sounds,Folder)]
  
  ListFiles <- list.files(SoundFolder,full.names = T)
  
  # Calculate the number of files
  NumFiles <- length(ListFiles)
  
  # Create a data frame with start and stop times
  TimeDF <- data.frame(
    FileName = ListFiles,
    StartTime = seq(1, by = 600, length.out = NumFiles),           # Start times: 0, 600, 1200, ...
    StopTime = seq(601, by = 600, length.out = NumFiles)          # Stop times: 600, 1200, 1800, ...
  )
  
 
  
UpdatedSelections <- data.frame()  
 
for(y in 1:nrow(Selections)){
   TempSelection <- Selections[y,]
   
   TimeDFInBetween <- (TimeDF %>%
     mutate(InBetween = TempSelection$Begin.Time..s. > StartTime & TempSelection$Begin.Time..s. < StopTime))
   
   TempSub <-TimeDF[ which(TimeDFInBetween$InBetween == TRUE),]
  
   TempRow <- cbind.data.frame(TempSelection,TempSub[ nrow(TempSub) ,])
   UpdatedSelections <- rbind.data.frame(UpdatedSelections,TempRow)
 }
  

  UniqueWavs <- unique(UpdatedSelections$FileName)
  
  for(z in 1:length(UniqueWavs)){
  
  FullWav <-  UniqueWavs[z]
  
  system(paste(
    "flac", "-d", shQuote(FullWav), "-o", shQuote(outputFile)
  ))
  
  # Read the full WAV file
  Tempwav <- readWave(outputFile)
  
  WavName <- basename( UniqueWavs[z])
  WavName <- str_split_fixed(WavName,pattern = '.flac',n=2)[,1]
  
  file.remove(outputFile)
  
  ShortSelections <- subset(UpdatedSelections,FileName== FileName[z])
  
  ShortSelections$Begin.Time..s. <- ShortSelections$Begin.Time..s. - ShortSelections$StartTime
  ShortSelections$End.Time..s. <- ShortSelections$End.Time..s. - ShortSelections$StartTime
  
  # Extract short sound files based on the selection times
  short.sound.files <- lapply(1:(nrow(ShortSelections)), function(i) {
    extractWave(
      Tempwav,
      from = ShortSelections$Begin.Time..s.[i],
      to = ShortSelections$End.Time..s.[i],
      xunit = c("time"),
      plot = F,
      output = "Wave"
    )
  })
  
  # Loop through each extracted sound file to save it
  for(b in 1:length(short.sound.files)){
    TempSelection <- Selections[b,]
    
    # Create a directory for the call type if it doesn't exist
    CallTypeOutputDir <- paste(ProjectOutputDir, TempSelection$Species, TempSelection$Call.Type, sep='/')
    dir.create(CallTypeOutputDir, recursive = T)  
    
    # Construct the output WAV file name
    SingleWavName <- paste(CallTypeOutputDir, '/', WavName, '_', b, '_', '.wav', sep='')
    
    # Write the short sound file to the specified location
    writeWave(short.sound.files[[b]], SingleWavName, extensible = F)
  }
}
  }
}



# Clean halb --------------------------------------------------------------
Filelist <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Train/Indonesia_MungkuBaru_hylalb_Erb/HA_FG',
           full.names = T)

FilelistShort <- basename(Filelist)

file.copy(from=Filelist[which(str_detect(Filelist,'GC'))],
          to=paste('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Train/Indonesia_MungkuBaru_hylalb_Erb/HA_FG_clean/',
                   FilelistShort[which(str_detect(Filelist,'GC'))],sep='' ) )
