library(stringr)

# All data ----------------------------------------------------------------

FullNames <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/ClipsByProjectID',recursive = T)

length(FullNames)

ClassName <-str_split_fixed(dirname(FullNames), pattern = '/', n=2)[,2]

table(ClassName)


# Train and validation ----------------------------------------------------

FullNames <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Train/',recursive = T)

length(FullNames)

ClassName <-str_split_fixed(dirname(FullNames), pattern = '/', n=2)[,2]

table(ClassName)

# Test ----------------------------------------------------

FullNames <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/',recursive = T)

length(FullNames)

ClassName <-str_split_fixed(dirname(FullNames), pattern = '/', n=2)[,2]

table(ClassName)
