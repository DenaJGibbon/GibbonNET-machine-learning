library(dplyr)
library(stringr)
library(ggpubr)

# BirdNET features --------------------------------------------------------

BirdNetFiles <- 
  list.files("/Volumes/DJC Files/OrxyGibbonAutomatedDetection/ClipsForEmbeddings/embeddings/",
             full.names = T)

BirdNetFilesShort <- 
  list.files("/Volumes/DJC Files/OrxyGibbonAutomatedDetection/ClipsForEmbeddings/embeddings/",
             full.names = F)


BirdNetFeatures <- data.frame()

for(a in 1:length(BirdNetFiles)){ # 
  Embeddingslist <- list.files(BirdNetFiles[[a]],recursive = T,full.names = T)
  
  Length <- ifelse( length(Embeddingslist) > 199, 200,length(Embeddingslist))
  Embeddingslist <- Embeddingslist[1:Length]
  print(paste(a, ' out of this many files', length(Embeddingslist)))
  
  ProjectID <- BirdNetFilesShort[a]
  print(ProjectID)
  
  for(b in 1:length(Embeddingslist)){ 
    
  TempDF <-read.table(Embeddingslist[b])
  CommaSplit <- strsplit(TempDF$V3,split = ',')
  
  CombinedValsDF <- data.frame()
  for(c in 1:length(CommaSplit)){
    TempVec <- CommaSplit[[c]]
    CombinedVals <- data.frame()
    CombinedVals <- rbind.data.frame(CombinedVals,TempVec)
    colnames(CombinedVals) <- paste('var_', seq(1,length(TempVec),1),sep='' )
    CombinedValsDF <- rbind.data.frame(CombinedValsDF,CombinedVals)
  }
  
  CombinedValsDF <- 
    CombinedValsDF %>% mutate_if(is.character,as.numeric)
  
  CombinedValsDFMean <- t(apply(CombinedValsDF,2,mean))
  CombinedValsSD <- t(apply(CombinedValsDF,2,sd))
  CombinedVals <-  cbind.data.frame(CombinedValsDFMean,CombinedValsSD)
  NewDataFrame <- data.frame()
  
  NewDataFrame <- rbind.data.frame(NewDataFrame,CombinedVals)
  colnames(NewDataFrame) <-  paste('var_', seq(1,ncol(NewDataFrame),1),sep='' )

  NewDataFrame$ProjectID <- ProjectID

  BirdNetFeatures <- rbind.data.frame(BirdNetFeatures,NewDataFrame)
 # write.csv(BirdNetFeatures,'data/BirdNETFeatures.csv',row.names = F)
  rm(TempDF)
  rm(CommaSplit)
  }
}




# BirdNET UMAP Plots ---------------------------------------------------

BirdNetFeatures <- na.omit(read.csv('data/BirdNETFeatures.csv'))

# Convert 'ProjectID' column to a factor
BirdNetFeatures$ProjectID <- as.factor(BirdNetFeatures$ProjectID)

BirdNETM2.umap <-
  umap::umap(BirdNetFeatures [, -c(2049)],
             #labels=as.factor(BirdNET$Validation),
             controlscale=TRUE,scale=3,n_neighbors=5)

plot.for.BirdNETM2 <-
  cbind.data.frame(BirdNETM2.umap$layout[,1:2],BirdNetFeatures $ProjectID)

colnames(plot.for.BirdNETM2) <-
  c("Dim.1", "Dim.2","Class")

plot.for.BirdNETM2$Species <- str_split_fixed(plot.for.BirdNETM2$Class,
                pattern = '_', n=4)[,3]

plot.for.BirdNETM2$Site <- paste(plot.for.BirdNETM2$Species,
                                 str_split_fixed(plot.for.BirdNETM2$Class,
                                                 pattern = '_', n=4)[,2],
                                 sep='_')


plot.for.BirdNETM2$Species <- plyr::revalue(plot.for.BirdNETM2$Species,
        c('hylalb'='HA',
          'hylmue'='HM'))




BirdNETM2Scatter <- ggpubr::ggscatter(data = plot.for.BirdNETM2,x = "Dim.1",
                                      y = "Dim.2",
                                      color='Site')+#guides(color='none')+
  scale_color_manual(values =matlab::jet.colors (length(
    unique(plot.for.BirdNETM2$Site)
  ))) + ggtitle( paste('Site'))+theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  

BirdNETM2Scatter


BirdNETM2ScatterSpecies <- ggpubr::ggscatter(data = plot.for.BirdNETM2,x = "Dim.1",
                                      y = "Dim.2",
                                      color='Species')+#guides(color='none')+
  scale_color_manual(values =matlab::jet.colors (length(
    unique(plot.for.BirdNETM2$Species)
  ))) + ggtitle( paste('Species'))+theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  

BirdNETM2ScatterSpecies





BirdNETPCA <- prcomp(BirdNetFeatures [, -c(2049)])
BirdNETPCA <- as.data.frame(BirdNETPCA$x[,1:3])
BirdNETPCA$Species <- plot.for.BirdNETM2$Species
BirdNETPCA$Site <- plot.for.BirdNETM2$Site
colnames(BirdNETPCA)

plot.for.BirdNETM2$Species[which(BirdNetFeatures$ProjectID=='Indonesia_Sokokembang_HM_Aoliya')] <- 'JG'


BirdNETPCA$Species <- plyr::revalue(as.factor(BirdNETPCA$Species),
                                           c("SS"='Siamang','AG'='Agile','HA'='White bearded', 'JG'= 'Javan', 'MG'= 'Mentawi',
                                             'HF'= 'Northern grey','HM'='Müller','LG'='Lar','NG'='Crested'))


PC1.2 <- ggpubr::ggscatter(data = BirdNETPCA,x = "PC1",
                  y = "PC2",size = 0.75,ellipse=TRUE,
                  color='Species',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                    unique(BirdNETPCA$Species)
                  ))) 

PC1.3 <- ggpubr::ggscatter(data = BirdNETPCA,x = "PC1",
                  y = "PC3",size = 0.75,ellipse=TRUE,
                  color='Species',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                    unique(BirdNETPCA$Species)
                  ))) 


BirdNETPCA$Genera <- plyr::revalue(BirdNETPCA$Species,
                                            c('Siamang'='Symphalangus',
                                              'Crested'='Nomasus'))

BirdNETPCA$Genera <- as.character(BirdNETPCA$Genera)
BirdNETPCA$Genera <- ifelse(BirdNETPCA$Genera != 'Symphalangus' & BirdNETPCA$Genera != 'Nomasus', 'Hylobates', BirdNETPCA$Genera)


PC1.2.genera <- ggpubr::ggscatter(data = BirdNETPCA,x = "PC1",
                           y = "PC2",size = 0.75,ellipse=TRUE,
                           color='Genera',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                             unique(BirdNETPCA$Genera)
                           ))) 

PC1.3.genera <- ggpubr::ggscatter(data = BirdNETPCA,x = "PC1",
                                  y = "PC3",size = 0.75,ellipse=TRUE,
                                  color='Genera',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                                    unique(BirdNETPCA$Genera)
                                  ))) 




BirdNETPCAHylobates <- subset(BirdNETPCA,Species=='Northern grey' | Species=='Müller')

PC1.3.hylobates <- ggpubr::ggscatter(data = BirdNETPCAHylobates,x = "PC1",
                  y = "PC3",size = 0.75,ellipse=TRUE,
                  color='Species',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                    unique(BirdNETPCA$Species)
                  ))) 

PC1.2.hylobates <-ggpubr::ggscatter(data = BirdNETPCAHylobates,x = "PC1",
                  y = "PC2",size = 0.75,ellipse=TRUE,
                  color='Species',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                    unique(BirdNETPCA$Species)
                  ))) 


BirdNETPCALar <- subset(BirdNETPCA,Site=="LG_Sikundur" |Site== "LG_Kenyir State Park")

BirdNETPCALar$Site <- revalue(BirdNETPCALar$Site,
        c("LG_Sikundur"="Indonesia (lar)",
          "LG_Kenyir State Park"= "Malaysia (lar)"))

PC1.3.lar <- ggpubr::ggscatter(data = BirdNETPCALar,x = "PC1",
                  y = "PC3",size = 0.75,ellipse=TRUE,
                  color='Site',alpha=0.5)+ scale_color_manual(values= c('yellow',
                                                                      'blue')) 

PC1.2.lar <- ggpubr::ggscatter(data = BirdNETPCALar,x = "PC1",
                  y = "PC2",size = 0.75,ellipse=TRUE,
                  color='Site',alpha=0.5)+ scale_color_manual(values= c('yellow',
                                                                        'blue')) 

pdf(file='Unsupervisedplot.pdf', height=14,width = 12)
cowplot::plot_grid(PC1.2,PC1.3,
                   PC1.2.genera,PC1.3.genera,
                   PC1.2.lar,PC1.3.lar,
                   PC1.2.hylobates, PC1.3.hylobates,
                   nrow=4,
                   labels = c('A','B','C','D','E','F','G','H'),
                   label_x = 0.99)
graphics.off()
# Sort annotations
# ListofAnnot <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Malaysia_Kenyir_LG_George/Selection Table',
#            full.names = T)
# ListofAnnotShort <- basename(ListofAnnot)
# 
# 
# 
# Combinelist <- lapply(ListofAnnot, read.delim)
# OutputDir <- '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Malaysia_Kenyir_LG_George/subset'
# 
# for(a in 1:length(Combinelist)){
# 
#   TempDF <- Combinelist[[a]]
#   CheckNA <- is.na(TempDF$Call.Type)
#   SumNA <- sum(str_count(CheckNA,'TRUE'))
#   
#   if(SumNA ==0  ){
#   
#   file.copy(from=ListofAnnot[[a]],
#             to=paste(OutputDir,ListofAnnotShort[[a]],sep='/')
#             )  
#   
#   }
#   
# }
# 
# ListofAnnot <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Indonesia_Tahawa_HA_Morrow_full/Annotations',
#                           full.names = T, pattern = '.txt')
# 
# ListofAnnotShort <- basename(ListofAnnot)
# 
# Combinelist <- lapply(ListofAnnot, read.delim)
# OutputDir <- '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Indonesia_Tahawa_HA_Morrow_full/Annotations/subset'
# 
# 
# for(a in 1:length(Combinelist)){
#   
#   TempDF <- Combinelist[[a]]
#   CheckNA <- is.na(TempDF$Call)
#   CheckNA <- is.na(TempDF[,c(1:8,15:17)])
#   SumNA <- sum(str_count(CheckNA,'TRUE'))
#   
#   if(SumNA ==0  ){
#     
#     file.copy(from=ListofAnnot[[a]],
#               to=paste(OutputDir,ListofAnnotShort[[a]],sep='/')
#     )  
#     
#   } else{
#    
#     print('contains NA')
#     print( ListofAnnotShort[[a]])
#   }
#   
# }
# 
