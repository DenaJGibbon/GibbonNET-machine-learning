library(dplyr)
library(stringr)
library(ggpubr)
library(dplyr)


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
  write.csv(BirdNetFeatures,'data/BirdNETFeatures_clean.csv',row.names = F)
  rm(TempDF)
  rm(CommaSplit)
  }
}




# BirdNET UMAP Plots ---------------------------------------------------

BirdNetFeatures <- na.omit(read.csv('data/BirdNETFeatures_clean.csv'))

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



#plot.for.BirdNETM2 <- subset(plot.for.BirdNETM2,Site=="hylmue_Lempake"|Site=="HM_Kukar"|Site=="HF_Danum"|Site=="HF_Maliau")

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





BirdNETUMAP <- umap::umap(BirdNetFeatures [, -c(2049)])

BirdNETUMAP <- as.data.frame(BirdNETUMAP$layout)
colnames(BirdNETUMAP) <- c('Dim.1','Dim.2')

BirdNETUMAP$Species <- plot.for.BirdNETM2$Species
BirdNETUMAP$Site <- plot.for.BirdNETM2$Site
colnames(BirdNETUMAP)


plot.for.BirdNETM2$Species[which(BirdNetFeatures$ProjectID=='Indonesia_Sokokembang_HM_Aoliya')] <- 'JG'


BirdNETUMAP$Species <- plyr::revalue(as.factor(BirdNETUMAP$Species),
                                           c("SS"='Siamang','AG'='Agile','HA'='White bearded', 'JG'= 'Javan', 'MG'= 'Kloss',
                                             'HF'= 'Northern grey','HM'='Müller','LG'='Lar','NG'='Crested'))


means <- BirdNETUMAP %>% 
  group_by(Species) %>% 
  summarize(across(c(Dim.1, Dim.2), mean))

means[3,]$Dim.2 <- means[3,]$Dim.2 -1
means[4,]$Dim.2 <- means[4,]$Dim.2 +1

Dim.1.2 <- ggpubr::ggscatter(data = BirdNETUMAP,x = "Dim.1",
                  y = "Dim.2",size = 1,
                  color='Species',alpha=1)+ scale_color_manual(values =matlab::jet.colors (length(
                    unique(BirdNETUMAP$Species)
                  ))) + 
  geom_point(size=4, data=means,color=matlab::jet.colors (length(
    unique(BirdNETUMAP$Species)))) + 
  geom_label(alpha=0.5,aes(label=Species), fill=matlab::jet.colors (length(
    unique(BirdNETUMAP$Species))), nudge_y =1, data=means) + 
  ggpubr::theme_pubr()+guides(color="none",fill="none")+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())

Dim.1.2


BirdNETUMAP$Genera <- plyr::revalue(BirdNETUMAP$Species,
                                            c('Siamang'='Symphalangus',
                                              'Crested'='Nomasus'))

BirdNETUMAP$Genera <- as.character(BirdNETUMAP$Genera)
BirdNETUMAP$Genera <- ifelse(BirdNETUMAP$Genera != 'Symphalangus' & BirdNETUMAP$Genera != 'Nomasus', 'Hylobates', BirdNETUMAP$Genera)

means.genera <- BirdNETUMAP %>% 
  group_by(Genera) %>% 
  summarize(across(c(Dim.1, Dim.2), mean))


Dim.1.2.genera <- ggpubr::ggscatter(data = BirdNETUMAP,x = "Dim.1",
                           y = "Dim.2",size = 1,#ellipse=TRUE,
                           color='Genera',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                             unique(BirdNETUMAP$Genera)
                           ))) + 
  geom_point(size=4, data=means.genera,color=matlab::jet.colors (length(
    unique(BirdNETUMAP$Genera)))) + 
  geom_label(alpha=0.5,aes(label=Genera), fill=matlab::jet.colors (length(
    unique(BirdNETUMAP$Genera))), nudge_y =1, data=means.genera) + 
  guides(color="none",fill="none")+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())

Dim.1.2.genera

BirdNETUMAPHylobates <- subset(BirdNETUMAP,Species=='Northern grey' | Species=='Müller')

means.Hylobates <- BirdNETUMAPHylobates %>% 
  group_by(Species) %>% 
  summarize(across(c(Dim.1, Dim.2), mean))

Dim.1.2.hylobates <-ggpubr::ggscatter(data = BirdNETUMAPHylobates,x = "Dim.1",
                  y = "Dim.2",size = 1,#ellipse=TRUE,
                  color='Species',alpha=0.5)+ scale_color_manual(values= c('grey',
                                                                           'orange')) + 
  geom_point(size=4, data=means.Hylobates,color=c('grey',
                                                  'orange')) + 
  geom_label(alpha=0.5,aes(label=Species), fill=c('grey',
                                        'orange'), nudge_y =1, data=means.Hylobates) + 
  ggpubr::theme_pubr()+guides(color="none",fill="none")+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())



BirdNETUMAPLar <- subset(BirdNETUMAP,Site=="LG_Sikundur" |Site== "LG_Kenyir State Park")

means.lar <- BirdNETUMAPLar %>% 
  group_by(Site) %>% 
  summarize(across(c(Dim.1, Dim.2), mean))


BirdNETUMAPLar$Site <- revalue(BirdNETUMAPLar$Site,
        c("LG_Sikundur"="Indonesia (lar)",
          "LG_Kenyir State Park"= "Malaysia (lar)"))

                                                       
Dim.1.2.lar <- ggpubr::ggscatter(data = BirdNETUMAPLar,x = "Dim.1",
                  y = "Dim.2",size = 1,#ellipse=TRUE,
                  color='Site',alpha=0.5)+ scale_color_manual(values= c('grey',
                                                                        'red')) +
  geom_point(size=4, data=means.lar,color=c('red',
                                                  'grey')) + 
  geom_label(alpha=0.5,aes(label=Site), fill=c('red',
                                        'grey'), nudge_y =1, data=means.lar) + 
  ggpubr::theme_pubr()+guides(color="none",fill="none")+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank())

#pdf(file='Unsupervisedplot_UMAP.pdf', height=14,width = 12)
cowplot::plot_grid(Dim.1.2,
                   Dim.1.2.genera,
                   Dim.1.2.lar,
                   Dim.1.2.hylobates, 
                   nrow=2,
                   labels = c('A)','B)','C)','D)'),
                   label_x = 0.9)
#graphics.off()



AllHDBSCAN <- dbscan::hdbscan(BirdNetFeatures [, -c(2049)],minPts=15)

class_counts <- table(BirdNETUMAP$Species, AllHDBSCAN$cluster)


cluster_with_most_class <-
  colnames(class_counts)[which.max(class_counts[target_class,])]

Binary <-
  ifelse(TempCluster$cluster == cluster_with_most_class,
         target_class,
         "Noise")
BinaryLabels <-
  ifelse(Embeddings$Label == target_class, target_class, "Noise")

Binary <-
  factor(Binary, levels = levels(as.factor(BinaryLabels)))

ConfMat <- caret::confusionMatrix(
  as.factor(Binary),
  as.factor(BinaryLabels),
  mode = "everything",
  positive = target_class
)
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
