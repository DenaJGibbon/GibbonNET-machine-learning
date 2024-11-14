library(gibbonNetR)

# Lar gibbon --------------------------------------------------------------
# Create spectorgram images for training data
spectrogram_images(
  trainingBasePath = '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/ClipsByProjectID/Indonesia_Sikundur_LG_Ginting/',
  outputBasePath = 'data/gibbonnetr/imageslar/',
  minfreq.khz = 0.4,
  maxfreq.khz = 1.6,
  splits = c(.8, 0.2, 0), # Assign proportion to training, validation, or test folders
  new.sampleratehz = 'NA'
)


# Create spectorgram images
spectrogram_images(
  trainingBasePath = '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Valid/Malaysia_Kenyir State Park_LG_George/',
  outputBasePath = 'data/gibbonnetr/imageslar/',
  minfreq.khz = 0.4,
  maxfreq.khz = 1.6,
  splits = c(0, 0, 1), # Assign proportion to training, validation, or test folders
  new.sampleratehz = 'NA'
)


input.data.path <- 'data/gibbonnetr/imageslar/'

test.data.path <- 'data/gibbonnetr/imageslar/test/'

trainingfolder.short <- 'imageslar'

epoch.iterations <- c(20)

gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture ='resnet50',
                            learning_rate = 0.001,
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            early.stop = "yes",
                            output.base.path = "data/gibbonnetr/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "noise")


evaluate_trainedmodel_performance_multi(
  trained_models_dir='data/gibbonnetr/_imageslar_multi_unfrozen_TRUE_',
  test.data.path,
  output_dir = "data/",
  class_names=c("LG_FG", "LG_MC", "noise"),
  noise.category = "noise",
  unfreeze = TRUE
)

performancetables.dir <- 'data/gibbonnetr/_imageslar_multi_unfrozen_TRUE_/performance_tables_multi'
PerformanceOutput <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir,
                                                      class='LG_FG',
                                                      model.type = "multi",Thresh.val=0)

PerformanceOutput$f1_plot

ModelPath <- 'data/gibbonnetr/_imageslar_multi_unfrozen_TRUE_/_imageslar_20_resnet50_model.pt'

# Deploy trained model over sound files
deploy_CNN_multi(
  clip_duration = 12,
  architecture='resnet50',
  output_folder = paste(tempdir(),'/data/Results/Images/',sep=''),
  output_folder_selections = paste(tempdir(),'/data/Results/Selections/',sep=''),
  output_folder_wav = paste(tempdir(),'/data/Results/Wavs/',sep=''),
  detect_pattern=NA,
  top_model_path = ModelPath,
  path_to_files = '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/Malaysia_Kenyir_LG_George/AudioFilesWav/',
  downsample_rate = 'NA',
  save_wav = F,
  class_names = c("LG_FG", "LG_MC", "noise"),
  noise_category = 'noise',
  single_class = TRUE,
  single_class_category = 'LG_FG',
  threshold = .1,
  max_freq_khz = 2
)





# # Covert to wav for processing
# flacfiles <-  list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Indonesia_Tahawa_HA_Morrow_full/',
#                          full.names = T)
# 
# outputdir <-  '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Indonesia_Tahawa_HA_Morrow_full/wavfiles/'
# 
# for(a in 10:length(flacfiles)){
# 
# outputFile <- paste(outputdir, str_split_fixed(basename(flacfiles[a]), '.flac',n=2)[,1] ,'.wav' , sep='')
# 
# system(paste(
#   "flac", "-d", shQuote(flacfiles[a]), "-o", shQuote(outputFile)
# ))
# 
#  }
# 
# 
ModelPath <- "data/gibbonnetr/_imageslar_multi_unfrozen_TRUE_/_imageslar_20_resnet50_model.pt"
result <- extract_embeddings(test_input="data/gibbonnetr/images_clustering/",
                             model_path=ModelPath,
                             target_class = "LG_FG_Malaysia")

result$EmbeddingsCombined


# Northern grey gibbon ----------------------------------------------------


# White bearded gibbon ----------------------------------------------------
# Create spectorgram images for training data
spectrogram_images(
  trainingBasePath = '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAllClips/TrainBinaryHA/',
  outputBasePath = 'data/gibbonnetr/imagesalbibarbis/',
  minfreq.khz = 0.4,
  maxfreq.khz = 1.6,
  splits = c(.8, 0.2, 0), # Assign proportion to training, validation, or test folders
  new.sampleratehz = 'NA'
)


# Create spectorgram images
spectrogram_images(
  trainingBasePath = '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Valid/Indonesia_Pematang Gadung_HA_Kennedi/',
  outputBasePath = 'data/gibbonnetr/imagesalbibarbis/',
  minfreq.khz = 0.4,
  maxfreq.khz = 1.6,
  splits = c(0, 0, 1), # Assign proportion to training, validation, or test folders
  new.sampleratehz = 24000
)


input.data.path <- 'data/gibbonnetr/imagesalbibarbis/'

test.data.path <- 'data/gibbonnetr/imagesalbibarbis/test/'

trainingfolder.short <- 'imagesalbibarbis'

epoch.iterations <- c(3)

gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='resnet50',
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             save.model= TRUE,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                             output.base.path = "data/gibbonnetr/",
                             trainingfolder=trainingfolder.short,
                             positive.class="HA",
                             negative.class="noise")



evaluate_trainedmodel_performance(
  trained_models_dir='data/gibbonnetr/_imagesalbibarbis_binary_unfrozen_TRUE_',
  test.data.path,
  output_dir = "data/",
  positive.class =c("HA_FG"),
  negative.class = "noise"
)

performancetables.dir <- 'data/gibbonnetr/_imagesalbibarbis_binary_unfrozen_TRUE_/performance_tables'
PerformanceOutput <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir,
                                                      class='HA',
                                                      model.type = "binary",Thresh.val=0)

PerformanceOutput$f1_plot
PerformanceOutput$pr_plot


ModelPath <- '/Users/denaclink/Desktop/RStudioProjects/BEAT Sessions/GibbonNET-machine-learning/data/gibbonnetr/_imagesalbibarbis_binary_unfrozen_TRUE_/_imagesalbibarbis_3_resnet50_model.pt'

deploy_CNN_binary (
  clip_duration = 12,
  architecture='resnet50',
  output_folder = paste(tempdir(),'/BinaryDir/Results/Images/',sep=''),
  output_folder_selections = paste(tempdir(),'/BinaryDir/Results/Selections/',sep=''),
  output_folder_wav = paste(tempdir(),'/BinaryDir/Results/Wavs/',sep=''),
  detect_pattern=NA,
  top_model_path = ModelPath,
  path_to_files = '/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAndTest/Test/full_sound_files/Indonesia_Tahawa_HA_Morrow_full/wavfiles/',
  downsample_rate = 'NA',
  threshold = 0.9,
  save_wav = F,
  positive.class = 'HA',
  negative.class = 'noise',
  max_freq_khz = 2
)

# All species -------------------------------------------------------------

# Create spectorgram images for training data
spectrogram_images(
  trainingBasePath = "/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAllClips/Train/",
  outputBasePath = 'data/gibbonnetr/imagesall/',
  minfreq.khz = 0.4,
  maxfreq.khz = 2,
  splits = c(.8, 0.2, 0), # Assign proportion to training, validation, or test folders
  new.sampleratehz = 16000
)


spectrogram_images(
  trainingBasePath ='/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAllClips/Test/',
  outputBasePath = 'data/gibbonnetr/imagesall/',
  minfreq.khz = 0.4,
  maxfreq.khz =2,
  splits = c(0,0,1), # Assign proportion to training, validation, or test folders
  new.sampleratehz = 16000
)

input.data.path <- 'data/gibbonnetr/imagesall/'

test.data.path <- 'data/gibbonnetr/imagesall/test/'

trainingfolder.short <- 'imagesall'

epoch.iterations <- c(20)

gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture ='resnet50',
                            learning_rate = 0.001,
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            class_weights = c(rep(1/9,9)),
                            early.stop = "yes",
                            output.base.path = "data/gibbonnetr/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "noise")




ModelPath <- "data/gibbonnetr/_imageslar_multi_unfrozen_TRUE_/_imageslar_20_resnet50_model.pt"
result <- extract_embeddings(test_input="data/gibbonnetr/imagesall/train/",
                             model_path=ModelPath,
                             target_class = "HF")

result$EmbeddingsCombined


# Embeddings comparison ---------------------------------------------------
test_input="data/gibbonnetr/imagesall/train/"

# Load the fine-tuned model
fine_tuned_model <- luz_load(ModelPath)


# Create a dataset from the test images
test_ds <- image_folder_dataset(
  file.path(test_input),
  transform = . %>%
    torchvision::transform_to_tensor() %>%
    torchvision::transform_resize(size = c(224, 224)) %>%
    torchvision::transform_normalize(
      mean = c(0.485, 0.456, 0.406),
      std = c(0.229, 0.224, 0.225)
    ),
  target_transform = function(x)
    as.double(x) - 1
)

# Create a dataloader
test_dl <- dataloader(test_ds, batch_size = 32, shuffle = FALSE)

# Define the module
net <- torch::nn_module(
  initialize = function() {
    self$model <- fine_tuned_model
    self$feature_extractor <- nn_sequential(
      self$model$features,
      self$model$avgpool,
      nn_flatten(start_dim = 2),
      self$model$classifier[1:6],
      nn_linear(150528, 1024)
    )
    for (par in self$parameters) {
      par$requires_grad_(FALSE)
    }
  },
  forward = function(x) {
    x %>% self$feature_extractor()
  }
)

# Create a new instance of the module
net <- net()

print("processing embeddings")
# Extract features
features <- list()

coro::loop(for (batch in test_dl) {
  inputs <- batch[[1]]
  with_no_grad({
    outputs <- net$forward(inputs)
  })
  features <- c(features, list(outputs$cpu() %>% as_array()))
})

features <- do.call(rbind, features)

# Read Embeddings.csv
Embeddings <- as.data.frame(features)

TempName <- list.files(test_input, recursive = TRUE)


ImageNETPCA <- prcomp(Embeddings [, -c(1025)])
ImageNETPCA <- as.data.frame(ImageNETPCA$x[,1:3])
ImageNETPCA$Species <- str_split_fixed(TempName,pattern = '/', n=2)[,1]

colnames(ImageNETPCA)

ggpubr::ggscatter(data = ImageNETPCA,x = "PC1",
                  y = "PC2",size = 0.75,#ellipse=TRUE,
                  color='Species',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                    unique(ImageNETPCA$Species)
                  ))) 


# MFCCs comparison --------------------------------------------------------
source('/Users/denaclink/Desktop/RStudioProjects/gibbonR/R/MFCCFunction.R')
Trainlist <- list.files('/Volumes/DJC Files/OrxyGibbonAutomatedDetection/TrainAllClips/Train',
           full.names = T)

CombinedMFCC <- data.frame()
for(a in 1:length(Trainlist)){
  
 MFCCDF <-  MFCCFunction(input.dir =Trainlist[a],win.avg = 'mean.sd' )
 MFCCDF$class <- basename(Trainlist[a] )
 CombinedMFCC <-rbind.data.frame(CombinedMFCC,MFCCDF)
}


MFCCPCA <- prcomp(CombinedMFCC [, -c(1)])
MFCCPCA <- as.data.frame(MFCCPCA$x[,1:3])
MFCCPCA$Species <- CombinedMFCC$class

ggpubr::ggscatter(data = MFCCPCA,x = "PC1",
                  y = "PC2",size = 0.75,ellipse=TRUE,
                  color='Species',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                    unique(MFCCPCA$Species)
                  ))) 

ggpubr::ggscatter(data = MFCCPCA,x = "PC1",
                  y = "PC3",size = 0.75,ellipse=TRUE,
                  color='Species',alpha=0.5)+ scale_color_manual(values =matlab::jet.colors (length(
                    unique(MFCCPCA$Species)
                  ))) 
