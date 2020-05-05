# Reduce data to appropriate components
# @AUTHOR: Mircea Davidescu
# @
# INPUT:
#   + fluctuationVectors.rds: the fluctuation fields for all collectives in the data set.
# OUTPUT:
#   + correlationProfiles.rds: a file containing the correlation profile for each frame for each animal.

# Data Processing 
# For each fluctuation vector, calculate the pairwise correlations across 3,600 randomly-selected vectors. Use this to determine the spatial correlation function.

if (!file.exists(paste(projectFolder, "Data_Processed", "correlationProfiles.rds", sep = "/"))) {
  animalVectors = read_rds(paste(projectFolder, "fluctuationVectors.rds", sep = "/"))
  nestedVelocity = select(animalVectors, folder, Frame, X, Y, vX, vY) %>% group_by(folder, Frame) %>% nest(.key = "velocityField")
  nestedFluctuation = select(animalVectors, folder, Frame, X, Y, fluctX, fluctY) %>% group_by(folder, Frame) %>% nest(.key = "fluctField")
  nestedData = inner_join(nestedVelocity, nestedFluctuation) %>% anti_join(invalidSampleFrames)
  
  
  animalVectors = nestedData %>% partition(folder) %>% cluster_library("purrr") %>% cluster_library("dplyr") %>% cluster_library("scalefree") %>%
    mutate(correlationFunction = map2(velocityField, fluctField, possibly(function(x,y) {calcCorrelationFunction(x,y, 3600)}, NA))) %>% collect() %>%
    ungroup()
  # Check the returns that gave NA!!! Why do they give NA?
  
  correlationProfiles = select(animalVectors, folder, Frame, correlationFunction)
  write_rds(correlationProfiles, paste(projectFolder, "correlationProfiles.rds", sep="/"))
  rm(list = c("animalVectors"))
}