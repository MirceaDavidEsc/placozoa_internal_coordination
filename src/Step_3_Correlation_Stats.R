## Calculate the correlation statistics for each frame and each animal.
# @AUTHOR: Mircea Davidescu
# @
# INPUT: 
#   + correlationProfiles.rds: a file containing all of the correlation profiles for the animals.
#   + allShapeData.rds: a file containing all of the shape data (including size) for each animal.
# OUTPUT:
#   + meanProfiles.rds: a file containing the mean correlation profile for each animal, averaged across time.
#   + meanProfileStats.csv: a file containing the mean correlation statistics (correlation length, positive integral) for the animals.

mapCorrStats = function(correlationFunctions) {
  # Get the profiles before they become negative.
  velocityProfile = correlationFunctions %>% select(domain, vCorr) 
  directionProfile = correlationFunctions %>% select(domain, dCorr) 
  speedProfile = correlationFunctions %>% select(domain, sCorr) 
  
  # Get the correltion lengths for each type of correlation.
  vZero = getFirstZeroCrossing(correlationFunctions$vCorr, distMapping = correlationFunctions$domain)
  dZero = getFirstZeroCrossing(correlationFunctions$dCorr, distMapping = correlationFunctions$domain)
  sZero = getFirstZeroCrossing(correlationFunctions$sCorr, distMapping = correlationFunctions$domain)
  
  # Susceptibility integrates correlation up to the correlation length, according to Attanasi et al. 2014
  # You need an "NA" condition in the rare cases that the correlation profile never becomes negative.
  if (is.na(vZero)) {positiveV = NA} else {positiveV = filter(velocityProfile, domain <= vZero)}
  if (is.na(dZero)) {positiveD = NA} else {positiveD = filter(directionProfile, domain <= dZero)}
  if (is.na(sZero)) {positiveS = NA} else {positiveS = filter(speedProfile, domain <= sZero)}
  
  # Get the integral of the positive correlation profile.
  if (length(positiveV) == 1) {vSuscept = NA} else {vSuscept = calculateSusceptibility(distance = positiveV$domain, correlation = positiveV$vCorr)}
  if (length(positiveD) == 1) {dSuscept = NA} else {dSuscept = calculateSusceptibility(distance = positiveD$domain, correlation = positiveD$dCorr)}
  if (length(positiveS) == 1) {sSuscept = NA} else {sSuscept = calculateSusceptibility(distance = positiveS$domain, correlation = positiveS$sCorr)}
  
  statsDF = data.frame(vZero, dZero, sZero, vSuscept, dSuscept, sSuscept)
  return(statsDF)
}

correlationStats = read_rds(paste(projectFolder, "correlationProfiles.rds", sep = "/")) %>% partition(folder) %>% cluster_library("scalefree") %>%
  cluster_library("dplyr") %>% cluster_library("pracma") %>% cluster_library("purrr") %>% cluster_copy(mapCorrStats) %>% 
  mutate(corrStats = map(correlationFunction, mapCorrStats)) %>% collect() %>% select(folder, Frame, corrStats) %>% unnest() %>% arrange(folder, Frame)
write_rds(correlationStats, paste(projectFolder, "correlationStats.rds", sep = ""))

# Get correlation statistics per each frame.
allShapeData = read_rds(paste(projectFolder, "Data_Processed/allShapeData.rds", sep = "")) %>% mutate(folder = basename(folder)) %>% filter(!(folder %in% basename(weirdFolders)))
correlationStats = read_rds(paste(projectFolder, "correlationStats.rds", sep = ""))
sizesAndCorrelations = inner_join(allShapeData, correlationStats)
rm(list = "allShapeData")

numMeasures = group_by(sizesAndCorrelations, folder) %>% summarise(numFrames = n())

perFrameAvgCorrStats = group_by(sizesAndCorrelations,folder) %>%
  summarise_each(funs(mean, sd), c(EquivalentDiameter, vZero, dZero, sZero, vSuscept, dSuscept, sSuscept)) %>%
  inner_join(numMeasures)
write_csv(perFrameAvgCorrStats, paste(projectFolder, "perFrameAvgCorrStats.csv", sep = "/"))



# Calculate the mean properties of different animals.
meanSizes = read_rds(paste(projectFolder, "Data_Processed/meanSizes.rds", sep = "/"))
meanProfiles = read_rds(paste(projectFolder, "correlationProfiles.rds", sep = "")) %>% mutate(Frame = as.integer(Frame)) %>% unnest() %>% 
  group_by(folder, domain) %>% summarise_each(funs(mean, sd), c(vCorr, dCorr, sCorr)) %>% inner_join(meanSizes)
write_rds(meanProfiles, paste(projectFolder, "meanProfiles.rds", sep = "/"))

meanProfilesStats = meanProfiles %>% select(folder, domain, vCorr = vCorr_mean, dCorr = dCorr_mean, sCorr = sCorr_mean) %>%
  group_by(folder) %>% nest(.key = "correlationFunction") %>% mutate(corrStats = map(correlationFunction, mapCorrStats)) %>%
  select(folder, corrStats) %>% unnest() %>% arrange(folder)
write_csv(meanProfilesStats, paste(projectFolder, "meanProfileStats.csv", sep = "/"))
