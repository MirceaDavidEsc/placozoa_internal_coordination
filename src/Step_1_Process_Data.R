# Reduce data to appropriate components
# @AUTHOR: Mircea Davidescu
# @
# INPUT: Only the parent directory of the Placozoa data.
# OUTPUT:
#   + readyDatasets.csv, a file that contains the list of all folders of Placozoa data.
#   + allShapeData.rds: a file that contains all the shape data (area, perimeter, circularity, etc.) for every frame for each animal.
#   + validShapes.rds: a file that contains the shape of frames that can be analyzed by optic flow (animals not curled up)
#   + sampledFrames.csv: a list of 100 frames per animal that will be sampled for velocity fluctuation and correlation
#   + exampleField.rds: a file that contains an example animal velocity field, to be used for visualizing the velocity fluctuations.
#   + animalVectorsReal.rds: the vector fields from sampledFrames.csv, but storing all data in RDS format, converted to real units.
#   + ahullVectors.rds: the real animals vectors but removing the boundary (using the alpha hull ball). These otherwise would cause calculation anomalies.

# Data Processing 
# Get list of all datasets to us. Determine what datasets are available for the analysis on the hard drive.
# Get the shape data for all the frames of the animal recordings.
# Use the shape data to identify which frames are valid for optical flow (based on animal being curled up).
# Sample 100 valid frames per animal.
# Save off one velocity field as the "example" field to show in the paper.
# Save all sampled animal vector fields in a real-unit converted format.
# Save all sampled animal vector fields in real units after applying an alpha-hull procedure to remove the boundary.


if (!file.exists(paste(projectFolder, processedDataFolder, "readyDatasets.csv", sep = "/"))) {
originalVideos = list.files(masterDir, "^[hamm-]*movie.avi", recursive = T, full.names = T)
readyDatasets = data.frame(folder = dirname(originalVideos), stringsAsFactors = F) %>%
    mutate(hasLongform = file.exists(paste(folder, "longformOpticalFlow.hdf5", sep = "/")), hasShapeProps = file.exists(paste(folder, "shapePropertiesReal.csv", sep = "/"))) %>% filter(hasLongform & hasShapeProps)  
write_csv(readyDatasets, paste(projectFolder, processedDataFolder, "readyDatasets.csv", sep = "/"))
}

# Get the shape data of all the animals
readyDatasets = read_csv(paste(projectFolder, processedDataFolder, "readyDatasets.csv", sep = "/"))
if (!file.exists(paste(projectFolder, processedDataFolder, "allShapeData.rds", sep = "/"))) {
  allShapeData = readyDatasets %>% select(folder) %>% mutate(folder = basename(folder)) %>% filter(!(folder %in% weirdFolders)) %>% mutate(shapeDF = map(paste(masterDir,folder,"shapePropertiesReal.csv",sep="/"), read_csv)) %>% unnest()
  validShapes = allShapeData %>% group_by(folder) %>% filter(touchingEdge == 0) %>% mutate(maxArea = max(Area)) %>% 
    filter(Area > 0.75*maxArea) %>% select(-maxArea) %>% ungroup()
  write_rds(allShapeData, paste(projectFolder, "allShapeData.rds", sep = "/"))
  write_rds(validShapes, paste(projectFolder, "validShapes.rds", sep = "/"))
  rm(list = c("validShapes", "allShapeData"))
}
validShapes = read_rds(paste(projectFolder, processedDataFolder, "validShapes.rds", sep = "/"))
meanSizes = validShapes %>% group_by(folder) %>% summarise_each(funs(mean), EquivalentDiameter:Solidity)
write_csv(meanSizes, paste(projectFolder, processedDataFolder, "meanSizes.csv", sep = "/"))
rm(list = c("validShapes"))


# Producing velocity field samples for analysis
## Sample frames for each animal
# For all datasets, get a list of possible frames to analyze.
if (!file.exists(paste(projectFolder, originalDataFolder, "sampledFrames.csv", sep = "/"))) {
  noInterpolation = read_rds(paste(projectFolder, "validShapes.rds", sep = "/")) %>% filter(!Interpolated)
  #allTrajectory = read_rds("allShapeData.rds") %>% select(folder, Frame, CentroidX, CentroidY)
  
  
  framesList = list()
  for (currFolder in unique(shapeProperties$folder)) {
    # Remove frames where the animal is partially outside of the field of view or the stage is moving.
    noInterpolation = filter(shapeProperties, folder == currFolder) %>% filter(!Interpolated & touchingEdge == 0)
    
    # Read the trajectory, measure its tortuisuty
    #trajectory = filter(allTrajectory, folder == currFolder)
    #trajectory$tortuosity = select(trajectory, -Frame, -folder) %>% measureTortuosity(30)
    #noTurning = filter(trajectory, tortuosity > 0.75)
    
    validFrames = noInterpolation %>% filter(Frame != 1) %>% select(Frame) %>% mutate(folder = currFolder)
    
    # Sample only 100 frames of the entire possible dataset.
    if (dim(validFrames)[1] > 100) {
      framesList[[length(framesList)+1]] = sample_n(validFrames,100)
    } else {
      framesList[[length(framesList)+1]] = validFrames
    }
  }
  framesDF = bind_rows(framesList)
  write_csv(framesDF, paste(projectFolder, originalDataFolder, "sampledFrames.csv", sep = "/"))
}


invalidSampleFrames = tribble(
  ~folder, ~Frame,
  "2014-10-09a-run_1", 39416,
  "2015-02-24-run_2", 5629
)


## Get the sampled velocity fields, save in one rds
if (!file.exists(paste(projectFolder, processedDataFolder, "animalVectors.rds", sep = "/"))) {
  framesDF = read_csv(paste(projectFolder, "sampledFrames.csv", sep = "/"), col_types = cols(Frame = col_number(), folder = col_character()))
  velocityFieldsList = list()
  uniqueAnimals = unique(framesDF$folder)
  for (thisAnimal in uniqueAnimals) {
    framesToRead = filter(framesDF, folder == thisAnimal)
    thisVectors = readDataAtFrame(paste(thisAnimal, "longformOpticalFlow.hdf5", sep = "/"), "longform", "longformFrameIndices", framesToRead$Frame) %>%
      standardizePlacozoaVectors() %>% mutate(folder = basename(thisAnimal))
    velocityFieldsList[[length(velocityFieldsList) + 1]] = thisVectors
  }
  allVelocitySamples = bind_rows(velocityFieldsList) %>% mutate(vectorID = 1:n())
  write_rds(allVelocitySamples, paste(projectFolder, processedDataFolder, "animalVectors.rds", sep = "/"))
  rm(list = c("allVelocitySamples"))
}

## Testing code below: comment out later
if (!file.exists(paste(projectFolder, processedDataFolder, "exampleField.rds", sep = "/"))) {
  exampleField = read_rds(paste(projectFolder, processedDataFolder,  "animalVectors.rds", sep = "/")) %>% group_by(folder, Frame) %>% nest() %>% slice(210) %>% unnest() %>% select(X, Y, vX, vY)
  write_rds(exampleField, paste(projectFolder, processedDataFolder, "exampleField.rds", sep = "/"))
}
exampleField = read_rds(paste(projectFolder, processedDataFolder, "exampleField.rds", sep = "/"))

# oFPlot  = quiverPlot(exampleField, 1, T) + ggtitle("Full Velocities")
# fluctField = calculateFluctuationField(exampleField)
# collectiveField = calculateCollectiveField(exampleField)
# fFPlot = quiverPlot(fluctField, 1, T) + ggtitle("Fluctuation Component")
# cFPlot = quiverPlot(collectiveField, 1, T) + ggtitle("Collective Component")
# (decomposeVectorsPlot = plot_grid(oFPlot, cFPlot, fFPlot, ncol = 3))
# save_plot(paste(projectFolder, "decomposedVectors.pdf", sep = "/"), decomposeVectorsPlot, base_width = 21, base_height = 6)


# Convert animal vectors to real units.
videoTypes = read_csv(paste(projectFolder, originalDataFolder, "videoTypes.csv", sep = "/")) %>% mutate(folder = basename(folder))
if (!file.exists(paste(projectFolder, processedDataFolder, "animalVectorsReal.rds", sep = "/"))) {
  animalVectorsReal = read_rds(paste(projectFolder, "animalVectors.rds", sep = "/")) %>% inner_join(videoTypes) %>% 
    mutate(convertReal = ifelse(hammVid, pxPerMicrometerHamm, pxPerMicrometer)) %>% mutate(X = X * 8/convertReal, Y = Y * 8/convertReal, vX = vX / convertReal, vY = vY / convertReal) %>%
    select(folder, Frame, X:vY)
  write_rds(x = animalVectorsReal, path = paste(projectFolder, processedDataFolder, "animalVectorsReal.rds", sep = ""))  
}


# Remove the boundary vectors, which will be anomalous and have all sorts of edge effects
if (!file.exists(paste(projectFolder, processedDataFolder, "ahullVectors.rds", sep = "/"))) {
  ahullApplied = read_rds(paste(projectFolder, processedDataFolder, "animalVectorsReal.rds", sep = "")) %>%
    group_by(folder, Frame) %>% nest(.key = "vectorField") %>% partition(folder) %>% cluster_library("purrr") %>% cluster_library("dplyr") %>% 
    cluster_library("alphahull") %>% cluster_copy(removeBoundary) %>% cluster_copy(identifyBoundary) %>%
    mutate(ahullVectors = map(vectorField, removeBoundary)) %>% collect() %>% select(folder, Frame, ahullVectors) %>% unnest() %>% ungroup()
  
  write_rds(ahullApplied, paste(projectFolder, processedDataFolder, "ahullVectors.rds", sep = "/"))
  rm(list = c("ahullApplied"))
  gc()
}

# Calculate the fluctuations for each vector field.
if (!file.exists(paste(projectFolder, processedDataFolder, "fluctuationVectors.rds", sep = "/"))) {
  # Get the fluctuation vector fields.
  ahullApplied = read_rds(paste(projectFolder, "ahullVectors.rds", sep = "/"))
  fluctuationVectors = ahullApplied %>% group_by(folder, Frame) %>% nest(.key = "ahullVectors") %>% partition(folder) %>% cluster_library("purrr") %>%
    cluster_library("dplyr") %>% cluster_library("collective") %>% cluster_library("scalefree") %>% 
    mutate(fluctField = map(ahullVectors, calculateFluctuationField)) %>% collect() %>% unnest() %>% ungroup()
  
  # Get the collective component as well, remove redundant columns.
  colnames(fluctuationVectors) = c("folder", "Frame", "X", "Y", "vX", "vY", "fX", "fY", "uX", "uY")
  fluctuationVectors = fluctuationVectors %>% select(-fX, -fY) %>% mutate(wX = vX - uX, wY = vY - uY)
  
  write_rds(fluctuationVectors, paste(projectFolder, processedDataFolder, "fluctuationVectors.rds", sep = "/"))
  rm(list = c("fluctuationVectors", "ahullApplied"))
  gc()
}

