# Reduce data to appropriate components
# @AUTHOR: Mircea Davidescu
# @DATE: May 20, 2018
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
## Calculate the collective order for each animal for each frame

# Read the relevant shape data for the sampled frames of each animal.
relevantShapeData = read_rds(paste(projectFolder, processedDataFolder, "validShapes.rds", sep = "/")) %>%
  select(Area, EquivalentDiameter, Eccentricity, FormFactor, AspectRatio, Compactness, Solidity, folder, Frame)

# Make an order measures dataset based on the sampled frames of data for each animal
if (!file.exists(paste(projectFolder, processedDataFolder, "allOrderMeasures.rds", sep = "/"))) {
  orderList = list()
  for (thisFolder in meanSizes$folder) {
    print(thisFolder)
    if (file.exists(paste(thisFolder, processedDataFolder, "OrderMeasures.csv", sep="/")))
      orderList[[length(orderList)+1]] = read_csv(paste(thisFolder, processedDataFolder, "OrderMeasures.csv", sep="/")) %>% mutate(folder = thisFolder)
  }
  allOrderMeasures = bind_rows(orderList)
  write_rds(allOrderMeasures, paste(projectFolder, processedDataFolder, "allOrderMeasures.rds", sep = "/"))
}

## Get the average collective order for each animal

# Order when animal is uncurled, not touching edge.
validShapes = read_rds(paste(projectFolder, processedDataFolder, "validShapes.rds", sep = "/"))
relevantOrderData = read_rds(paste(projectFolder, processedDataFolder, "allOrderMeasures.rds", sep = "/")) %>% mutate(crystalMeasure = (rotation^2 + polarization^2 + dilatation^2)^(1/3)) %>% inner_join(validShapes) %>% filter(Frame != 1)
write_rds(relevantOrderData, paste(projectFolder, processedDataFolder, "/relevantOrderData.rds", sep = ""))

rm(list = "validShapes")

## Dynamics and distribution of collective order

if (!file.exists(paste(projectFolder, processedDataFolder, "exampleOrder.rds", sep = "/"))) {
  exampleFolder = paste(masterDir, "2013-07-01-run_1", sep = "/")
  orderValues = read_csv(paste(exampleFolder, processedDataFolder, "OrderMeasures.csv", sep = "/"))
  write_rds(orderValues, paste(projectFolder, processedDataFolder, "exampleOrder.rds", sep = "/"))
}