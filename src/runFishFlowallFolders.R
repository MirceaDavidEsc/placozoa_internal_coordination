# This script creates a list of all subdirectories of the dataSourceFolder directory. It copies the videos from the external hard drive to the local disk. Then it runs fishFlow on all the movies. The result is that all movies will have original-tracks.hdf5 made.

dataSourceFolder = "/Volumes/Untitled/ForFishFlow/"
dataProcessFolder = "/Users/mircea/Desktop/ForFishFlow/"
dir.create(dataProcessFolder)

setwd(dataSourceFolder)
allFilesToProcess = list.files(pattern="movie.avi",recursive=T,full.names=T,all.files=T)

for (movieFile in allFilesToProcess) {
  folderName = basename(dirname(movieFile))
  destinationFolder = paste(dataProcessFolder,folderName,sep="/")
  dir.create(destinationFolder)
  print("Copying file")
  file.copy(from=movieFile,to=paste(destinationFolder,"movie.avi",sep="/"),overwrite=F)
  setwd(destinationFolder)
  if (!file.exists("original-tracks.hdf5")) {
  	system("fishFlow -i movie.avi --background=../background.tiff -d original-tracks.hdf5 --grid.width=174 --grid.height=130")
  }
  setwd(dataSourceFolder)
}