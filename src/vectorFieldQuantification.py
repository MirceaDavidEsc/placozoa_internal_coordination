# -*- coding: utf-8 -*-
"""
Created on Thu Jun 23 17:10:46 2016

@author: Mircea
"""
# Setup: load required libraries, change folder to a working folder
import numpy as np
from skimage import data, io, filters
import os
import h5py
from skimage import io, measure, morphology

projectFolder = 'C:/Users/Mircea/Projects/Davidescu et al - The effect of size on internal coordination'
dataFolder = 'G:/ALL DATA/HighMagTracked/2015-02-12-run_1'
os.chdir(dataFolder)

def readFrameVectors(frameIndex):
    vectorFieldFile = h5py.File('longformOpticalFlow.hdf5','r')
    
    #Get the vector field indices corresponding to the frame.
    vectorFieldIndices = vectorFieldFile['longformFrameIndices']
    vectorFieldIndices = vectorFieldIndices[:]
    frameIndices = vectorFieldIndices[:,frameIndex-1]
    
    # Load the vector field data.
    vectorFields = vectorFieldFile['longform']
    thisFrameVectorField = vectorFields[0:5,int(frameIndices[1]):int(frameIndices[2])]
    return thisFrameVectorField


def getStationaryVectorsMatrix(vectorField,threshold = 1):
    speeds = np.sqrt(vectorField[3,]**2 + vectorField[4,]**2)
    stationary = speeds < threshold
    stationaryVectors = vectorField[:,stationary]
    return(stationaryVectors)
    

def longformToMatrix(vectorField):
    wholeAnimalArray = np.zeros((256,256))
    xinds = vectorField[0,:]
    xinds = xinds.astype(int)
    yinds = vectorField[1,:] 
    yinds = yinds.astype(int)
    wholeAnimalArray[xinds,yinds] = 1
    wholeAnimalArray.astype(int)
    return wholeAnimalArray

def bwperim(bwimage):
    animalContour = measure.find_contours(bwimage,0.5,'high','high')
    animalContour = np.concatenate(animalContour, axis = 0)
    animalContour = np.round(animalContour,0).astype(int)
    wholeAnimalPerimeter = np.zeros((256,256))
    wholeAnimalPerimeter[animalContour[:,0], animalContour[:,1]] = 1
    io.imshow(wholeAnimalPerimeter)

def bwperim(bwimage):
    eroded = morphology.binary_erosion(bwimage)
    eroded = morphology.binary_erosion(eroded)
    onlyPerimeter = bwimage - eroded
    return onlyPerimeter
    
def convertPxToVectorBoundary(thisFrameBoundary):
    vectorFieldUnits = np.round(thisFrameBoundary[0:2,:]/8,0)
    numVectors = np.shape(thisFrameBoundary)[1]
    toKeep = np.repeat([True],numVectors)
    for i in range(1,numVectors):
        if ((vectorFieldUnits[0,i] == vectorFieldUnits[0,i-1]) and (vectorFieldUnits[1,i] == vectorFieldUnits[1,i-1])):
            toKeep[i] = False
    vectorBoundary = vectorFieldUnits[:,toKeep]
    vectorBoundary.astype(int)
    return vectorBoundary
    

boundaryPaths = h5py.File('boundaryPaths.h5')
boundaryPathsTrace = boundaryPaths['boundaryPaths']
boundaryPathsTrace = boundaryPathsTrace[:]

thisFrame = 2643
thisFrameBoundary = boundaryPathsTrace[:,boundaryPathsTrace[2,:]==thisFrame]

thisVectorBoundary = convertPxToVectorBoundary(thisFrameBoundary)
thisVectorBoundary[0,:] = 255 - thisVectorBoundary[0,:]
thisVectorBoundary = thisVectorBoundary.astype(int)



frameVectors = readFrameVectors(thisFrame)
frameVectorPositions = frameVectors[0:2,:]
frameVectorPositions = frameVectorPositions.astype(int)
frameVectorPositionsTuple = tuple(map(tuple, (frameVectorPositions[:,:], frameVectorPositions[:,:])))
vectorInds = np.ravel_multi_index(frameVectorPositionsTuple, (256,256), order='F')


vectorBoundaryMat = longformToMatrix(thisVectorBoundary)
io.imshow(vectorBoundaryMat)

speeds = np.sqrt(frameVectors[3,:]**2 + frameVectors[4,:]**2)


#animalMask = longformToMatrix(frameVectors)
#animalBoundary = segmentation.find_boundary(animalMask)
#io.imshow(animalMask)
#boundaryInds = np.where(animalBoundary == 1)



# Find the stationary boundary coordinates.
stationarySegments = getStationaryVectorsMatrix(frameVectors,1)
stationaryMatrix = longformToMatrix(stationarySegments)
io.imshow(stationaryMatrix)
stationaryInds = np.where(stationaryMatrix == 1)


linearInds = np.ravel_multi_index(stationaryInds, (256,256), order='F')
linearIndsBound = np.ravel_multi_index(thisVectorBoundary, (256,256), order='F')
intersectInds = list(set(linearInds) & set(linearIndsBound))

tupleIndex = np.unravel_index(intersectInds, (256,256), 'F')

stationaryBoundary = np.zeros((256,256),dtype=int)
stationaryBoundary[tupleIndex[0],tupleIndex[1]] = 1
io.imshow(stationaryBoundary)

movingBoundary = animalBoundary - stationaryBoundary
io.imshow(movingBoundary)

io.imshow(stationaryMatrix)

# Compare position of stationary boundary center, stationary vectors center, and total animal center
# Get vector from animal center to stationary center.
# Get angle between that vector and the vector <1,0>
# Get a time series of those angles.
