# -*- coding: utf-8 -*-
"""
Created on Mon Jun 27 01:02:38 2016

@author: Mircea
"""

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