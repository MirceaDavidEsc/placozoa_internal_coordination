# -*- coding: utf-8 -*-
"""
Created on Mon Jun 27 00:28:45 2016

@author: Mircea
"""

import imageio
filename = '/tmp/file.mp4'
vid = imageio.get_reader(filename,  'ffmpeg')

