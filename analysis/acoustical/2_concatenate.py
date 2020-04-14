# -*- coding: utf-8 -*-
"""
Created on Fri Mar  1 01:58:34 2019
Use this to take a number of smaller subclips split by export_subclips.py and concatenate them together, based on their original filenames

@author: hleerubin

https://stackoverflow.com/questions/54043249/concatenate-several-audios-of-a-folder
"""
import librosa
import numpy as np
import librosa.display
import os
import sys

clips = {}
# Directory of Subclips
subclip_path = './ids_subclips/'
# Directory for reconcatenated clips
export_path = './reconcatenated/'

if len(sys.argv) == 0:
    pass
elif len(sys.argv) == 2:
    subclip_path = sys.argv[0]
    export_path = sys.argv[1]
else:
    raise Exception('Wrong number of arguments: Passing no arguments defaults the... input path to'
                    + subclip_path + 'and the output path to ' + export_path
                    + '. Otherwise pass into arguments for the input path and output path, respectively.')

# Put all of filenames in a dictionary sorted by clip
for filename in os.listdir(subclip_path):
    name, ext = os.path.splitext(filename)
    if ext == '.wav':
        clip_id = name[0:6]
        if clip_id not in clips:
            clips[clip_id] = []
        clips[clip_id].append(name)

# For each clip, concatenate them
for clip_id in clips:
    print('~ Concatenating ' + clip_id + ' ~')
    # Make sure the subclips are ordered by number
    clips[clip_id].sort(key = lambda x: int(x[7:]))
    subclips = np.empty((2, 0))
    for subclip_id in clips[clip_id]:
        print('Concatenating ' + subclip_id + '...')
        aud, sr = librosa.load(subclip_path + subclip_id + '.wav', mono=False, sr=44100)
        if sr != 44100:
            raise Exception(subclip_id + ': does not have samplerate of 44100.')
        subclips = np.concatenate((subclips, aud), axis=1)
    librosa.output.write_wav(export_path + clip_id + '.wav', subclips, sr)