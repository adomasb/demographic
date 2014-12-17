__author__ = 'adomas'

import pandas as pd
import os
from urllib.parse import urlparse


# read data
def read(inputPath):
    data = pd.read_csv(inputPath, sep='\t', header=None)
    data.columns = ['cookieID', 'timestamp', 'URL']
    data = data[~data.isnull().any(axis=1)]
    return data

# parses url column
def parseURL(data):
    data['URL'] = data.apply(lambda x: urlparse(x['URL'])[1], axis=1)
    return data

# reads history files in path in format 'cookie_history_x.csv' and parses depending on x to train or test sets
if __name__ == '__main__':
    path = '/home/adomas/Learning/logistic/'
    dirFiles = os.listdir(path)
    files = []
    [files.append(f) for f in dirFiles if 'cookie_history' in f]
    files.sort()
    for f in files:
        inputPath = path+f
        if files.index(f) % 2 == 0:
            outputPath = path + f.split('.')[0] + '_parsed_train.csv'
        else:
            outputPath = path + f.split('.')[0] + '_parsed_test.csv'
        data = read(inputPath)
        data = parseURL(data)
        data.to_csv(outputPath, ';', index=False)
