__author__ = 'adomas'

import pandas as pd
from ua_parser import user_agent_parser


# reads data and drops NA
def read(inputPath):
    data = pd.read_csv(inputPath, sep='\t', header=None)
    data.columns = ['cookieID', 'segment', 'userAgent']
    data = data[~data.isnull().any(axis=1)]
    return data


# takes segment line and extracts to dictionary
def splitSegment(segmentRow):
    cookieID = segmentRow[0]
    properties = segmentRow[1].split(';')
    propDict = {'age': None, 'children': None, 'education': None, 'employ': None, 'gender': None, 'household': None, 'income': None}
    propDict['age'] = [s for s in properties if "age" in s]
    propDict['children'] = [s for s in properties if "children" in s]
    propDict['education'] = [s for s in properties if "education" in s]
    propDict['employ'] = [s for s in properties if "employ" in s]
    propDict['gender'] = [s for s in properties if "gender" in s]
    propDict['household'] = [s for s in properties if "household" in s]
    propDict['income'] = [s for s in properties if "income" in s]

    for prop in propDict:
        if len(propDict[prop]) == 0:
            propDict[prop] = None
        else:
            propDict[prop] = propDict[prop][0]
    propDict['cookieID'] = cookieID
    return propDict


# makes segment line into data frame with segments
def extractSegment(segment):
    split = [splitSegment(row) for index, row in segment.iterrows()]
    return pd.DataFrame.from_records(split)


# parses ua dict of dict to list
def parseUA(ua_dict):
    ua_parsed = dict()
    ua_parsed['cookieID'] = ua_dict['cookieID']
    ua_parsed['device_family'] = ua_dict['device']['family']
    ua_parsed['ua_patch'] = ua_dict['user_agent']['patch']
    ua_parsed['ua_major'] = ua_dict['user_agent']['major']
    ua_parsed['ua_minor'] = ua_dict['user_agent']['minor']
    ua_parsed['ua_family'] = ua_dict['user_agent']['family']
    ua_parsed['os_patchminor'] = ua_dict['os']['patch_minor']
    ua_parsed['os_patch'] = ua_dict['os']['patch']
    ua_parsed['os_major'] = ua_dict['os']['major']
    ua_parsed['os_minor'] = ua_dict['os']['minor']
    ua_parsed['os_family'] = ua_dict['os']['family']
    return ua_parsed


# parses useragents and puts into df
def extractUA(userAgent):
    userAgents = []
    for index, row in userAgent.iterrows():
        ua = user_agent_parser.Parse(row[1])
        ua['cookieID'] = row[0]
        userAgents.append(ua)
    return pd.DataFrame(list(map(parseUA, userAgents)))


if __name__ == '__main__':
    inputPath = '/home/adomas/Learning/logistic/cookies.csv'
    outputPath = '/home/adomas/Learning/logistic/cookiesParsed.csv'
    data = read(inputPath)
    segment = extractSegment(data[['cookieID', 'segment']])
    userAgent = extractUA(data[['cookieID', 'userAgent']])
    data = segment.merge(userAgent, on='cookieID')
    data.to_csv(outputPath, ';', index=False)