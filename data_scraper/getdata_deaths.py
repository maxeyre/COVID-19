#!/usr/bin/env python

import urllib.request
import xml.etree.ElementTree as ET
from datetime import datetime
import os.path
import json

DATALISTURL="https://publicdashacc.blob.core.windows.net/publicdata?restype=container&comp=list&prefix=data_"
DATAURL="https://c19pub.azureedge.net/" # + data_{date}.json


def get_listing():
    """ get the XML from the data source """
    response = urllib.request.urlopen(DATALISTURL)
    xmldata = response.read()
    text = xmldata.decode("utf-8")
    return text

def get_datafiles(xmltext):
    """ get sorted list of data files from listing XML 
    First one is most recent
    Each element is a dict with Name and Date"""
    root = ET.fromstring(xmltext)
    datablobs = root.find("Blobs").findall("Blob")
    datadata = [
        {"Name": blob.find("Name").text,
         "Date": datetime.strptime(blob.find("Properties").find("Last-Modified").text, "%a, %d %b %Y %H:%M:%S %Z")
         } for blob in datablobs
        ]
    datadata = sorted(datadata, key=lambda k: k['Date'], reverse=True) 
    return datadata

def download_datafiles(datadata, destdir):
    for datafile in datadata:
        src = DATAURL + datafile['Name']
        dst = os.path.join(destdir, datafile['Name'])
        urllib.request.urlretrieve(src, dst)

def json_to_csv(jsonfile):
    with open(jsonfile) as js:
        j = json.load(js)
    print(HEAD)
    write_json_section(j['overview'],"Country - UK")
    write_json_section(j['countries'],"Country")
    return

HEAD='"Area name","Area code","Area type","Reporting date","Daily hospital deaths","Cumulative hospital deaths"'
FORMAT='"%s","%s","%s","%s",%s,%s'

def write_json_section(areadata, Area_Type):
    outputrecords = {}
    codes = areadata.keys()
    for code in codes:
      onearea = areadata[code]
      try:
        name = onearea['name']['value']
        rec={}
        for datevalue in onearea['dailyDeaths']:
            dat = datevalue['date']
            rec[dat]={}
            rec[dat]['date']=dat
            rec[dat]['dailyDeaths'] = datevalue['value']
            rec[dat]['name']=name
            rec[dat]['code']=code
        for datevalue in onearea['dailyTotalDeaths']:
            dat = datevalue['date']
            if dat not in rec.keys():
                rec[dat]={}
            rec[dat]['dailyTotalDeaths'] = datevalue['value']
            rec[dat]['date']=dat
            rec[dat]['name']=name
            rec[dat]['code']=code
        outputrecords[code] = rec
      except:
          pass

    for area,record in outputrecords.items():
        for datekey,daterecord in record.items():
            name = daterecord['name']
            code = daterecord['code']
            date = daterecord['date']
            dcc = daterecord.get('dailyDeaths',"")
            dtcc = daterecord.get('dailyTotalDeaths',"")
            print(FORMAT % ( name,code,Area_Type,
                                 date,
                                 dcc,
                                 dtcc)
            )

    return outputrecords

import sys
if __name__ == "__main__":
    cmd = sys.argv[1]
    if cmd == "json_to_csv":
        json_to_csv(sys.argv[2])
    if cmd == "get_latest_csv":
        pass
    if cmd == "build":
        listing = get_listing()
        datafiles = get_datafiles(listing)
        download_datafiles(datafiles, "./public")
        latest = datafiles[0]
        latestpath = os.path.join("./public",latest['Name'])
        json_to_csv(latestpath)
