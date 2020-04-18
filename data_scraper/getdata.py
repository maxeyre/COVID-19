#!/usr/bin/env python

import urllib.request
import xml.etree.ElementTree as ET
from datetime import datetime
import os.path
import json

DATALISTURL="https://publicdashacc.blob.core.windows.net/publicdata?restype=container&comp=list&prefix=data_"
DATAURL="https://c19pub.azureedge.net/" # + data_{date}.json


def get_listing():
    response = urllib.request.urlopen(DATALISTURL)
    xmldata = response.read()
    text = xmldata.decode("utf-8")
    return text

def get_datafiles(xmltext):
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
    write_json_section(j['countries'],"Country")
    write_json_section(j['regions'],"Region")
    write_json_section(j['utlas'],"Upper tier local authority")
    return

HEAD='"Area name","Area code","Area type","Specimen date","Daily lab-confirmed cases","Cumulative lab-confirmed cases"'
FORMAT='"%s","%s","%s","%s",%s,%s'

def write_json_section(areadata, Area_Type):
    outputrecords = {}
    codes = areadata.keys()
    for code in codes:
      onearea = areadata[code]
      try:
        name = onearea['name']['value']
        rec={}
        for datevalue in onearea['dailyConfirmedCases']:
            dat = datevalue['date']
            rec[dat]={}
            rec[dat]['date']=dat
            rec[dat]['dailyConfirmedCases'] = datevalue['value']
            rec[dat]['name']=name
            rec[dat]['code']=code
        for datevalue in onearea['dailyTotalConfirmedCases']:
            dat = datevalue['date']
            if dat not in rec.keys():
                rec[dat]={}
            rec[dat]['dailyTotalConfirmedCases'] = datevalue['value']
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
            dcc = daterecord.get('dailyConfirmedCases',"")
            dtcc = daterecord.get('dailyTotalConfirmedCases',"")
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
    
