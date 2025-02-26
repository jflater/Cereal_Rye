# -*- coding: utf-8 -*-
"""
Created on Thu Jul  1 10:39:45 2021

Read in small drainage pit logger csv at SABR

15 plots in total

Registers list units of US gallons, but still have yet to confirm if a pulse is 1 gallon

@author: edmonds5
"""

import pandas as pd
import glob
import os
from datetime import datetime


#define constants

#plot areas will go here, can likely use just 1 for all

plotarea = 120*160*92903. #ft2 to mm2, Kate believes 120ft E-W, 160ft N-S; convert this to metric
pulsefactor = 1. #per online specs, one pulse is 1 gallon
USgaltoL = 3.78541 #1 gal = 3.78541 L
galtomm3 = 3785000. #1 US gal = 3.785e6 mm3

TodaysDate = datetime.today().strftime('%Y-%m-%d')
#csv_out30min = '30minSmallDrainage_'+TodaysDate+'.csv'
#csv_outDaily = 'DailySmallDrainage_'+TodaysDate+'.csv'
csv_5min = 'SABR_5min_SmallDrainage_'+TodaysDate+'.csv'

#data Out
csv_30minL = "SABR_30min_SmallPlotDrainage_L_"+TodaysDate+".csv"
csv_30minmm = "SABR_30min_SmallPlotDrainage_mm_"+TodaysDate+".csv"
csv_dailyL = "SABR_daily_SmallPlotDrainage_L_"+TodaysDate+".csv"
csv_dailymm = "SABR_daily_SmallPlotDrainage_mm_"+TodaysDate+".csv"

#grab working directory
scriptpath = os.getcwd()

#read in raw appended file
SmallDrainAppended = pd.read_csv("SmallDrainage_RawDataAppended_2022-02-07.csv", parse_dates = ['TIMESTAMP'], index_col = ['TIMESTAMP'])

#Scan for list of raw logger CSVs to append
SmallDrainFiles = glob.glob(scriptpath+"/SmallDrainage_ServerDataRaw*.csv")

if(len(SmallDrainFiles) < 1):
    print('No new Small Drainage files to append')

if(len(SmallDrainFiles) >= 1):
    for f in SmallDrainFiles:
        csv = pd.read_csv(f, parse_dates = ['TIMESTAMP'], index_col = ['TIMESTAMP'])
        csv = csv.drop(['RECORD'], axis =1)
        SmallDrainAppended = SmallDrainAppended.append(csv)
        
    SmallDrainAppended = SmallDrainAppended.reset_index()
    SmallDrainAppended.to_csv("SmallDrainage_RawDataAppended_"+TodaysDate+".csv", index = False)
    SmallDrainAppended = SmallDrainAppended.set_index('TIMESTAMP') #because i'm lazy

#make a copy from raw and rename columns to the appropriate plots
dataModified = SmallDrainAppended.copy(deep=True)

dataModified.rename(columns={'SW8ACount_Tot(1)':'Plot_1', 'SW8ACount_Tot(2)':'Plot_6', 'SW8ACount_Tot(3)':'Plot_11',
                             'SW8ACount_Tot(4)':'Plot_2', 'SW8ACount_Tot(5)':'Plot_7', 'SW8ACount_Tot(6)':'Plot_12',
                             'SW8ACount_Tot(7)':'Plot_3', 'SW8ACount_Tot(8)':'Plot_8', 'SW8ACount_Tot(9)':'Plot_13',
                             'SW8ACount_Tot(10)':'Plot_4', 'SW8ACount_Tot(11)':'Plot_9', 'SW8ACount_Tot(12)':'Plot_14',
                             'SW8ACount_Tot(13)':'Plot_5', 'SW8ACount_Tot(14)':'Plot_10', 'SW8ACount_Tot(15)':'Plot_15'}, inplace = True)
#reorder columns sequentially for sanity
dataModified = dataModified[['Plot_1', 'Plot_2', 'Plot_3', 'Plot_4', 'Plot_5', 
                             'Plot_6', 'Plot_7', 'Plot_8', 'Plot_9', 'Plot_10', 
                             'Plot_11', 'Plot_12', 'Plot_13', 'Plot_14', 'Plot_15']]

#1) Check if any records missing
MissingDates = pd.date_range(dataModified.index.min(), dataModified.index.max(),freq = '5Min').difference(dataModified.index)

if MissingDates.size > 0:
    print("!!Dates missing for Small Drainage Plots!!")
    print(MissingDates)

#Come back in and add a case for whatever happens when a date is missing (example code here)
if MissingDates.size > 0:
    dataModified = dataModified.reindex(pd.date_range(dataModified.index.min(), dataModified.index.max(),freq = '5Min'), fill_value = None)

#apply pulsefactor to get correct Gallons
dataModified = dataModified*pulsefactor

#convert one dataframe to L and one to mm3
drainage_mm = dataModified*galtomm3/plotarea
drainage_L = dataModified*USgaltoL

#rename columns with units appropriately
drainage_mm.rename(columns={'Plot_1':'Plot_1 (mm)', 'Plot_2':'Plot_2 (mm)', 'Plot_3':'Plot_3 (mm)', 'Plot_4':'Plot_4 (mm)', 'Plot_5':'Plot_5 (mm)', 
                             'Plot_6':'Plot_6 (mm)', 'Plot_7':'Plot_7 (mm)', 'Plot_8':'Plot_8 (mm)', 'Plot_9':'Plot_9 (mm)', 'Plot_10':'Plot_10 (mm)', 
                             'Plot_11':'Plot_11 (mm)', 'Plot_12':'Plot_12 (mm)', 'Plot_13':'Plot_13 (mm)', 'Plot_14':'Plot_14 (mm)', 'Plot_15':'Plot_15 (mm)'}, inplace = True)

drainage_L.rename(columns={'Plot_1':'Plot_1 (L)', 'Plot_2':'Plot_2 (L)', 'Plot_3':'Plot_3 (L)', 'Plot_4':'Plot_4 (L)', 'Plot_5':'Plot_5 (L)', 
                             'Plot_6':'Plot_6 (L)', 'Plot_7':'Plot_7 (L)', 'Plot_8':'Plot_8 (L)', 'Plot_9':'Plot_9 (L)', 'Plot_10':'Plot_10 (L)', 
                             'Plot_11':'Plot_11 (L)', 'Plot_12':'Plot_12 (L)', 'Plot_13':'Plot_13 (L)', 'Plot_14':'Plot_14 (L)', 'Plot_15':'Plot_15 (L)'}, inplace = True)
#resample data for 30 minute interval
smalldrainage_30min_L = drainage_L.resample('30min', label = 'right').sum(min_count = 1) #right for 30min
smalldrainage_30min_mm = drainage_mm.resample('30min', label = 'right').sum(min_count = 1)

#resample data for daily interaval (this may be redundant if the logger daily count function works, tbd)
smalldrainage_Daily_L = drainage_L.resample('D',label = 'left').sum(min_count = 1) #left for daily
smalldrainage_Daily_mm = drainage_mm.resample('D',label = 'left').sum(min_count = 1) #left for daily

#bring date column back in and replace NAs with -99
smalldrainage_30min_L.reset_index(inplace = True); smalldrainage_30min_L.rename(columns={'index':'TIMESTAMP'}, inplace = True)
smalldrainage_30min_mm.reset_index(inplace = True); smalldrainage_30min_mm.rename(columns={'index':'TIMESTAMP'}, inplace = True)
smalldrainage_Daily_L.reset_index(inplace = True); smalldrainage_Daily_L.rename(columns={'index':'TIMESTAMP'}, inplace = True)
smalldrainage_Daily_mm.reset_index(inplace = True); smalldrainage_Daily_mm.rename(columns={'index':'TIMESTAMP'}, inplace = True)
#5minute data
drainage_mm.reset_index(inplace = True); drainage_mm.rename(columns={'index':'TIMESTAMP'}, inplace = True)

smalldrainage_30min_L.fillna(-99, inplace = True)
smalldrainage_30min_mm.fillna(-99, inplace = True)
smalldrainage_Daily_L.fillna(-99, inplace = True)
smalldrainage_Daily_mm.fillna(-99, inplace = True)
#5minute data
drainage_mm.fillna(-99,inplace = True)

#output files

smalldrainage_30min_L.to_csv("../DerivedProcessed/"+csv_30minL, index = False)
smalldrainage_30min_mm.to_csv("../DerivedProcessed/"+csv_30minmm, index = False)
smalldrainage_Daily_L.to_csv("../DerivedProcessed/"+csv_dailyL, index = False)
smalldrainage_Daily_mm.to_csv("../DerivedProcessed/"+csv_dailymm, index = False)
drainage_mm.to_csv("../DerivedProcessed/"+csv_5min, index = False)



