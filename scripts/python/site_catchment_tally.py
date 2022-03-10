#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      Bogdan
#
# Created:     21/09/2016
# Copyright:   (c) Bogdan 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Purpose:
#
# Author:      Bogdan
#
# Created:     26/07/2016
# Copyright:   (c) Bogdan 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, os, sys, string, csv, time
from arcpy import env
from arcpy.sa import *

arcpy.CheckOutExtension('spatial')

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\Network_analysis" # define top-level dir to store results
gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
gdb_s = root + "\\" + "Scratch.gdb"

solution = root + "\\" + "sitesByCatchment.csv"

env.workspace = gdb
env.scratchWorkspace = gdb_s
env.overwriteOutput = True

# FUNCTIONS

# INPUT DATA #
# Required FCs: subcatchments, sites, WWTP/HEPs, river network
sites = gdb + "\\" + "Invertebrates\DistinctSites_09_26_select"

# TEMPORARY DATA #
# Make feature layers
sites = arcpy.MakeFeatureLayer_management(sites, "site_lyr")

# Load list of all distinct site IDs
siteID = []
cursor = arcpy.SearchCursor(sites)
for row in cursor:
    siteID.append(row.getValue("siteId"))
del row, cursor

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # siteId : EZGNR pairs to associate sites with subcatchments

for ID in siteID:
    site_catchment[ID] = []

# Load list of catchment FCs
fcList = arcpy.ListFeatureClasses("*", "", "Catchments")
fieldList =["tezgnr1000", "tezgnr150", "tezgnr40", "EZG_NR"]

# Counters for print output statements
loopCount = 0
write = sys.stdout.write

print "Script setup... done"

for fc, field in zip(fcList, fieldList):
    start = time.time()
    write('fc: ' + str(fc) + ' | ')
    loopCount = loopCount + 1

    sj = env.scratchWorkspace + "\\" + fc

    c = gdb + "\\Catchments\\" + str(fc)
    catchments = arcpy.MakeFeatureLayer_management(c, "c_lyr")

    write('Join -> | ')
    arcpy.SpatialJoin_analysis(sites, catchments, sj, "JOIN_ONE_TO_ONE")

    spatialJoin = arcpy.MakeFeatureLayer_management(sj, "sj_lyr")

    cursor = arcpy.da.SearchCursor(spatialJoin, ("SiteId", field))
    for row in cursor:
        site_catchment[row[0]].append(row[1])
    del cursor

    arcpy.Delete_management(catchments)
    arcpy.Delete_management(sj)

    stop = time.time()
    elapsed = (stop - start)/60
    elapsed = round(elapsed, 1)

    write('Done loop ' + str(loopCount) + ': ' + str(elapsed) + ' minutes\n')

write('Writing results ... \n')
with open(solution, 'wb') as csv_file:
    writer = csv.writer(csv_file)
    for key, value in site_catchment.items():
        writer.writerow([key] + value)

print "SCRIPT COMPLETE"

