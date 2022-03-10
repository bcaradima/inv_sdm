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

arcpy.CheckOutExtension('spatial')
arcpy.CheckOutExtension('network')

from arcpy import env
from arcpy.sa import *

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\InputData1" # define top-level dir to store results
gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
results = root + "\\" + "Results.gdb"
scratch_gdb = root + "\\" + "Scratch.gdb"
scratch_dir = root + "\\" + "scratch"

env.workspace = gdb
env.scratchWorkspace = scratch_gdb
env.overwriteOutput = True

# FUNCTIONS

# INPUT DATA #
# Required FCs: subcatchments, sites, WWTP/HEPs, river network
sc = gdb + "\\" + "Catchments\GAB_EZG_CH"
s = gdb + "\\" + "Sites\DistinctSites_09_26_select"

##dem = r"D:\Workspace\DEM_analysis\swissALTI3D.gdb\swissALTI3D_fill"
dem = root + "\\" + "DEM25.gdb\dem25_fill"
##fd = r"D:\Workspace\DEM_analysis\swissALTI3D_FD"

# TEMPORARY DATA #
# Make feature layers
catchments = arcpy.MakeFeatureLayer_management(sc, "sc_lyr")
sites = arcpy.MakeFeatureLayer_management(s, "site_lyr")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # siteId : EZGNR pairs to associate sites with subcatchments
catch_hier = {} # EZGNR : H1, H2 values to associate subcatchments with catchment hierarchy
site_elev = {}

# Load list of all distinct site IDs
siteID = []

cursor = arcpy.SearchCursor(sites)
for row in cursor:
    siteID.append(row.getValue("siteId"))
del row, cursor

fileName = map(lambda n: n.replace(" ","_").replace(".", "_").replace("-", "_"), siteID)

# Counters for print output statements
loopCounter = 0
siteFail = []
write = sys.stdout.write

print "Script setup... done"

# GEOPROCESSING #
# Match sites to sub-catchments
write('Perform spatial join -> | ')
sj = env.scratchWorkspace + "\\" + "spatialJoin"
arcpy.SpatialJoin_analysis(sites, catchments, sj, "JOIN_ONE_TO_ONE", "KEEP_ALL", match_option="INTERSECT", search_radius="", distance_field_name="")
spatialJoin = arcpy.MakeFeatureLayer_management(sj, "sj_lyr")

cursor = arcpy.da.SearchCursor(spatialJoin, ("SiteId", "EZG_NR", "h1", "h2"))
for row in cursor:
    site_catchment[row[0]] = row[1]
    catch_hier[row[1]] = (row[2], row[3])
del cursor

arcpy.Delete_management(sj)
arcpy.Delete_management(spatialJoin)
write('Join complete \n')

for ID in siteID:
    start = time.time()
    write('Site: ' + str(ID) + ' | ')
    loopCounter = loopCounter + 1

    i = siteID.index(str(ID))
    FN = fileName[i]

    # Select sub-catchments upstream of a sample
    arcpy.SelectLayerByAttribute_management(sites, "NEW_SELECTION", "\"SiteId\" = \'" + str(ID) + "\'")

    h1 = catch_hier[site_catchment[ID]][0]
    h2 = catch_hier[site_catchment[ID]][1]

    arcpy.SelectLayerByAttribute_management(catchments, "ADD_TO_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < '+ str(h2))
    write('Query -> | ')

##    try:
    # Catchment AREA
    # Run a cursor through the sub-catchments, sum the areas

    catchmentArea = 0
    cursor = arcpy.da.SearchCursor(catchments, ["Shape_Area"])
    for row in cursor:
        catchmentArea = catchmentArea + row[0]
    del cursor

    write('Area -> | ')

    # Catchment ELEVATION
    # Extract the DEM by dissolved feature, get the mean value of the extracted DEM
    # Prepare a mask for extraction
    tempFC = env.scratchWorkspace + "\\" + "site_" + str(FN)
    arcpy.Dissolve_management(catchments, tempFC, "", "", "SINGLE_PART", "")
    mask = arcpy.MakeFeatureLayer_management(tempFC, "temp_lyr")

##    outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster,
##                                 outTable, "NODATA", "MEAN")
    arcpy.env.cellSize = 25

    extractRaster = env.scratchWorkspace + "\\" + "extract_" + str(FN)
    arcpy.gp.ExtractByMask_sa(dem, mask, extractRaster)

    meanElev = arcpy.GetRasterProperties_management(extractRaster, 'MEAN')
    meanElev = meanElev.getOutput(0)
    site_elev[ID] = meanElev, catchmentArea

    write('Elevation -> | ')

    # Clear selections, print times, and delete garbage
    arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(sites, "CLEAR_SELECTION")

    stop = time.time()
    elapsed = (stop - start)/60
    elapsed = round(elapsed, 1)
    write('|| Loop ' + str(loopCounter) + ': ' + str(elapsed) + ' minutes\n')

    arcpy.Delete_management(tempFC)
    arcpy.Delete_management(mask)
    arcpy.Delete_management(extractRaster)
    del catchmentArea
##    except:
##        siteFail.append(str(ID))
##        print "FAILED site calculation: " + str(ID)

solution = root + "\\" + "site_Elevations.csv"

with open(solution, 'wb') as csv_file:
    writer = csv.writer(csv_file)
    for key, value in site_elev.items():
       writer.writerow([key, value[0], value[1]])

print "SCRIPT COMPLETE"