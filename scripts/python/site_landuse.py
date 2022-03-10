#-------------------------------------------------------------------------------
# Purpose:
#
# Author:      Bogdan
#
# Created:     26/07/2016
# Copyright:   (c) Bogdan 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, os, string, csv, sys, time
from arcpy import env

arcpy.CheckOutExtension('spatial')
arcpy.CheckOutExtension('network')

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\InputData1" # define top-level dir to store results
gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
results = root + "\\" + "results.gdb"
scratch = root + "\\" + "scratch.gdb"

env.workspace = gdb
env.scratchWorkspace = scratch
env.overwriteOutput = True
env.parallelProcessingFactor = "100%"

# INPUT DATA #
c = "Catchments\\GAB_EZG_CH"
s = "Sites\inv_sites_CH"
r = "Rivers\\rvr_clean"
g = "TLM_Golf"

# TEMPORARY DATA #
# Make feature layers
arcpy.CopyFeatures_management(c, "in_memory\\catchments")
catchments = arcpy.MakeFeatureLayer_management("in_memory\\catchments", "sc_lyr")

arcpy.CopyFeatures_management(s, "in_memory\\sites")
sites = arcpy.MakeFeatureLayer_management("in_memory\\sites", "site_lyr")

arcpy.CopyFeatures_management(g, "in_memory\\golf")
golf = arcpy.MakeFeatureLayer_management("in_memory\\golf", "g_lyr")

arcpy.CopyFeatures_management(r, "in_memory\\rivers")
rivers = arcpy.MakeFeatureLayer_management("in_memory\\rivers", "r_lyr")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # siteId : EZGNR pairs to associate sites with subcatchments
catch_hier = {} # EZGNR : H1, H2 values to associate subcatchments with catchment hierarchy

resultsDict = {}

# Load list of all distinct site IDs
siteID = []
with arcpy.da.SearchCursor(sites, ("SiteId")) as cursor:
    for row in cursor:
        siteID.append(row[0])
fileName = map(lambda n: n.replace(" ","_").replace(".", "_").replace("-", "_"), siteID)

# Counters for print output statements
loopCounter = 0
siteFail = []
print "Script setup... done"

write = sys.stdout.write

# GEOPROCESSING #
# Match sites to sub-catchments
write('Perform spatial join -> | ')
sj = env.scratchWorkspace + "\\" + "spatialJoin"
arcpy.SpatialJoin_analysis(sites, catchments, sj, "JOIN_ONE_TO_ONE", "KEEP_ALL", match_option="INTERSECT", search_radius="", distance_field_name="")
with arcpy.da.SearchCursor(sj, ("SiteId", "EZG_NR", "h1", "h2")) as cursor:
    for row in cursor:
        site_catchment[row[0]] = row[1]
        catch_hier[row[1]] = (row[2], row[3])
arcpy.Delete_management(sj)
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

    arcpy.SelectLayerByAttribute_management(catchments, "NEW_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < '+ str(h2))
    count = int(arcpy.GetCount_management(catchments).getOutput(0))
    write('NC: ' + str(count) + ' | ')

##    try:
    arcpy.SelectLayerByLocation_management(golf, "INTERSECT", catchments)
    count = int(arcpy.GetCount_management(golf).getOutput(0))

    write('Query -> | ')

    if count > 0:
        gclip = scratch + "\\" + "gc_" + str(FN)
        arcpy.Clip_analysis(golf, catchments, gclip, "")

        # Get golf area in catchment
        intersectArea = 0
        with arcpy.da.SearchCursor(gclip, ("Shape_Area")) as cursor:
            for row in cursor:
                intersectArea = intersectArea + row[0]

        # Get catchment area
        catchmentArea = 0
        with arcpy.da.SearchCursor(catchments, ("A_SUBEZG")) as cursor:
            for row in cursor:
                catchmentArea = catchmentArea + row[0]

        resultsDict[ID] = ((intersectArea/1000000), (catchmentArea/1000000))

        write('Intersection -> |')

        arcpy.Delete_management(gclip)
    arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(sites, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(golf, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(rivers, "CLEAR_SELECTION")

    stop = time.time()
    elapsed = (stop - start)/60
    elapsed = round(elapsed, 1)
    write('| Loop ' + str(loopCounter) + ': ' + str(elapsed) + ' min\n')

##    except:
##        print "Site FAILED: " + str(ID)
##        siteFail.append(str(ID))

solution = root + "\\" + "site_Golf.csv"

with open(solution, 'wb') as csv_file:
    writer = csv.writer(csv_file)
    for key, value in resultsDict.items():
       writer.writerow([key, value[0], value[1]])

print "SCRIPT COMPLETE"
