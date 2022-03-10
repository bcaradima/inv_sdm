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
root = r"D:\Workspace\InitialAnalysis" # define top-level dir to store results
gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
scratch = root + "\\" + "scratch.gdb"

env.workspace = gdb
env.scratchWorkspace = scratch
env.overwriteOutput = True
env.parallelProcessingFactor = "100%"

# INPUT DATA #
c = "Catchments\\GAB_EZG_CH"
s = "Sites\\inv_sites_CH"
r = "Rivers\\rvr_clean"
f = "TLM_Wald"

# TEMPORARY DATA #
# Make feature layers
arcpy.CopyFeatures_management(c, "in_memory\\catchments")
catchments = arcpy.MakeFeatureLayer_management("in_memory\\catchments", "catch_lyr")

arcpy.CopyFeatures_management(s, "in_memory\\sites")
sites = arcpy.MakeFeatureLayer_management("in_memory\\sites", "site_lyr")

arcpy.CopyFeatures_management(f, "in_memory\\forest")
forest = arcpy.MakeFeatureLayer_management("in_memory\\forest", "forest_lyr")

arcpy.CopyFeatures_management(r, "in_memory\\rivers")
rivers = arcpy.MakeFeatureLayer_management("in_memory\\rivers", "river_lyr")

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
write('Join complete \n')

for ID in siteID:
    start = time.time()
    write('Site: ' + str(ID) + ' | ')
    loopCounter = loopCounter + 1

    i = siteID.index(str(ID))
    FN = fileName[i]

    # Select sub-catchments upstream of a sample
    h1 = catch_hier[site_catchment[ID]][0]
    h2 = catch_hier[site_catchment[ID]][1]

    arcpy.SelectLayerByAttribute_management(catchments, "NEW_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < '+ str(h2))
    count = int(arcpy.GetCount_management(catchments).getOutput(0))

    try:
        arcpy.SelectLayerByLocation_management(forest, "INTERSECT", catchments)
        arcpy.SelectLayerByLocation_management(rivers, "INTERSECT", catchments)
        write('Query ' + str(count) + ' -> | ')

        cr = env.scratchWorkspace + "\\" + "catch_rivers"
        arcpy.Clip_analysis(rivers, catchments, cr, "")
        write('Clip -> | ')

        # Calculate river length intersecting forest areas in catchment
        table = scratch + "\\" + "site_" + str(FN)
        arcpy.TabulateIntersection_analysis(forest, "Forest", cr, table, "OBJECTID", "", "", "KILOMETERS")
        tv = arcpy.MakeTableView_management(table, "tv_view")

        intersectLength = 0
        with arcpy.da.SearchCursor(tv, ("LENGTH")) as cursor:
            for row in cursor:
                intersectLength = intersectLength + row[0]

        # Calculate total river length in catchment
        totalLength = 0
        with arcpy.da.SearchCursor(cr, ("Shape_Length")) as cursor:
            for row in cursor:
                totalLength = totalLength + row[0]

        resultsDict[ID] = (intersectLength, (totalLength/1000))
        write('Intersection -> | ')

        arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")
        arcpy.SelectLayerByAttribute_management(rivers, "CLEAR_SELECTION")
        arcpy.SelectLayerByAttribute_management(forest, "CLEAR_SELECTION")

        arcpy.Delete_management(table)
        arcpy.Delete_management(tv)
        arcpy.Delete_management(cr)

        stop = time.time()
        elapsed = (stop - start)/60
        elapsed = round(elapsed, 1)
        write('Loop ' + str(loopCounter) + ': ' + str(elapsed) + ' min\n')

    except Exception:
        print "Site FAILED: " + str(ID) + ' | '
        siteFail.append(str(ID))

        e = sys.exc_info()[1]
        print(e.args[0] + "\n")
        arcpy.AddError(e.args[0])


solution = root + "\\" + "site_inv_FRI.csv"

with open(solution, 'wb') as csv_file:
    w = csv.writer(csv_file)
    fnames = ["ID", "ForestLength", "RiverLength"]
    w.writerow(fnames)
    for k, v in resultsDict.items():
       w.writerow([k, v[0], v[1]])

print "SCRIPT COMPLETE"