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

from arcpy.sa import *

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\InitialAnalysis" # define top-level dir
gdb = root + "\\" + "EcologicalData.gdb" # input data
results = root + "\\" + "resultsFA.gdb" # output results
scratch = root + "\\" + "scratch.gdb" # temporary data

env.workspace = gdb
env.scratchWorkspace = scratch
env.overwriteOutput = True
env.parallelProcessingFactor = "100%"

# INPUT DATA #
c = gdb + "\\" + "Catchments\GAB_EZG_CH"
s = gdb + "\\" + "Sites\inv_sites_CH"
fd = gdb + "\\" + "alti10_fd"

# FD raster required:
# - Aggregation to 10m by minimum cell value
# - Burn-in of streams (F2R@10m resolution, set Null to 0 and subtract from DEM)
# - Fill to create depressionless DEM

# TEMPORARY DATA #
# Make feature layers
arcpy.CopyFeatures_management(c, "in_memory\\catchments")
catchments = arcpy.MakeFeatureLayer_management("in_memory\\catchments", "c_lyr")

arcpy.CopyFeatures_management(s, "in_memory\\sites")
sites = arcpy.MakeFeatureLayer_management("in_memory\\sites", "site_lyr")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # SiteId : EZGNR pairs to associate sites with subcatchments
catch_hier = {} # EZGNR : H1, H2 values to associate subcatchments with catchment hierarchy
site_val = {} # SiteId : flow distance to outlet

# Load list of all distinct site IDs
siteID = []
with arcpy.da.SearchCursor(sites, ["SiteId"]) as cursor:
    for row in cursor:
        siteID.append(row[0])

# Counters for print output statements
siteCounter = 0
siteFail = []
print "Script setup... done"

write = sys.stdout.write

# GEOPROCESSING #
# Match sites to sub-catchments
write('Perform spatial join -> | ')
sj = env.scratchWorkspace + "\\" + "spatialJoin"
arcpy.SpatialJoin_analysis(sites, catchments, sj, "JOIN_ONE_TO_ONE", "KEEP_ALL", match_option="INTERSECT", search_radius="", distance_field_name="")
with arcpy.da.SearchCursor(sj, ["SiteId", "EZG_NR", "h1", "h2"]) as cursor:
    for row in cursor:
        site_catchment[row[0]] = row[1]
        catch_hier[row[1]] = (row[2], row[3])
arcpy.Delete_management(sj)
write('Join complete \n')

# Get list of sites that are not in the already completed solutions
env.workspace = results
solutions = arcpy.ListRasters("", "")
env.workspace = gdb
solutions = map(lambda n: n.replace("fd_", ""), solutions)
inc = list(set(siteID) - set(solutions))
fileName = map(lambda n: n.replace(" ","_").replace(".", "_").replace("-", "_"), inc)
write('Remaining sites: ' + str(len(inc)) + '\n')

for ID in inc:
    # Set up loop variables
    start = time.time()
    write('Site: ' + str(ID) + ' | ')
    i = inc.index(str(ID))
    FN = fileName[i]
    siteCounter = siteCounter + 1

    env.extent = c

    table = scratch + "\\" + "site_" + str(FN)
    tfc = scratch + "\\" + "c_" + str(FN)
    fSave = results + "\\" + "FA_" + str(FN)

    # Query site and sub-catchments
    h1 = catch_hier[site_catchment[ID]][0]
    h2 = catch_hier[site_catchment[ID]][1]

    arcpy.SelectLayerByAttribute_management(sites, "NEW_SELECTION", "\"SiteId\" = \'" + str(ID) + "\'")
    arcpy.SelectLayerByAttribute_management(catchments, "NEW_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < ' + str(h2))
    count = int(arcpy.GetCount_management(catchments).getOutput(0))

    write('Query: ' + str(count) + ' -> | ')

##        try:
    # Compute flow accumulation
    arcpy.Dissolve_management(catchments, tfc, "", "", "MULTI_PART", "")
    env.extent = tfc

    FL = FlowAccumulation(fd, "", "INTEGER")
##    env.snapRaster = FL
    write('Flow Accumulation -> | ')

    FL.save(fSave)
    write('Save || ')

    # Clean up and print loop time
    arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(sites, "CLEAR_SELECTION")

    arcpy.Delete_management(tfc)
##    arcpy.ClearEnvironment("snapRaster")

    stop = time.time()
    elapsed = (stop - start)/60
    elapsed = round(elapsed, 1)
    write('Loop ' + str(siteCounter) + ': ' + str(elapsed) + ' minutes\n')

##    except Exception:
##                    print "Site FAILED: " + str(ID)
##                    siteFail.append(str(ID))
##                    e = sys.exc_info()[1]
##                    print(e.args[0] + "\n")
##                    arcpy.AddError(e.args[0])

##else:
##    stop = time.time()
##    elapsed = (stop - start)/60
##    elapsed = round(elapsed, 1)
##    write(' TRUE | Loop ' + str(siteCounter) + ': ' + str(elapsed) + ' minutes\n')

print "SCRIPT COMPLETE"