#-------------------------------------------------------------------------------
# Purpose:
#
# Author:      Bogdan
#
# Created:     26/07/2016
# Copyright:   (c) Bogdan 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------
# Import Python packages
import arcpy, os, string, csv, sys, time

import numpy as np

arcpy.CheckOutExtension('spatial')
arcpy.CheckOutExtension('network')

from arcpy import env
from arcpy.sa import *

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\InitialAnalysis" # define top-level dir
gdb = root + "\\" + "EcologicalData.gdb"
distances =  root + "\\" + "resultsFLO.gdb" # raster fGDB
flow_accumulation = root + "\\" + "resultsFA.gdb"

env.workspace = gdb
env.scratchWorkspace = root + "\\" + "scratch.gdb"
env.overwriteOutput = True
env.parallelProcessingFactor = "100%"

# INPUT DATA #
c = "Catchments\GAB_EZG_CH"
s = "Sites\inv_sites_CH"

land_use = "forest"
raster_lu = arcpy.Raster(land_use)

power = [-1.0]

# FD raster required:
# - Aggregation to 10m by minimum cell value
# - Burn-in of streams (F2R@10m resolution, set Null to 0 and subtract from DEM)
# - Fill to create depressionless DEM

# TEMPORARY DATA #
# Make feature layers
arcpy.CopyFeatures_management(c, "in_memory\\catchments")
catchments = arcpy.MakeFeatureLayer_management("in_memory\\catchments", "sc_lyr")

arcpy.CopyFeatures_management(s, "in_memory\\sites")
sites = arcpy.MakeFeatureLayer_management("in_memory\\sites", "site_lyr")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # SiteId : EZGNR pairs to associate sites with subcatchments
catch_hier = {} # EZGNR : H1, H2 values to associate subcatchments with catchment hierarchy
site_val = {} # SiteId : flow distance to outlet
d = {} # SiteID : sum of target land use distances, sum of all distances

# Load list of all distinct site IDs
siteID = []
with arcpy.da.SearchCursor(sites, ("SiteId")) as cursor:
    for row in cursor:
        siteID.append(row[0])
    del row, cursor

fileName = map(lambda n: n.replace(" ","_").replace(".", "_").replace("-", "_"), siteID)

# Counters for print output statements
siteCounter = 0
print "Script setup... done"
write = sys.stdout.write

# GEOPROCESSING #
# Match sites to sub-catchments
write('Perform spatial join -> | ')
sj = env.workspace + "\\" + "spatialJoin"
arcpy.SpatialJoin_analysis(sites, catchments, sj, "JOIN_ONE_TO_ONE", "KEEP_ALL", match_option="INTERSECT", search_radius="", distance_field_name="")
with arcpy.da.SearchCursor(sj, ("SiteId", "EZG_NR", "h1", "h2")) as cursor:
    for row in cursor:
        site_catchment[row[0]] = row[1]
        catch_hier[row[1]] = (row[2], row[3])
arcpy.Delete_management(sj)
write('Join complete \n')

for p in power:
    for ID in siteID:
        # Set up loop variables
        start = time.time()
        siteCounter = siteCounter + 1
        write('Site: ' + str(ID) + ' | ')
        i = siteID.index(str(ID))
        FN = fileName[i]
        d.setdefault(ID, [0, 0])

        rdist = distances + "\\" + "fd_" + str(FN)
        fa = flow_accumulation + "\\" + "FA_" + str(FN)
        sc = "catchment_dissolve"

        if arcpy.Exists(rdist) == True:
            # Check if site has h1, h2 values
            if site_catchment[ID] == None:
                pass
            else:
                # Query site and sub-catchments
                h1 = catch_hier[site_catchment[ID]][0]
                h2 = catch_hier[site_catchment[ID]][1]

                arcpy.SelectLayerByAttribute_management(catchments, "NEW_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < ' + str(h2))
                count = int(arcpy.GetCount_management(catchments).getOutput(0))
                write('Query: ' + str(count) + ' -> | ')

                try:
                    dist = arcpy.Raster(rdist)
                    far = arcpy.Raster(fa)

                    # Dissolve sub-catchments to yield site catchment
                    arcpy.Dissolve_management(catchments, sc, "", "", "MULTI_PART", "")

                    # Environment settings
                    env.mask = sc
                    env.extent = sc

                    # Extract inverse distances for target land use
                    # If target land use is present, get distance; else keep NoData
                    con_lu = Con(raster_lu, dist, raster_lu, "Value = 1")
                    num = con_lu * far

                    targetDist = arcpy.RasterToNumPyArray(num, nodata_to_value = -1)
                    targetDist = np.ma.masked_array(targetDist, targetDist == -1)
                    targetDist = targetDist + 1
                    targetDist = np.power(targetDist, -1)
                    d[ID][0] = np.sum(targetDist)

                    write('Target -> | ')

                    # Extract inverse distances for entire catchment
                    denom = dist * far
                    totalDist = arcpy.RasterToNumPyArray(denom, nodata_to_value = -1)
                    totalDist = np.ma.masked_array(totalDist, totalDist == -1)
                    totalDist = totalDist + 1
                    totalDist = np.power(totalDist, -1)
                    d[ID][1] = np.sum(totalDist)

                    write('Total -> | ')

                    # Clean up and print loop time
                    # Delete catchment polygons
                    arcpy.Delete_management(sc)

                    # Delete the intermediate rasters
                    arcpy.Delete_management(con_lu)
##                    arcpy.Delete_management(num)
##                    arcpy.Delete_management(denom)

                    # Delete the arrays
                    targetDist = None
                    totalDist = None

                    stop = time.time()
                    elapsed = (stop - start)/60
                    elapsed = round(elapsed, 1)
                    write('| Loop ' + str(siteCounter) + ': ' + str(elapsed) + ' minutes\n')

                except Exception:
                    print "Site FAILED: " + str(ID)

                    e = sys.exc_info()[1]
                    print(e.args[0] + "\n")
                    arcpy.AddError(e.args[0])

            arcpy.ClearEnvironment("mask")
            arcpy.ClearEnvironment("extent")

            arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")
            arcpy.SelectLayerByAttribute_management(sites, "CLEAR_SELECTION")

        else:
            stop = time.time()
            elapsed = (stop - start)/60
            elapsed = round(elapsed, 1)
            write('Loop ' + str(siteCounter) + ': ' + str(elapsed) + ' minutes\n')

solutions = root + "\\" + "site_IDW_HA-FLO_" + str(abs(p)) + "_" + land_use + ".csv"
with open(solutions, 'wb') as csv_file:
    w = csv.writer(csv_file)
    fnames = ["SiteId", "idw.num", "idw.denom"]
    w.writerow(fnames)
    for k, v in d.items():
          w.writerow([k, v[0], v[1]])

print "SCRIPT COMPLETE"
