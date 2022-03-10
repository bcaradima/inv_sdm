#-------------------------------------------------------------------------------
# Purpose: outputs GeoTIFFs into a folder and prepares a batch file for FragStats
# Output raster has value 1 for land use, an arbitrary positive value for the
# landscape background, and NoData for outside the landscape boundary.
# Output rasters are GeoTIFF files to avoid compatibility issues between
# ArcGIS 10.1 or later and Fragstats 4.2
#
# Note: set background value and distance threshold with which to select
# forest within distance of streams in site catchment
#
# Author:      Bogdan
#
# Created:     26/09/2016
# Copyright:   (c) Bogdan 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, os, string, csv, sys, time
from arcpy import env

arcpy.CheckOutExtension('spatial')

from arcpy.sa import *

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\InputData3" # define top-level dir to store results

gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
scratch = root + "\\" + "Scratch.gdb"
results = root + "\\" + "OUTPUTS"


env.workspace = gdb
env.scratchWorkspace = scratch
env.overwriteOutput = True
env.parallelProcessingFactor = "100%"

# INPUT DATA #
c = gdb + "\\" + "Catchments\GAB_EZG_CH"
s = gdb + "\\" + "chem_sites_catchments"
rvr = gdb + "\\" + "Rivers\\rvr_clean"
lu = gdb + "\\" + "TLM_Wald"


# TEMPORARY DATA #
# Make feature layers
mfc1 = "in_memory\\catchments"
arcpy.CopyFeatures_management(c, mfc1)
catchments = arcpy.MakeFeatureLayer_management(mfc1, "sc_lyr")

mfc2 = "in_memory\\sites"
arcpy.CopyFeatures_management(s, mfc2)
sites = arcpy.MakeFeatureLayer_management(mfc2, "site_lyr")

mfc3 = "in_memory\\rvr"
arcpy.CopyFeatures_management(rvr, mfc3)
rivers = arcpy.MakeFeatureLayer_management(mfc3, "r_lyr")

mfc4 = "in_memory\\land_use"
arcpy.CopyFeatures_management(lu, mfc4)
land_use  = arcpy.MakeFeatureLayer_management(mfc4, "lu_lyr")

# Dictionaries to associate sites, catchmentsments, and hierarchy values
site_catchment = {} # siteId : EZGNR pairs to associate sites with catchments
catch_hier = {} # EZGNR : H1, H2 values to associate catchmentsments with catchment hierarchy

rProperties = {}

# Load list of all distinct site IDs
siteID = []
cursor = arcpy.SearchCursor(sites)
for row in cursor:
    siteID.append(row.getValue("StationNumber"))
del row, cursor

##fileName = map(lambda n: n.replace(" ","_").replace(".", "_").replace("-", "_"), siteID)

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

cursor = arcpy.da.SearchCursor(spatialJoin, ("StationNumber", "EZG_NR", "h1", "h2"))
for row in cursor:
    site_catchment[row[0]] = row[1]
    catch_hier[row[1]] = (row[2], row[3])
del cursor

arcpy.Delete_management(sj)
arcpy.Delete_management(spatialJoin)
write('Join complete \n')

# Loop through the sites and produce the ASCII landscape rasters
for ID in siteID:
    start = time.time()
    loopCounter = loopCounter + 1

    cs = map(lambda n: n.replace(".tif", ""), os.listdir(results))
    if ID not in cs:
        write('Site: ' + str(ID) + ' | ')

##        i = siteID.index(str(ID))
##        FN = fileName[i]

        # Select site catchments, rivers within catchments, land use within a distance of the rivers
        h1 = catch_hier[site_catchment[ID]][0]
        h2 = catch_hier[site_catchment[ID]][1]

        # Perform spatial and attribute queries
        # Count number of catchments before any queries take place
        write('Query -> | ')

        # Select the catchments and print the number selected
        arcpy.SelectLayerByAttribute_management(catchments, "NEW_SELECTION", '"h1" >= ' + str(h1) + ' AND "h1" < ' + str(h2))

        x = arcpy.Describe(catchments)
        env.extent = x.extent

        count = int(arcpy.GetCount_management(catchments).getOutput(0))
        write('NC: ' + str(count) + ' | ')

        # Select any land use intersecting the selected catchment and within a distance of rivers in the catchment
        arcpy.SelectLayerByLocation_management(land_use, "INTERSECT", catchments)
        count = int(arcpy.GetCount_management(land_use).getOutput(0))
        write('LU: ' + str(count) + ' | ')

        if count > 0:
            arcpy.SelectLayerByLocation_management(rivers, "INTERSECT", catchments)
            rc = scratch + "\\" + "rc_" + str(ID)
            arcpy.Clip_analysis(rivers, catchments, rc, "")

            arcpy.SelectLayerByLocation_management(land_use, "WITHIN_A_DISTANCE", rc, "100 Meters", "SUBSET_SELECTION")
            count = int(arcpy.GetCount_management(land_use).getOutput(0))

            # If land use polygons are selected, try to export a land use raster
            if count > 0:
##                try:
                # Clip the land use by catchment
                tempFC = scratch + "\\" + "catchment_" + str(ID)
                arcpy.Dissolve_management(catchments, tempFC, "", "", "", "")
                arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")

                clu = scratch + "\\" + "clu_" + str(ID)
                arcpy.Clip_analysis(land_use, tempFC, clu, "")

                write('Clip -> | ')

                # Convert land use to raster
                # set tool environments: mask, extent, cell size
                arcpy.env.mask = tempFC # Note: setting is not honored.
                arcpy.env.extent = tempFC

                lur = scratch + "\\" + "landuse_" + str(ID)
                arcpy.FeatureToRaster_conversion(clu, "Forest", lur, "20")

                # Keep 1 (forest) values, reclass NoData to 666
                reclass = scratch + "\\" + "reclass_" + str(ID)
                arcpy.gp.Reclassify_sa(lur, "Value", "1 1;NODATA 666", reclass, "DATA")
                reclass = arcpy.Raster(reclass)

                # Extract raster by catchment (1:forest, 666:background, NoData:outside catchment)
                extract = scratch + "\\" + "extract_" + str(ID)
                arcpy.gp.ExtractByMask_sa(reclass, tempFC, extract)
                final = arcpy.Raster(extract)

##                final = scratch + "\\" + "final_" + str(FN)
##                arcpy.gp.Reclassify_sa(extract, "Value", "0 NODATA;1 1;666 666", final, "DATA")

##                arcpy.gp.SetNull_sa(extract, extract, final, """"Value" = 0""")
#                # CopyRaster to GeoTIFF
#                Background value (0):
#                Use this option to remove the unwanted values created around the raster data.
#                The value specified will be distinguished from other valuable data in the raster dataset.
#                For example, a value of zero along the raster dataset's borders will be distinguished from
#                zero values within the raster dataset. The pixel value specified will be set to NoData in the output raster dataset.
                export = results + "\\" + str(ID) + ".tif"
                arcpy.CopyRaster_management(final, export, "NONE", 0, "65535")

                try:
                    arcpy.CopyRaster_management(final, export, "NONE", 0, "65535")

                except:
                    arcpy.Delete_management(final)

##                    final = scratch + "\\" + "final_" + str(FN)
##                    arcpy.gp.Reclassify_sa(extract, "Value", "0 NODATA;1 1;666 666", final, "DATA")
                    final = arcpy.Raster(extract)

                    arcpy.CopyRaster_management(final, export, "NONE", 0, "65535")

                write('F2R -> |')

                # Garbage collection if the site successfully runs
                arcpy.Delete_management(rc)
                arcpy.Delete_management(tempFC)
                arcpy.Delete_management(clu)
                arcpy.Delete_management(lur)
                arcpy.Delete_management(reclass)
                arcpy.Delete_management(extract)
                arcpy.Delete_management(final)

##                except:
##                    siteFail.append(ID)
##                    write(' ERROR | ')

    # Clear selections
    arcpy.SelectLayerByAttribute_management(sites, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(rivers, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(land_use, "CLEAR_SELECTION")

    stop = time.time()
    elapsed = (stop - start)/60
    elapsed = round(elapsed, 1)

    write('| Loop ' + str(loopCounter) + ': ' + str(elapsed) + ' min\n')

write('Writing results ... \n')

for ID in siteID:
    i = siteID.index(str(ID))
##    FN = fileName[i]

    final = results + "\\" + str(ID) + ".tif"
    # Obtain raster properties: InputFileName, CellSize, Background, Rows, Columns, InputDataType
    # http://www.umass.edu/landeco/research/fragstats/documents/User%20guidelines/Running%20via%20GUI/Step%204.htm
    try:
        cellSize = arcpy.GetRasterProperties_management(final, "CELLSIZEX")
        cellSize = cellSize.getOutput(0)

        background = 666

        nrow = arcpy.GetRasterProperties_management(final, "ROWCOUNT")
        nrow = nrow.getOutput(0)

        ncol = arcpy.GetRasterProperties_management(final, "COLUMNCOUNT")
        ncol = ncol.getOutput(0)

        InputDataType = "IDF_GeoTIFF"

        fullPath = results + "\\" + str(ID) + '.tif'
        rProperties[ID] = [fullPath, cellSize, background, nrow, ncol, InputDataType]
    except:
        pass

    write('Processed: ' + str(ID) + '\n')

solution = root + "\\" + "Fragstats_batch.csv"
with open(solution, 'wb') as csv_file:
    writer = csv.writer(csv_file)
    for key, value in rProperties.items():
        writer.writerow(value)

print "SCRIPT COMPLETE"