import arcpy, os, string, csv, sys, time
from arcpy import env

arcpy.CheckOutExtension('spatial')
arcpy.CheckOutExtension('network')

from arcpy.sa import *

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\DEM_analysis" # define top-level dir
gdb = r"D:\Workspace\DEM_analysis\EcologicalData.gdb" # input data
results = r"D:\Workspace\DEM_analysis\Results.gdb" # output results
scratch = r"D:\Workspace\DEM_analysis\Scratch.gdb" # temporary data
##output = r"D:\Workspace\DEM_analysis\outputs"

env.workspace = gdb
env.scratchWorkspace = scratch
env.overwriteOutput = True
env.parallelProcessingFactor = "100%"

# INPUT DATA #
subcatch = gdb + "\\" + "Catchments\GAB_EZG_CH"
sites = gdb + "\\" + "Invertebrates\DistinctSites_select"

##dem = root + "\\" + "swissALTI3D.gdb\swissALTI3D_fill_20m"
fd = root + "\\" + "swissALTI3D.gdb\swissALTI3D_fd_20m"
### Binary (0 or 1) raster indicating presence of streams
##riverRaster = gdb + "\\" + "BinaryRiver_2"

##dem = root + "\\" + "DEM25.gdb\dem25_fill"
##fd = root + "\\" + "DEM25.gdb\dem25_fd"
### Binary (0 or 1) raster indicating presence of streams
##riverRaster = gdb + "\\" + "BinRivers_25"

# TEMPORARY DATA #
# Make feature layers
subcatch = arcpy.MakeFeatureLayer_management(subcatch, "sc_lyr")
sites = arcpy.MakeFeatureLayer_management(sites, "site_lyr")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catch = {} # siteId : EZGNR pairs to associate sites with subcatchments
catch_hier = {} # EZGNR : H1, H2 values to associate subcatchments with catchment hierarchy
site_val = {} # siteId : flow distance to outlet

# Load list of all distinct site IDs
siteID = []

cursor = arcpy.SearchCursor(sites)
for row in cursor:
    siteID.append(row.getValue("siteId"))
del row, cursor

# Query each site and load siteId : EZGNR pairs
# Intensive approach since multiple sites may belong to a single subcatchment
for ID in siteID:
    arcpy.SelectLayerByAttribute_management(sites, "NEW_SELECTION", "\"SiteId\" = \'" + str(ID) + "\'")
    arcpy.SelectLayerByLocation_management(subcatch, "CONTAINS", sites)

    cursor = arcpy.SearchCursor(subcatch)
    for row in cursor:
        site_catch[ID] = row.EZG_NR
    try:
        del row, cursor
    except:
        del cursor

    arcpy.SelectLayerByAttribute_management(subcatch, "CLEAR_SELECTION")
#    print str(site_catch[ID])

# Counters for print output statements
siteCounter = 0
siteFail = []
print "Script setup... done"

write = sys.stdout.write

##siteID = ['CSCF_644203']

# GEOPROCESSING #
for ID in siteID:
    start = time.time()
    write('Site: ' + str(ID) + ' | ')
    siteCounter = siteCounter + 1
    extractRaster = scratch + "\\" + "ext_" + str(ID)
##    nullRaster = scratch + "\\" + "null_" + str(ID)
##    riverSave = scratch + "\\" + "FD_river_" + str(ID)
    finalSave = results + "\\" + "final_" + str(ID)

    outputED = scratch + "\\" + "euclideanDistance_" + str(ID)
    extractSave = scratch + "\\" + "extract_" + str(ID)
    fdSave = scratch + "\\" + "fd_" + str(ID)
    minusSave = scratch + "\\" + "minus_" + str(ID)
    table = scratch + "\\" + "site_" + str(ID)
    outputRaster = scratch + "\\" + "temp_" + str(ID)

    # Select sub-catchments upstream of a sample
    arcpy.SelectLayerByAttribute_management(sites, "NEW_SELECTION", "\"SiteId\" = \'" + str(ID) + "\'")
    arcpy.SelectLayerByLocation_management(subcatch, "CONTAINS", sites)

    cursor = arcpy.SearchCursor(subcatch)
    for row in cursor:
        catch_hier[row.EZG_NR] = (row.H1, row.H2)
    try:
        del row, cursor
    except:
        del cursor

    h1 = catch_hier[site_catch[ID]][0]
    h2 = catch_hier[site_catch[ID]][1]

    arcpy.SelectLayerByAttribute_management(subcatch, "ADD_TO_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < ' + str(h2))
    write('Query -> | ')

##    try:
    extractRaster = arcpy.gp.ExtractByMask_sa(fd, subcatch)

    write('Extract -> | ')

    # Flow distance to stream
##        nullRaster = arcpy.gp.SetNull_sa(riverRaster, extractRaster)
##        river_FD = arcpy.gp.FlowLength_sa(nullRaster, "DOWNSTREAM", "")
##        river_FD = arcpy.Raster(river_FD)
##        river_FD.save(riverSave)
##
##        write('River FD -> | ')

    # Flow distance to catchment outlet
    initialFD = arcpy.gp.FlowLength_sa(extractRaster, "DOWNSTREAM", "")
    initialFD = arcpy.Raster(initialFD)
##    initialFD.save(fdSave)

    write('Flow -> | ')

    # Correct flow distance to site, not to outlet
    arcpy.ExtractValuesToTable_ga(sites, initialFD, table)
    cursor = arcpy.da.SearchCursor(table, ("Value"))
    for row in cursor:
        site_val[ID] = row[0]
    del cursor

    x = site_val[ID]

    outMinus = Minus(initialFD, x)
    outMinus.save(minusSave)
    outSetNull = SetNull(outMinus, outMinus, "VALUE < 1")
##    n = scratch + "\\" + "null_" + str(ID)
##    outSetNull.save(n)

    # Set flow lengths less than Euclidean distance to Null
    arcpy.env.mask = initialFD
    arcpy.env.extent = initialFD
    arcpy.env.snapRaster = initialFD

    EucDist = arcpy.gp.EucDistance_sa(sites, outputED, "", "20", "")
    EucDist = Raster(EucDist)
##    e = scratch + "\\" + "ed_" + str(ID)
##    EucDist.save(e)

    outputRaster = Con((outSetNull < EucDist), -9999, outSetNull)
    outFinal = SetNull(outputRaster, outputRaster, "VALUE = -9999")
    outFinal.save(finalSave)

    write('Clean & Save || ')

    # Clean up and print loop time
    arcpy.SelectLayerByAttribute_management(subcatch, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(sites, "CLEAR_SELECTION")



    arcpy.Delete_management(extractRaster)
    fdRaster = gdb + "\\" + "DOWNSTREAM"
    arcpy.Delete_management(fdRaster)
    arcpy.Delete_management(initialFD)
    arcpy.Delete_management(table)
    arcpy.Delete_management(outMinus)
    arcpy.Delete_management(outSetNull)
##    arcpy.Delete_management(nullRaster)


#    arcpy.Delete_management(EucDist)
    arcpy.Delete_management(outputRaster)
##        arcpy.Delete_management(outSetNull)

    stop = time.time()
    elapsed = (stop - start)/60
    elapsed = round(elapsed, 1)
    write('Loop ' + str(siteCounter) + ': ' + str(elapsed) + ' minutes\n')

##    except:
##        print "Site FAILED: " + str(ID)
##        siteFail.append(str(ID))


print "SCRIPT COMPLETE"