#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      Bogdan
#
# Created:     28/04/2017
# Copyright:   (c) Bogdan 2017
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, os, string, csv, sys, time
from arcpy import env
arcpy.CheckOutExtension('spatial')
arcpy.CheckOutExtension('GeoStats')
from arcpy.sa import *

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\InputData1" # define top-level dir to store results
gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
results = root + "\\" + "results.gdb"
scratch = root + "\\" + "scratch.gdb"
output_dir = root + "\\" + "outputs"

env.workspace = gdb
env.scratchWorkspace = scratch
env.overwriteOutput = True
env.parallelProcessingFactor = "100%"

# INPUT DATA #
c = "Catchments\\GAB_EZG_CH"
s = "Sites\\temp_sites_CH"

rivers = "riverBinary"
solar_rad = "solar_rad"

# TEMPORARY DATA #
# Make feature layers
arcpy.CopyFeatures_management(c, "in_memory\\catchments")
catchments = arcpy.MakeFeatureLayer_management("in_memory\\catchments", "catch_lyr")

arcpy.CopyFeatures_management(s, "in_memory\\sites")
sites = arcpy.MakeFeatureLayer_management("in_memory\\sites", "site_lyr")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # siteId : EZGNR pairs to associate sites with subcatchments
catch_hier = {} # EZGNR : H1, H2 values to associate subcatchments with catchment hierarchy

site_solar = {} # SiteId : site point solar radiation
upstream_solar = {} # SiteId : upstream solar radiation

# Load list of all distinct site IDs
siteID = []
with arcpy.da.SearchCursor(sites, ("ID")) as cursor:
    for row in cursor:
        siteID.append(row[0])

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
with arcpy.da.SearchCursor(sj, ("ID", "EZG_NR", "h1", "h2")) as cursor:
    for row in cursor:
        site_catchment[row[0]] = row[1]
        catch_hier[row[1]] = (row[2], row[3])
write('Join complete \n')

# Get site-level solar radiation
temp = {}
with arcpy.da.SearchCursor(sites, ("OBJECTID", "ID")) as cursor:
    for row in cursor:
        temp[row[0]] = row[1]

##arcpy.ExtractValuesToTable_ga(sites, solar_rad, "site_solar", "", "")
##with arcpy.da.SearchCursor("site_solar", ("SrcID_Feat", "Value")) as cursor:
##    for row in cursor:
##        site_solar[temp[row[0]]] = row[1]
##del temp
# Get solar radiation (25m) along river network (10m binary raster)
solar_stream = Con(rivers, solar_rad, rivers, "Value = 10")

for ID in siteID:
    start = time.time()
    write('Site: ' + str(ID) + ' | ')
    loopCounter = loopCounter + 1

    # Select sub-catchments upstream of a sample
    h1 = catch_hier[site_catchment[ID]][0]
    h2 = catch_hier[site_catchment[ID]][1]

    env.extent = c
    arcpy.SelectLayerByAttribute_management(catchments, "NEW_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < '+ str(h2))
    count = int(arcpy.GetCount_management(catchments).getOutput(0))

    try:
        write('Query ' + str(count) + ' -> | ')

        # Dissolve the sub-catchments
        tfc = "catchment_dissolve"
        arcpy.Dissolve_management(catchments, tfc, "", "", "MULTI_PART", "")
        env.extent = tfc

##        # Clip solar radiation along river network by catchment
##        upstream_solar = scratch + "\\" + "upstream_solar"
##        arcpy.Clip_management(out, "#", upstream_solar, tfc, "", "ClippingGeometry")
##        write('Clip -> | ')

        table = ZonalStatisticsAsTable(tfc, "OBJECTID", solar_stream, "zs_table", "DATA", "ALL")
        write('ZS table -> | ')
##        # Sample the solar radiation along the upstream river network
##        # WARNING: IDs are not unique within the loop and so it won't work!
##        output_table ="upstream_sample"
##        arcpy.gp.Sample_sa(solar_rad, upstream_rivers, output_table, "NEAREST", "VALUE")
##        with arcpy.da.SearchCursor(output_table, ("solar_rad")) as cursor:
##            for row in cursor:
##                upstream_solar[ID] = row[0]
##        write('Sample -> | ')
        arcpy.AddField_management(table, "ID", "SHORT")
        with arcpy.da.UpdateCursor(table, ("ID")) as cursor:
            for row in cursor:
                row[0] = ID
                cursor.updateRow(row)

        out_table = "ZS_site_" + str(ID) + ".csv"
        arcpy.TableToTable_conversion(table, output_dir, out_table)

        arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")
        arcpy.Delete_management(tfc)
        arcpy.Delete_management(table)

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

##solution = root + "\\" + "site_temp_solarPoint.csv"
##with open(solution, 'wb') as csv_file:
##    w = csv.writer(csv_file)
##    fnames = ["ID", "solar.site"]
##    w.writerow(fnames)
##    for k, v in site_solar.items():
##       w.writerow([k, v])

##solution = root + "\\" + "site_temp_solarUpstream.csv"
##with open(solution, 'wb') as csv_file:
##    w = csv.writer(csv_file)
##    fnames = ["ID", "solar.upstream"]
##    w.writerow(fnames)
##    for k, v in upstream_solar.items():
##       w.writerow([k, v[0]])

print "SCRIPT COMPLETE."
