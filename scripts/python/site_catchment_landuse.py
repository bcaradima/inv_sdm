#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      Bogdan
#
# Created:     27/07/2017
# Copyright:   (c) Bogdan 2017
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, os, sys, string, csv, time
from arcpy import env
from arcpy.sa import *

arcpy.CheckOutExtension('spatial')
arcpy.CheckOutExtension('network')

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\InitialAnalysis" # define top-level dir to store results
gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
scratch_gdb = root + "\\" + "scratch.gdb"
scratch_dir = root + "\\" + "scratch"

env.workspace = gdb
env.scratchWorkspace = scratch_gdb
env.overwriteOutput = True

# FUNCTIONS

# INPUT DATA #
# Required FCs: subcatchments, sites, WWTP/HEPs, river network
c = gdb + "\\" + "Catchments\GAB_EZG_CH"
s = gdb + "\\" + "inv_sites_CH"

#land_use = gdb + "\\" + "AS09_72"
land_use = gdb + "\\" + "AS97_72"

# TEMPORARY DATA #
# Make feature layers
arcpy.CopyFeatures_management(c, "in_memo\\catchments")
catchments = arcpy.MakeFeatureLayer_management("in_memory\\catchments", "catchment_lyr")

arcpy.CopyFeatures_management(s, "in_memory\\sites")
sites = arcpy.MakeFeatureLayer_management("in_memory\\sites", "site_lyr")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # siteId : EZGNR pairs to associate sites with subcatchments
catch_hier = {} # EZGNR : H1, H2 values to associate subcatchments with catchment hierarchy
zs = {} # SiteId : zonal stats
site_catchment_area = {}

# Load list of all distinct site IDs
siteID = []
with arcpy.da.SearchCursor(sites, ["SiteId"]) as cursor:
    for row in cursor:
        siteID.append(row[0])
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
cursor = arcpy.da.SearchCursor(sj, ("SiteId", "EZG_NR", "h1", "h2"))
for row in cursor:
    site_catchment[row[0]] = row[1]
    catch_hier[row[1]] = (row[2], row[3])
del cursor
arcpy.Delete_management(sj)
write('Join complete \n')

# Main loop
for ID in siteID:
    start = time.time()
    # Exchange actual siteID with
    i = siteID.index(str(ID))
    FN = fileName[i]
    loopCounter = loopCounter + 1
    write('Site: ' + str(ID) + ' | ')

    # Select sub-catchments upstream of a sample
    h1 = catch_hier[site_catchment[ID]][0]
    h2 = catch_hier[site_catchment[ID]][1]

    arcpy.SelectLayerByAttribute_management(catchments, "NEW_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < '+ str(h2))
    write('Query -> | ')

    try:
        # Dissolve selected sub-catchments for a site
        tfc = env.scratchWorkspace + "\\" + "site_catch"
        arcpy.Dissolve_management(catchments, tfc, "", "", "MULTI_PART", "")

        arcpy.AddField_management(tfc, "SiteId", "TEXT")
        with arcpy.da.UpdateCursor(tfc, ("SiteId", "Shape_Area")) as cursor:
            for row in cursor:
                row[0] = ID
                site_catchment_area[ID] = row[1]
                cursor.updateRow(row)
        env.extent = tfc

        write('Dissolve -> | ')

        # Tabulate area: for each raster class, compute the areas per zone
        # Converts output DBF to CSV and deletes the written DBF and XML files
        table_dbf = scratch_dir + "\\site_" + str(FN) + ".dbf"
        table_csv = "site_" + str(FN) + ".csv"
        xml_file = scratch_dir + "\\site_" + str(FN) + ".txt.xml"

        TabulateArea(tfc, "SiteId", land_use, "Value", table_dbf, 10)

        # Add the zone areas to the DBF file
        arcpy.AddField_management(table_dbf, "Shape_Area", "DOUBLE")
        with arcpy.da.UpdateCursor(table_dbf, ("SiteId", "Shape_Area")) as cursor:
            for row in cursor:
                row[1] = site_catchment_area[ID]
                cursor.updateRow(row)

        arcpy.TableToTable_conversion(table_dbf, scratch_dir, table_csv)

        write('ZS -> | ')

        arcpy.Delete_management(table_dbf)
        arcpy.Delete_management(xml_file)
        arcpy.Delete_management(tfc)
        arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")
        arcpy.SelectLayerByAttribute_management(sites, "CLEAR_SELECTION")

        arcpy.ClearEnvironment("extent")

    except Exception:
        print "Site FAILED: " + str(ID) + ' | '
        siteFail.append(str(ID))

        e = sys.exc_info()[1]
        print(e.args[0] + "\n")
        arcpy.AddError(e.args[0])

    stop = time.time()
    elapsed = (stop - start)/60
    elapsed = round(elapsed, 1)
    write('| Loop ' + str(loopCounter) + ': ' + str(elapsed) + ' min\n')

    arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(sites, "CLEAR_SELECTION")

### Export the results as a CSV table
##arcpy.CreateTable_management(gdb, "results_ZS")
##statNames = ["MEAN", "MAJORITY", "MAXIMUM", "MEDIAN", "MINIMUM", "MINORITY", "RANGE", "STD", "SUM", "VARIETY", "MINMAX", "MEANSTD", "MIN_MAX_MEAN"]
##arcpy.AddField_management("results_ZS", "SiteId", "TEXT", "20")
##for stat in statNames:
##    arcpy.AddField_management("results_ZS", stat, "DOUBLE")
##
##fields = ["SiteId", "MEAN", "MAJORITY", "MAXIMUM", "MEDIAN", "MINIMUM", "MINORITY", "RANGE", "STD", "SUM", "VARIETY", "MINMAX", "MEANSTD", "MIN_MAX_MEAN"]
##nrows = len(zs) + 1
##with arcpy.da.InsertCursor('results_ZS', fields) as cursor:
##    for k, v in zs.items():
##        cursor.insertRow((key, v[0], v[1], v[2], v[3], v[4]))
##del cursor
##
##output = root + "\\" + "results_ZS.csv"
##
###--first lets make a list of all of the fields in the table
##fields = arcpy.ListFields("results_ZS")
##field_names = [field.name for field in fields]
##
##with open(output,'wb') as f:
##    w = csv.writer(f)
##    #--write all field names to the output file
##    w.writerow(field_names)
##
##    #--now we make the search cursor that will iterate through the rows of the table
##    for row in arcpy.SearchCursor("results_ZS"):
##        field_vals = [row.getValue(field.name) for f in fields]
##        w.writerow(field_vals)
##    del row

print "SCRIPT COMPLETE"