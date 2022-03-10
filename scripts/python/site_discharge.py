#-------------------------------------------------------------------------------
# Purpose: Joins a feature class and table based on a common field, reads the ID
# and value pairs into a dictionary, and writes the dictionary to a simple CSV.
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
root = r"D:\Workspace\InitialAnalysis" # define top-level dir to store results

gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
scratch = root + "\\" + "scratch.gdb"

env.workspace = gdb
env.scratchWorkspace = scratch
env.overwriteOutput = True
env.parallelProcessingFactor = "100%"

# INPUT DATA #
sc = "Catchments\GAB_EZG_CH"
s = "Sites\inv_sites_CH"
tbl = "MJA"

# TEMPORARY DATA #
# Make feature layers
catchments = arcpy.MakeFeatureLayer_management(sc, "sc_lyr")
sites = arcpy.MakeFeatureLayer_management(s, "site_lyr")
dc = arcpy.MakeTableView_management(tbl, "tbl_view")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # siteId : EZG pairs to associate sites with subcatchments
catch_hier = {} # EZG : H1, H2 values to associate subcatchments with catchment hierarchy
catchment_Q = {} # EZG : Q pairs associate catchments with discharge values (m3/s)
site_discharge = {}

# Load list of all distinct site IDs
siteID = []
cursor = arcpy.da.SearchCursor(sites, ("SiteId"))
for row in cursor:
    siteID.append(row[0])
del row, cursor

# Counters for print output statements
loopCounter = 0
siteFail = []

print "Script setup... done"
write = sys.stdout.write

# GEOPROCESSING #
# Match sites to sub-catchments
write('Spatial join -> | ')
sj = env.scratchWorkspace + "\\" + "spatialJoin"
arcpy.SpatialJoin_analysis(sites, catchments, sj, "JOIN_ONE_TO_ONE", "KEEP_ALL", match_option="INTERSECT", search_radius="", distance_field_name="")
with arcpy.da.SearchCursor(sj, ("SiteId", "EZG_NR", "h1", "h2")) as cursor:
    for row in cursor:
        site_catchment[row[0]] = row[1]
        catch_hier[row[1]] = (row[2], row[3])
arcpy.Delete_management(sj)

# Load the EZG : Q data
write('Read EZG : Q pairs \n')
with arcpy.da.SearchCursor(dc, ("EZG_NR", "AS_M_S")) as cursor:
    for row in cursor:
        catchment_Q[row[0]] = row[1]

# For each site, look up the dictionaries to obtain the Q based on spatial join
for ID in siteID:
    start = time.time()
    write('Site: ' + str(ID) + ' | ')
    loopCounter = loopCounter + 1

    try:
        site_discharge[ID] = catchment_Q[site_catchment[ID]]
        write('Q retrieved | ')
        write('Loop ' + str(loopCounter) + '\n')

    except:
        print "Site FAILED: " + str(ID)
        siteFail.append(str(ID))

# Write the results
solution = root + "\\" + "site_Discharge.csv"

with open(solution, 'wb') as csv_file:
    w = csv.writer(csv_file)
    fnames = ["SiteId", "Discharge"]
    for k, v in site_discharge.items():
       w.writerow([k, v])

print "SCRIPT COMPLETE"
