#-------------------------------------------------------------------------------
# Purpose: Creates, loads, and solves OD cost matrices based on sampling sites and
# upstream subcatchment areas. Calculates downstream distance from point features
# (e.g., dams or wastewater plants) along a river network using a one-way restriction
# for To-From direction. Script assumes all sample sites are contained within a
# subcatchment. See dates 08-20.07.2016 for log of conceptual/code development for
# river networks.
#
# Author:      Bogdan
#
# Created:     22/07/2016
# Copyright:   (c) Bogdan 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, os, string, csv, sys
from arcpy import env

arcpy.CheckOutExtension('spatial')
arcpy.CheckOutExtension('network')

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\InputData4" # define top-level dir to store results
gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
results = root + "\\" + "outputs_sites" # define folder name to store results

env.workspace = gdb
env.overwriteOutput = True

# FUNCTIONS

# INPUT DATA #
# Required FCs: subcatchments, sites, river network
sc = gdb + "\\" + "Catchments\GAB_EZG_CH"
sites = gdb + "\\" + "Invertebrates\DistinctSites_09_26_select"

network = gdb + "\\" + "Rivers\\rvr_network"

# TEMPORARY DATA #
# Make feature layers
sites = arcpy.MakeFeatureLayer_management(sites, "site_lyr")
catchments = arcpy.MakeFeatureLayer_management(sc, "sc_lyr")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # siteId : EZGNR pairs to associate sites with subcatchments
catch_hier = {} # EZGNR : H1, H2 values to associate subcatchments with catchment hierarchy

# Load list of all distinct site IDs
siteID = []
cursor = arcpy.SearchCursor(sites)
for row in cursor:
    siteID.append(row.getValue("siteId"))
del row, cursor

# Loop counter
loopCount = 0

##arcpy.BuildNetwork_na(network)

print "Script setup... done"

write = sys.stdout.write

# GEOPROCESSING #
for ID in siteID:
    write('Site: ' + str(ID) + ' | ')
    CostMatrix = results + "\\" + "CM_" + str(ID)
    solution = results + "\\" + "solution_" + str(ID) + ".txt"

    parameter1 = "river_network_filtered SHAPE;river_network_Junctions NONE"
    parameter2 = "river_network_filtered #;river_network_Junctions #"

    arcpy.SelectLayerByAttribute_management(sites, "NEW_SELECTION", "\"SiteId\" = \'" + str(ID) + "\'")
    arcpy.SelectLayerByLocation_management(catchments, "CONTAINS", sites)

    # Select sub-catchments upstream of a sample
    cursor = arcpy.SearchCursor(catchments)
    for row in cursor:
        site_catchment[ID] = row.EZG_NR
        catch_hier[row.EZG_NR] = (row.H1, row.H2)
    del cursor

    h1 = catch_hier[site_catchment[ID]][0]
    h2 = catch_hier[site_catchment[ID]][1]
    arcpy.SelectLayerByAttribute_management(catchments, "ADD_TO_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < '+ str(h2))

    # For each site-based catchment selection, select all *other* sites
    arcpy.SelectLayerByLocation_management(sites, "WITHIN", catchments)
    arcpy.SelectLayerByAttribute_management(sites, "REMOVE_FROM_SELECTION", "\"SiteId\" = \'" + str(ID) + "\'")

    ss = int(arcpy.GetCount_management(sites).getOutput(0))

    write('Query -> | ')

    if ss > 0:
        arcpy.MakeODCostMatrixLayer_na(network, CostMatrix, "Length", "", "", "", "NO_UTURNS", ["Oneway"], "NO_HIERARCHY", "", "NO_LINES", "")
        # Add the sites upstream from the site of interest
        arcpy.AddLocations_na(CostMatrix, "Origins", sites, "Name SiteId #", "100 Meters", "", parameter1, "MATCH_TO_CLOSEST", "APPEND", "NO_SNAP", "5 Meters", "INCLUDE", parameter2)

        # Add the site of interest
        arcpy.SelectLayerByAttribute_management(sites, "NEW_SELECTION", "\"SiteId\" = \'" + str(ID) + "\'")
        arcpy.AddLocations_na(CostMatrix, "Destinations", sites, "Name SiteId #", "100 Meters", "", parameter1, "MATCH_TO_CLOSEST", "APPEND", "NO_SNAP", "5 Meters", "INCLUDE", parameter2)

        arcpy.Solve_na(CostMatrix, "SKIP", "CONTINUE", "")
        lines = CostMatrix + "\\Lines"
        table = arcpy.MakeTableView_management(lines, "temp")
        fields = arcpy.ListFields(table)
        field_names = map(lambda n:n.name, fields)

        header = string.join(field_names, "\t")

        with open(solution, 'w') as x:
            x.write(header + "\n")
            cursor = arcpy.SearchCursor(table)
            for row in cursor:
                values = string.join(map(lambda n: str(row.getValue(n)), field_names), "\t")
                x.write(values + "\n")
            del row, cursor

        arcpy.Delete_management(table)
        arcpy.Delete_management(CostMatrix)
        write('Solving for ' + str(ss) + ' upstream sites || ')

    else:
        write('No sites upstream || ')

    loopCount = loopCount + 1
    write('Loop ' + str(loopCount) + '\n')

    arcpy.SelectLayerByAttribute_management(sites, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")

    # Garbage management

print "SCRIPT COMPLETE"
