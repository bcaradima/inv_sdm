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
scratch = root + "\\" + "Scratch.gdb"
results = root + "\\" + "results" # define folder name to store results

env.workspace = gdb
env.scratchWorkspace = scratch
env.overwriteOutput = True

# FUNCTIONS

# INPUT DATA #
# Required FCs: subcatchments, sites, WWTP/HEPs, river network
sc = gdb + "\\" + "Catchments\GAB_EZG_CH"
sites = gdb + "\\" + "chem_sites_catchments"

wwtp = gdb + "\\" + "RiverStructures\wwtp"
hep = gdb + "\\" + "RiverStructures\hep"

network = gdb + "\\" + "Rivers\\rvr_network"

# TEMPORARY DATA #
# Make feature layers
sites = arcpy.MakeFeatureLayer_management(sites, "site_lyr")
catchments = arcpy.MakeFeatureLayer_management(sc, "sc_lyr")
wwtp = arcpy.MakeFeatureLayer_management(wwtp, "wwtp_lyr")
hep = arcpy.MakeFeatureLayer_management(hep, "hep_lyr")

# Dictionaries to associate sites, subcatchments, and hierarchy values
site_catchment = {} # siteId : EZGNR pairs to associate sites with subcatchments
catch_hier = {} # EZGNR : H1, H2 values to associate subcatchments with catchment hierarchy

# Load list of all distinct site IDs
siteID = []
cursor = arcpy.SearchCursor(sites)
for row in cursor:
    siteID.append(row.getValue("StationNo_1"))
del row, cursor

##fileName = map(lambda n: n.replace(" ","_").replace(".", "_").replace("-", "_"), siteID)

# Loop counter
loopCounter = 0

##arcpy.BuildNetwork_na(network)

print "Script setup... done"

write = sys.stdout.write

# GEOPROCESSING #
# Match sites to sub-catchments
write('Perform spatial join -> | ')
sj = env.scratchWorkspace + "\\" + "spatialJoin"
arcpy.SpatialJoin_analysis(sites, catchments, sj, "JOIN_ONE_TO_ONE", "KEEP_ALL", match_option="INTERSECT", search_radius="", distance_field_name="")
spatialJoin = arcpy.MakeFeatureLayer_management(sj, "sj_lyr")

cursor = arcpy.da.SearchCursor(spatialJoin, ("StationNo_1", "EZG_NR", "h1", "h2"))
for row in cursor:
    site_catchment[row[0]] = row[1]
    catch_hier[row[1]] = (row[2], row[3])
del cursor

arcpy.Delete_management(sj)
arcpy.Delete_management(spatialJoin)
write('Join complete \n')

# Match sites to sub-catchments
for ID in siteID:
    write('Site: ' + str(ID) + ' | ')

##    i = siteID.index(str(ID))
##    FN = fileName[i]

    CostMatrix = results + "\\" + "CM_" + str(ID)
    solution = results + "\\" + "solution_" + str(ID) + ".txt"

    parameter1 = "river_network_filtered SHAPE;river_network_Junctions NONE"
    parameter2 = "river_network_filtered #;river_network_Junctions #"

    arcpy.SelectLayerByAttribute_management(sites, "NEW_SELECTION", "\"StationNo_1\" = \'" + str(ID) + "\'")

    h1 = catch_hier[site_catchment[ID]][0]
    h2 = catch_hier[site_catchment[ID]][1]

    arcpy.SelectLayerByAttribute_management(catchments, "NEW_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < '+ str(h2))

    # For each sub-catchment hierarchy, check for wwtp/hep facilities
    arcpy.SelectLayerByLocation_management(wwtp, "WITHIN", catchments)
    arcpy.SelectLayerByLocation_management(hep, "WITHIN", catchments)

    ww = int(arcpy.GetCount_management(wwtp).getOutput(0))
    hydro = int(arcpy.GetCount_management(hep).getOutput(0))

    write('Query -> | ')
    # if wwtp/hep count is not zero, load them into
    if ww > 0:
        arcpy.MakeODCostMatrixLayer_na(network, CostMatrix, "Length", "", "", "", "NO_UTURNS", ["Oneway"], "NO_HIERARCHY", "", "NO_LINES", "")
        # Add the site in any case
        arcpy.AddLocations_na(CostMatrix, "Destinations", sites, "Name StationNo_1 #", "100 Meters", "", parameter1, "MATCH_TO_CLOSEST", "APPEND", "NO_SNAP", "5 Meters", "INCLUDE", parameter2)

        # Add any wwtp/hep sites as Origins headed downstream to Destination (site) along a one-way network
        if ww > 0:
            arcpy.AddLocations_na(CostMatrix, "Origins", wwtp, "Name ARA_Nr #", "250 Meters", "", parameter1, "MATCH_TO_CLOSEST", "APPEND", "NO_SNAP", "5 Meters", "INCLUDE", parameter2)
##        if hydro > 0:
##            arcpy.AddLocations_na(CostMatrix, "Origins", hep, "Name xtf_id #", "250 Meters", "", parameter1, "MATCH_TO_CLOSEST", "APPEND", "NO_SNAP", "5 Meters", "INCLUDE", parameter2)


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
            del cursor

        arcpy.Delete_management(table)
        arcpy.Delete_management(CostMatrix)
        write('Solving for ' + str(ww) + ' WWTPs | ')

    else:
        write('No origins upstream | ')

    loopCounter = loopCounter + 1
    write('Loop ' + str(loopCounter) + '\n')

    arcpy.SelectLayerByAttribute_management(wwtp, "CLEAR_SELECTION")
    arcpy.SelectLayerByAttribute_management(catchments, "CLEAR_SELECTION")

print "SCRIPT COMPLETE"