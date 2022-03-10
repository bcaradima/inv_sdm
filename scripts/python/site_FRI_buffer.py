#-------------------------------------------------------------------------------
# Purpose: fast computation of proportions of forest for site-based buffer zones
# within catchment. Includes forest-river intersections within buffer zones.
#
# Author:      Bogdan
#
# Created:     30/03/2017
# Copyright:   (c) Bogdan 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, os, string, csv, sys, time
from arcpy import env
from arcpy.sa import *

arcpy.CheckOutExtension('spatial')
arcpy.CheckOutExtension('network')

# LOCATIONS #
# Local machine paths
root = r"D:\Workspace\InputData1" # define top-level dir to store results
gdb = root + "\\" + "EcologicalData.gdb" # relative path of input database
results = root + "\\" + "results.gdb"
scratch = root + "\\" + "scratch.gdb"

env.workspace = gdb
env.scratchWorkspace = scratch
env.overwriteOutput = True
env.parallelProcessingFactor = "100%"

siteIDfield = "ID"

# INPUT DATA #
c = "Catchments\\GAB_EZG_CH"
s = "Sites\\temp_sites_CH"
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
results = {} # Temporarily stores FRI results for output to GDB table and finally to CSV

# Load list of all distinct site IDs
siteID = []
cursor = arcpy.da.SearchCursor(sites, (siteIDfield))
for row in cursor:
    siteID.append(str(row[0]))
del row, cursor
fileName = map(lambda n: n.replace(" ","_").replace(".", "_").replace("-", "_"), siteID)

# Counters for print output statements
loopCounter = 0
siteFail = []
print "Script setup... done"

write = sys.stdout.write

# GEOPROCESSING #
# Calculate site point coordinates on the nearest stream line
site_tracker = {}
with arcpy.da.SearchCursor(sites, ["OBJECTID", siteIDfield]) as cursor:
    for row in cursor:
        site_tracker[row[0]] = row[1]
    del cursor

output = scratch + "\\" + "near_table"
arcpy.GenerateNearTable_analysis(sites, rivers, output, "", "LOCATION", "NO_ANGLE", "CLOSEST", "", "PLANAR")

near = {}
with arcpy.da.SearchCursor(output, ["IN_FID", "NEAR_DIST", "NEAR_X", "NEAR_Y"]) as cursor:
    for row in cursor:
        near[site_tracker[row[0]]] = row[2], row[3], row[1]
del cursor

# Generate point FC from the near table
spatial_ref = arcpy.Describe(sites).spatialReference
arcpy.MakeXYEventLayer_management(output, "NEAR_X", "NEAR_Y", "near_events", spatial_ref, "")
sr_points = "in_memory\\sr_points"
arcpy.FeatureToPoint_management("near_events", sr_points)

arcpy.AddField_management(sr_points, siteIDfield, "TEXT", "20")
with arcpy.da.UpdateCursor(sr_points, [siteIDfield, "IN_FID"]) as cursor:
    for row in cursor:
        row[0] = site_tracker[row[1]]
        cursor.updateRow(row)
del cursor



# Generate buffers along the river line for clipping the river network
rbuffers = "in_memory\\rbuffers"
rbuffers = arcpy.Buffer_analysis(sr_points, rbuffers, "150 meters", "FULL", "ROUND", "NONE", "", "PLANAR")

# Clip the river network by the buffers
arcpy.Clip_analysis(r, rbuffers, "in_memory\\rivers")
rivers = arcpy.MakeFeatureLayer_management("in_memory\\rivers", "river_lyr")
arcpy.Delete_management(rbuffers)

# Generate buffers along the river line as geometries for fast computation
buffers = arcpy.Geometry()
buffers = arcpy.Buffer_analysis(sr_points, buffers, "150 meters", "FULL", "ROUND", "NONE", "", "PLANAR")

# Match sites to sub-catchments
write('Perform spatial join -> | ')
sj = env.scratchWorkspace + "\\" + "spatialJoin"
arcpy.SpatialJoin_analysis(sr_points, catchments, sj, "JOIN_ONE_TO_ONE", "KEEP_ALL", match_option="INTERSECT", search_radius="", distance_field_name="")

with arcpy.da.SearchCursor(sj, (siteIDfield, "EZG_NR", "h1", "h2")) as cursor:
    for row in cursor:
        site_catchment[row[0]] = row[1]
        catch_hier[row[1]] = (row[2], row[3])

arcpy.Delete_management(sj)
arcpy.Delete_management(sr_points)
arcpy.Delete_management(output)
del(site_tracker, spatial_ref)

write('Join complete \n')

for ID in siteID:
    start = time.time()
    write('Site: ' + str(ID) + ' | ')
    loopCounter = loopCounter + 1

    # Position of site buffer geometry
    i = siteID.index(str(ID))
    FN = fileName[i]

    if site_catchment[ID] == None:
        results[ID] = 0, 0, 0, 0, near[int(ID)][2]
    else:
        h1 = catch_hier[site_catchment[ID]][0]
        h2 = catch_hier[site_catchment[ID]][1]
        arcpy.SelectLayerByAttribute_management(catchments, "NEW_SELECTION", '"H1" >= ' + str(h1) + ' AND "H1" < '+ str(h2))
        count = int(arcpy.GetCount_management(catchments).getOutput(0))
        write('Query -> | ')

        # Dissolve sub-catchments to obtain site catchment polygon
        sc = "in_memory\\sc"
        arcpy.Dissolve_management(catchments, sc, "", "", "MULTI_PART", "")
        write('Dissolve -> | ')

        # Clip the buffer by the site catchment
        buffer_clip = "in_memory\\buffer_clip"
        arcpy.Clip_analysis(buffers[i], sc, buffer_clip)
        write('Clip -> | ')

        # Select forest polygons and river polylines intersecting clipped buffer
        arcpy.SelectLayerByLocation_management(forest, "INTERSECT", buffer_clip)
        arcpy.SelectLayerByLocation_management(rivers, "INTERSECT", buffer_clip)
        count = int(arcpy.GetCount_management(forest).getOutput(0))

        # Get the geometry of the clipped buffer
        b = arcpy.CopyFeatures_management(buffer_clip, arcpy.Geometry())[0]

        # For each buffer, get the intersecting forest area and total area
        f_area = 0
        b_area = b.getArea("PLANAR", "SQUAREMETERS")

        fri_length = 0
        river_length = 0

        # For each forest polygon and for each river line, get the forest area
        with arcpy.da.SearchCursor(forest, ["SHAPE@"]) as fcursor:
            for f in fcursor:
                fpoly = f[0]
                f_area = f_area + b.intersect(fpoly, 4).getArea("PLANAR", "SQUAREMETERS")

                # For each forest polygon and for each river line, get the FRI length
                with arcpy.da.SearchCursor(rivers, ["SHAPE@"]) as frcursor:
                    for fr in frcursor:
                        frline = fr[0]
                        fri_length = fri_length + fpoly.intersect(frline, 2).getLength("PLANAR", "METERS")
                    del frcursor
            del fcursor

        # Get the total river length
        with arcpy.da.SearchCursor(rivers, ["SHAPE@"]) as rcursor:
            for r in rcursor:
                rline = r[0]
                river_length = river_length + rline.getLength("PLANAR", "METERS")
            del rcursor

        # Load the results into the dictionary
        results[ID] = f_area, b_area, fri_length, river_length, near[int(ID)][2]


        write('bFRI | ')

        arcpy.SelectLayerByAttribute_management(forest, "CLEAR_SELECTION")
        arcpy.SelectLayerByAttribute_management(rivers, "CLEAR_SELECTION")

        arcpy.Delete_management(b)
        arcpy.Delete_management(sc)
        arcpy.Delete_management(buffer_clip)

        stop = time.time()
        elapsed = (stop - start)/60
        elapsed = round(elapsed, 1)
        write('Loop ' + str(loopCounter) + ': ' + str(elapsed) + ' min\n')

##    except:
##        print "Site FAILED: " + str(ID)
##        siteFail.append(str(ID))

# Export the results as a CSV table
arcpy.CreateTable_management(gdb, "results_FRI")
arcpy.AddField_management("results_FRI", siteIDfield, "TEXT", "20")
arcpy.AddField_management("results_FRI", "ForestArea", "DOUBLE")
arcpy.AddField_management("results_FRI", "BufferArea", "DOUBLE")
arcpy.AddField_management("results_FRI", "ForestLength", "DOUBLE")
arcpy.AddField_management("results_FRI", "RiverLength", "DOUBLE")
arcpy.AddField_management("results_FRI", "NearDist", "DOUBLE")

fields = [siteIDfield, 'ForestArea', 'BufferArea', 'ForestLength', 'RiverLength', 'NearDist']
nrows = len(results) + 1
with arcpy.da.InsertCursor('results_FRI', fields) as cursor:
    for key, value in results.items():
        cursor.insertRow((key, value[0], value[1], value[2], value[3], value[4]))
del cursor

output = root + "\\" + "site_temp_bFRI.csv"
##arcpy.ExportXYv_stats('results_FRI', fields, "COMMA", output, "ADD_FIELD_NAMES")


#--first lets make a list of all of the fields in the table
fields = arcpy.ListFields("results_FRI")
field_names = [field.name for field in fields]

with open(output,'wb') as f:
    w = csv.writer(f)
    #--write all field names to the output file
    w.writerow(field_names)

    #--now we make the search cursor that will iterate through the rows of the table
    for row in arcpy.SearchCursor("results_FRI"):
        field_vals = [row.getValue(field.name) for field in fields]
        w.writerow(field_vals)
    del row

print "SCRIPT COMPLETE"

##solution = root + "\\" + "site_FRI.csv"
##
##with open(solution, 'wb') as csv_file:
##    writer = csv.writer(csv_file)
##    for key, value in results.items():
##       writer.writerow([key, value[0], value[1]])

### Example of working with geometries##
### Get the site geometries
##test = arcpy.CopyFeatures_management(sites, arcpy.Geometry())
##
### Print the site buffer areas
##for s in test:
##    x = s.buffer(100)
##    print(str(x.getArea("PLANAR", "meters")))
##
### map() and lambda() to extract 100m buffers by site:
##f = map(lambda n: n.buffer(100), test)
