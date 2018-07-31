scenario_name = "5e_full"
subpath =  "5e"

source("c:/code/matsimScaling/siloAnalysis/readZonesAndRegions.R")
source("silo/results_reader.R")
source("silo/analyze_sp.R")
source("silo/spatial_reader.R")



output_path = "c:/projects/SILO/relocation/scenarios/"
read_results(output_path, subpath, scenario_name)
analyze_sp(output_path, subpath, scenario_name)

output_path = "c:/projects/SILO/relocation/scenarios/regional/"
regions = c("München-Landeshauptstadt","Augsburg","Eichstätt","Dachau")
read_spatial(output_path, subpath, scenario_name, regions)


