scenario_name = "base"
subpath =  "base"
scaled = F

source("c:/code/matsimScaling/siloAnalysis/readZonesAndRegions.R")
source("silo/results_reader.R")
source("silo/analyze_sp.R")
source("silo/spatial_reader.R")



output_path = "c:/projects/SILO/relocation/scenarios/"
read_results(output_path, subpath, scenario_name, scaled)

analyze_sp(output_path, subpath, scenario_name, scaled, F)

output_path = "c:/projects/SILO/relocation/scenarios/regional/"
regions = c("München-Landeshauptstadt","Augsburg","Eichstätt","Dachau")
read_spatial(output_path, subpath, scenario_name, regions, scaled)


