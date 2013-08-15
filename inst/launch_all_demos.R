# Launch all demo files
# actually should be able to generate that automatically from the file demo/00Index.R
source(system.file(package="bi", "demo/import_data.R"))
source(system.file(package="bi", "demo/init_files.R"))
source(system.file(package="bi", "demo/PZ_filtering.R"))
source(system.file(package="bi", "demo/PZ_generate_dataset.R"))
source(system.file(package="bi", "demo/PZ_PMMH.R"))
source(system.file(package="bi", "demo/PZ_SMC2.R"))
