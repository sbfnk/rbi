bi_create_parameters <- function(filename, parameters){
  parameterlengths <- unlist(lapply(parameters, function(x) length(x)))
  if (!(all(parameterlengths == parameterlengths[1]))){
    stop("all parameter vectors must be of the same length")
  }
  parameterlength <- parameterlengths[1]
  if (parameterlength == 1){
    parameterlength <- 2
    for (name in names(parameters)){
      parameters[[name]] <- rep(parameters[name][1], 2)
    }
    # because can't create a vector with only one value with ncdf
  }
  np <- dim.def.ncdf(name="np", units="", vals=1:parameterlength, create_dimvar=FALSE)
  variables  <- list()
  for (name in names(parameters)){
    variables[[length(variables) + 1]] <- var.def.ncdf(name, "", np, -1,  prec="double")
  }
  
  # create NetCDF file
  nco <- create.ncdf(filename, variables)
  for (name in names(parameters)){
    # fill in the variables with the values
    put.var.ncdf(nco, name, parameters[[name]], start = 1)
  }
  close.ncdf(nco)
}