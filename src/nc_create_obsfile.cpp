#include <netcdfcpp.h>
#include <string>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
// [[Rcpp::export]]
void nc_create_obs_file_(std::string filename, std::string dimension_name, List variables) {
  NcFile ncfile(filename.c_str(), NcFile::Replace);
  if (!ncfile.is_valid()){
    cerr << "Couldn't open file!\n";
  }
  StringVector variable_names = variables.names();
  int nvariables = variables.length();
  int variablelength = as<NumericVector>(variables[0]).length();
  NcDim* nr = ncfile.add_dim(dimension_name.c_str(), variablelength);
  double* v = new double[variablelength];
  for (unsigned int ivar = 0; ivar < nvariables; ivar ++){
    NcVar *variable = ncfile.add_var(variable_names[ivar], ncDouble, nr);
    for (unsigned int index = 0; index < variablelength; index ++){
      v[index] = as<NumericVector>(variables[ivar])[index];
    }
    variable->put(v, variablelength);
  }  
  delete[] v;
}
