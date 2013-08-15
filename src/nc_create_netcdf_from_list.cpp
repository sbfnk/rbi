#include <netcdfcpp.h>
#include <string>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
// [[Rcpp::export]]
void nc_create_netcdf_from_list_(std::string filename, List variables) {
  NcFile ncfile(filename.c_str(), NcFile::Replace);
  if (!ncfile.is_valid()){
    cerr << "Couldn't open file!\n";
  }
  StringVector variable_names = variables.names();
  StringVector dimension_names;
  int nvariables = variable_names.length();
  std::string s = "dimension";
  int last_dim_size = 0;
  for (unsigned int ivar = 0; ivar < nvariables; ivar ++){
    List variable = variables[ivar];
    std::string dim_name = as<std::string>(variable["dimension"]);
    std::vector<double> values = as<std::vector<double> >(variable["values"]);
    dimension_names.push_back(dim_name);
    dimension_names = unique(dimension_names);
    NcDim* dim;
    if (dimension_names.size() > last_dim_size){
      dim = ncfile.add_dim(dim_name.c_str(), values.size());
    } else {
      dim = ncfile.get_dim(dim_name.c_str());
    }
    last_dim_size = dimension_names.size();
    NcVar *ncvariable = ncfile.add_var(as<std::string>(variable_names[ivar]).c_str(), ncDouble, dim);
    double* v = new double[values.size()];
    for (unsigned int index = 0; index < values.size(); index ++){
      v[index] = values[index];
    }
    ncvariable->put(v, values.size());
    delete[] v;
  }
}
