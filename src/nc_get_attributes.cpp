#include <netcdfcpp.h>
#include <string>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
// [[Rcpp::export]]
List nc_get_attributes_from_path(std::string path) {
//  string path = "/home/pierre/workspace/pz/results/sample.nc";
  NcFile ncfile(path.c_str(), NcFile::ReadOnly);
  int natts = ncfile.num_atts();
  StringVector attributenames(natts);
  StringVector attributevalues(natts);
  
  for (int i = 0; i < natts; i++){
    NcAtt* attribute = ncfile.get_att(i);
    attributenames(i) = (string) attribute->name();
    attributevalues(i) = (string) attribute->as_string(0);
  }
  return List::create(Named("names") = attributenames, Named("values") = attributevalues);
}
