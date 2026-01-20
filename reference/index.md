# Package index

## Run LibBi and analyse results

### LibBi operations

Functions for running LibBi.

- [`filter(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/filter.md)
  [`filter(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/filter.md)
  : Using the LibBi wrapper to filter
- [`generate_dataset()`](https://sbfnk.github.io/rbi/reference/generate_dataset.md)
  : Generate Dataset
- [`optimise(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/optimise.md)
  [`optimise(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/optimise.md)
  : Using the LibBi wrapper to optimise
- [`libbi()`](https://sbfnk.github.io/rbi/reference/libbi.md) : LibBi
  Wrapper
- [`predict(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/predict.md)
  : Using the LibBi wrapper to predict
- [`run(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/run.md) :
  Using the LibBi wrapper to launch LibBi
- [`sample(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/sample.md)
  [`sample(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/sample.md)
  : Using the LibBi wrapper to sample
- [`sample_obs()`](https://sbfnk.github.io/rbi/reference/sample_obs.md)
  : Sample observations from a LibBi model that has been run
- [`simulate(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/simulate.md)
  [`simulate(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/simulate.md)
  : Using the LibBi wrapper to simulate

### Read and write LibBi inputs/outputs

Functions for interacting with the NetCDF files used by LibBi.

- [`bi_contents()`](https://sbfnk.github.io/rbi/reference/bi_contents.md)
  : Bi contents
- [`bi_file_summary()`](https://sbfnk.github.io/rbi/reference/bi_file_summary.md)
  : NetCDF File Summary
- [`bi_read()`](https://sbfnk.github.io/rbi/reference/bi_read.md) : Bi
  Read
- [`bi_write()`](https://sbfnk.github.io/rbi/reference/bi_write.md) :
  Create (e.g., init or observation) files for LibBi

### Analyse LibBi outputs

Functions for analysing LibBi results.

- [`extract_sample()`](https://sbfnk.github.io/rbi/reference/extract_sample.md)
  :

  Extract a sample from a `LibBi` run.

- [`get_traces()`](https://sbfnk.github.io/rbi/reference/get_traces.md)
  : Get the parameter traces

- [`summary(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/summary.md)
  :

  Print summary information about a `libbi` object

- [`logLik(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/logLik.md)
  : Using the LibBi wrapper to logLik

### Manipulate LibBi inputs/outputs

Functions for updating and saving LibBi objects.

- [`attach_data(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/attach_data.md)
  :

  Attach a new file or data set to a `libbi` object

- [`join(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/join.md)
  :

  Join multiple `libbi` objects

- [`read_libbi()`](https://sbfnk.github.io/rbi/reference/read_libbi.md)
  :

  Read results of a `LibBi` run from an RDS file or from a folder. This
  completely reconstructs the saved `LibBi` object

- [`save_libbi(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/save_libbi.md)
  :

  Write results of a `LibBi` run to an RDS file

- [`update(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/update.md)
  : Update a libbi object

## Construct, show and manipulate LibBi models

### Create a model

Function for creating a LibBi model.

- [`bi_model()`](https://sbfnk.github.io/rbi/reference/bi_model.md) : Bi
  Model

### Show models

Functions for getting information from LibBi models.

- [`get_block(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/get_block.md)
  : Get the contents of a block in a LibBi model
- [`get_const()`](https://sbfnk.github.io/rbi/reference/get_const.md) :
  Get constants in a LibBi model
- [`get_dims()`](https://sbfnk.github.io/rbi/reference/get_dims.md) :
  Get dimensions in a LibBi model
- [`get_name(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/get_name.md)
  : Get the name of a bi model
- [`rewrite(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/rewrite.md)
  [`rewrite(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/rewrite.md)
  : Using the LibBi wrapper to rewrite
- [`var_names()`](https://sbfnk.github.io/rbi/reference/var_names.md) :
  Get variable names in a LibBi model

### Manipulate models

Functions for manipulating LibBi models.

- [`add_block(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/add_block.md)
  : Add a block to a LibBi model

- [`enable_outputs()`](https://sbfnk.github.io/rbi/reference/enable_outputs.md)
  :

  Enable outputting variables in a `bi_model`

- [`fix(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/fix.md)
  : Fix noise term, state or parameter of a libbi model

- [`insert_lines(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/insert_lines.md)
  : Insert lines in a LibBi model

- [`remove_lines(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/remove_lines.md)
  : Remove line(s) and/or block(s) in a libbi model

- [`remove_vars()`](https://sbfnk.github.io/rbi/reference/remove_vars.md)
  : Remove variables

- [`replace_all(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/replace_all.md)
  : Replace all instances of a string with another in a model

- [`set_name(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/set_name.md)
  : Set the name of a bi model

- [`write_model(`*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/write_model.md)
  [`write_model(`*`<libbi>`*`)`](https://sbfnk.github.io/rbi/reference/write_model.md)
  : Writes a bi model to a file.

### Model operators

Operators for LibBi models

- [`` `==`( ``*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/Equals.bi_model.md)
  : Check if two models are equal
- [`` `[`( ``*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/Extract.bi_model.md)
  : Subset model lines
- [`` `[<-`( ``*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/Extract_assign.bi_model.md)
  : Subset and replace model lines
- [`` `!=`( ``*`<bi_model>`*`)`](https://sbfnk.github.io/rbi/reference/Unequals.bi_model.md)
  : Check if two models are unequal

## Other functions

### Utility functions

Useful additional functions.

- [`flatten()`](https://sbfnk.github.io/rbi/reference/flatten.md) :

  Flatten list of data frames This function takes a list of data frames
  (such as, for example, returned by `bi_read`) and converts it to a
  flat data frame

- [`print_log()`](https://sbfnk.github.io/rbi/reference/print_log.md) :

  Print the log file a `libbi` object

### Deprecated functions

To be removed in the next version of rbi.

- [`bi_generate_dataset()`](https://sbfnk.github.io/rbi/reference/bi_generate_dataset.md)
  : Bi Generate Dataset
