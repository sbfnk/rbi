# Absolute Path

This function is used to convert relative file paths to absolute file
paths without checking if the file exists as
tools::file_as_absolute_path does

## Usage

``` r
absolute_path(filename, dirname)
```

## Arguments

- filename:

  name of a file, absolute or relative to a folder

- dirname:

  name of a folder where the file is supposed to be

## Value

a character string containing the absolute path
