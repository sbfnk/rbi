#' @rdname libbi
#' @name libbi
#' @title LibBi Wrapper
#' @description
#' \code{libbi} allows to call \code{LibBi}.
#' Upon creating a new libbi object, the following arguments can be given.
#' Once the instance is created, \code{LibBi} can be run through the \code{\link[rbi]{sample}}, \code{\link[rbi]{filter}}, or \code{\link{optimise}}, or \code{\link{rewrite}}
#' methods. Note that \code{\link{libbi}} objects can be plotted using \code{\link{plot}} if the \code{rbi.helpers} package is loaded.
#'
#' @param model either a character vector giving the path to a model file (typically ending in ".bi"), or a \code{bi_model} object
#' @param path_to_libbi path to \code{LibBi} binary; by default it tries to locate the \code{libbi} binary
#' using the \code{which} Unix command, after having loaded "~/.bashrc" if present;
#' if unsuccessful it tries "~/PathToBiBin/libbi"; if unsuccessful again it fails.
#' @param dims any named dimensions, as list of character vectors
#' @param use_cache logical; whether to use the cache (default: true)
#' @param ... options passed to \code{\link{run.libbi}}
#' @return a \code{\link{libbi}} object
#' @examples
#' bi_object <- libbi(model = system.file(package="rbi", "PZ.bi"))
#' @seealso \code{\link{sample}}, \code{\link{filter}}, \code{\link{optimise}}, \code{\link{rewrite}}
#' @export
libbi <- function(model, path_to_libbi, dims, use_cache=TRUE, ...){
  libbi_dims <- list()
  if (!missing(dims)) {
    for (dim_name in names(dims))
    {
      libbi_dims[[dim_name]] <- factor(dims[[dim_name]])
    }
  }

  if (missing(path_to_libbi)) path_to_libbi <- character(0)

  if (missing(model)) model <- bi_model()

  new_obj <-
    structure(list(options=list(),
                   path_to_libbi=path_to_libbi,
                   model=model,
                   model_file_name=character(0),
                   working_folder=character(0),
                   dims=libbi_dims,
                   time_dim=character(0),
                   coord_dims=NULL,
                   thin=1,
                   command=character(0),
                   output_file_name=character(0),
                   log_file_name=character(0),
                   timestamp=.POSIXct(NA),
                   run_flag=FALSE,
                   error_flag=FALSE,
                   use_cache=use_cache,
                   supplement=NULL,
                   .gc_env=emptyenv(),
                   .cache=new.env(parent = emptyenv())), class="libbi")
  return(do.call(run, c(list(x=new_obj, client=character(0)), list(...))))
}

#' @export
run <- function(x, ...) UseMethod("run")
#' @rdname run
#' @name run
#' @title Using the LibBi wrapper to launch LibBi
#' @description
#' The method \code{run} launches \code{LibBi} with a particular set of command line #' arguments. Normally, this function would not be run by the user, but instead one of the client functions \code{\link{sample}}, \code{\link{filter}}, or \code{\link{optimise}}, or \code{\link{rewrite}}, which pass any options on to \code{run}. Note that any options specified here are stored in the \code{\link{libbi}} object and do not have to be specified again if another command is run on the object.
#'
#' @param x a \code{\link{libbi}} object
#' @param client client to pass to LibBi
#' @param proposal proposal distribution to use; either "model" (default: proposal distribution in the model) or "prior" (propose from the prior distribution)
#' @param model either a character vector giving the path to a model file (typically ending in ".bi"), or a \code{bi_model} object; by default, will use any model given in \code{x}
#' @param fix any variable to fix, as a named vector
#' @param options list of additional arguments to pass to the call to \code{LibBi}. Any arguments starting with `enable`/`disable` can be specified as boolean (e.g., `assert=TRUE`). Any `dry-` options can be specified with a `"dry"` argument, e.g., `dry="parse"`. Any options that would be specified with `with`/`without` can be specified as character vector to an option named `with`/`without`, respectively, e.g. with="transform-obs-to-state".
#' @param config path to a configuration file, containing multiple arguments
#' @param add_options deprecated, replaced by \code{options}
#' @param log_file_name path to a file to text file to report the output of \code{LibBi}
#' @param init initialisation of the model, either supplied as a list of values and/or data frames, or a (netcdf) file name, or a \code{\link{libbi}} object which has been run (in which case the output of that run is used as input). If the object given as \code{x} has been run before, it will be used here with \code{init-np} set to the last iteration of the previous run, unless \code{init} is given explicitly.
#' @param input input of the model, either supplied as a list of values and/or data frames, or a (netcdf) file name, or a \code{\link{libbi}} object which has been run (in which case the output of that run is used as input)
#' @param obs observations of the model, either supplied as a list of values and/or data frames, or a (netcdf) file name, or a \code{\link{libbi}} object which has been run (in which case the output of that run is used as observations)
#' @param time_dim The time dimension in any R objects that have been passed (\code{init}, \code{input}) and \code{obs}); if not given, will be guessed
#' @param coord_dims The coord dimension(s) in any \code{obs} R objects that have been passed; if not given, will be guessed
#' @param working_folder path to a folder from which to run \code{LibBi}; default to a temporary folder.
#' @param output_all logical; if set to TRUE, all parameters, states and observations will be saved; good for debugging
#' @param sample_obs logical; if set to TRUE, will sample observations
#' @param thin any thinning of MCMC chains (1 means all will be kept, 2 skips every other sample etc.); note that \code{LibBi} itself will write all data to the disk. Only when the results are read in with \code{\link{bi_read}} will thinning be applied.
#' @param chain logical; if set to TRUE and \code{x} has been run before, the previous output file will be used as \code{init} file, and \code{init-np} will be set to the last iteration of the previous run (unless target=="prediction"). This is useful for running inference chains.
#' @param seed Either a number (the seed to supply to \code{LibBi}), or a logical variable: TRUE if a seed is to be generated for \code{RBi}, FALSE if \code{LibBi} is to generate its own seed
#' @param ... any unrecognised options will be added to \code{options}
#' @seealso \code{\link{libbi}}
#' @examples
#' bi_object <- libbi(model = system.file(package="rbi", "PZ.bi"))
#' \dontrun{run(bi_object, options=list(client="sample", sample="smc2"))}
#' if (bi_object$run_flag) {
#'   bi_file_summary(bi_object$output_file_name)
#' }
#' @return a \code{\link{libbi}} object, except if \code{client} is 'rewrite',  in which case a \code{\link{bi_model}} object will be returned
#' @importFrom ncdf4 nc_open nc_close ncvar_rename
#' @importFrom stats runif
#' @export
run.libbi <-  function(x, client, proposal=c("model", "prior"), model, fix, options, config, add_options, log_file_name, init, input, obs, time_dim, coord_dims, working_folder, output_all, sample_obs, thin, chain=TRUE, seed=TRUE, ...){

  ## client options
  libbi_client_args <-
    list(sample = c("target", "sampler", "nsamples", "nmoves", "tmoves",
                    "sampler-resampler", "sample-ess-rel", "sample-stopper",
                    "sample-stopper-threshold", "sample-stopper-max",
                    "adapter", "adapter-scale", "adapter-ess-rel"),
         optimise = c("target", "optimiser", "simplex-size-real",
                      "stop-size", "stop-steps"),
         filter = c("start-time", "end-time", "noutputs",
                    "with-output-at-obs", "filter", "nparticles", "ess-rel",
                    "resampler", "nbridges", "stopper", "stopper-threshold",
                    "stopper-max", "stopper-block"),
         rewrite = c())

  all_client_args <- unique(unname(unlist(libbi_client_args)))
  ## both sample and optimise inherit from filter
  libbi_client_args[["sample"]] <-
    unique(c(libbi_client_args[["sample"]], libbi_client_args[["filter"]]))
  libbi_client_args[["optimise"]] <-
    unique(c(libbi_client_args[["optimise"]], libbi_client_args[["filter"]]))

  if (missing(sample_obs)) sample_obs <- FALSE
  if (missing(output_all)) output_all <- FALSE
  proposal <- match.arg(proposal)

  if (!missing(thin)) x$thin <- thin

  if (missing(options)){
    new_options <- list()
  } else {
    new_options <- option_list(options)
  }

  if (missing(config) || config == ""){
    config_file_options <- list()
  } else {
    config_str <- config
    if (substr(config_str, 1, 1) == "@") {
      config_str <- substr(config_str, 2, nchar(config_str))
    }
    if (file.exists(config_str)) {
      config_file_options <- paste(readLines(config_str), collapse = " ")
    } else {
      stop("Could not find config file ", config_str)
    }
  }

  ## get model
  if (!missing(model)) x$model <- model

  if (!("bi_model" %in% class(x$model))) {
      x$model <- bi_model(filename=x$model)
  }
  ## save model name, set again after all is done
  model_name <- get_name(x$model)

  if (!missing(working_folder)) {
    x$working_folder <- absolute_path(working_folder)
    if (!dir.exists(x$working_folder)) {
      dir.create(working_folder)
    }
  } else if (length(x$working_folder) == 0) {
    x$working_folder <- tempfile(pattern=paste(get_name(x$model)))
    dir.create(x$working_folder)
    ## make sure temporary folder gets deleted upon garbage collection
    x$.gc_env <- new.env() ## dummy environment
    x$.gc_env$folder <- x$working_folder
    reg.finalizer(x$.gc_env, function(env) {
      unlink(env$folder, recursive=TRUE)
    }, onexit=TRUE)
  }

  libbi_seed <- integer(0)
  if (is.logical(seed)) {
    if (seed == TRUE) {
      libbi_seed <- ceiling(runif(1, -1, .Machine$integer.max - 1))
    }
  } else {
    libbi_seed <- seed
  }
  if (length(libbi_seed) > 0) new_options[["seed"]] <- libbi_seed

  ## check if 'model-file' is contained in any options
  all_options <- option_list(getOption("libbi_args"), config_file_options, x$options, new_options, list(...))
  if ("model-file" %in% names(all_options)) {
    if (missing(model)) {
      x$model_file_name <- absolute_path(all_options[["model-file"]], getwd())
      x$model <- bi_model(x$model_file_name)
    } else {
      warning("'model-file' and 'model' options both provided. Will ignore 'model-file'.")
    }
  } else if (!is_empty(x$model)) {
    if (length(x$model_file_name) == 0 ||
        !all((x$model == bi_model(x$model_file_name))[-1])) {
      x$model_file_name <-
        tempfile(pattern=paste(get_name(x$model), "model", sep = "_"),
                 fileext=".bi",
                 tmpdir=absolute_path(x$working_folder))
      write_model(x)
    }
  }

  ## read file options: input, init, obs
  args <- match.call()
  file_types <- c("input", "init", "obs")
  file_args <- intersect(names(args), file_types)
  ## assign file args to global options
  for (arg in file_args) x$options[[arg]] <- get(arg)

  file_options <- list()

  if (x$run_flag && length(x$output_file_name) == 1 &&
      file.exists(x$output_file_name)) {
    init_file_given <-
      "init" %in% file_args || "init-file" %in% names(new_options)
    init_np_given <- "init-np" %in% names(new_options)
    init_given <- init_file_given || init_np_given
    if (missing(chain)) { ## if chain not specified, only chain if no init
      ## option is given
      chain <- !init_given
    }
    if (chain) {
      if (init_file_given) {
        warning("init file given and 'chain=TRUE'. Will ignore 'init' option. To use the 'init' option, set 'chain=FALSE'.")
      }
      if (init_np_given) {
        warning("'init-np' given as new option and 'chain=TRUE'. Will ignore 'init-np' option. To use the 'init-np' option, set 'chain=FALSE'")
      }
      if ("target" %in% names(all_options) &&
          all_options[["target"]] == "prediction") {
        read_init <- bi_read(x, type=c("param", "state"))
        np_dims <- bi_dim_len(x$output_file_name, "np")
        file_options[["nsamples"]] <- floor(np_dims / x$thin)
        if (x$thin > 1) {
          x$thin <- 1
        }
        if (length(read_init) > 0) {
          x$options[["init"]] <- read_init
        } else {
          x$options[["init"]] <- NULL
        }
      } else {
        ## TODO: check if initial parameters exist
        types <- "param"
        chain_init <- ("with-transform-initial-to-param" %in% names(all_options))
        if (chain_init) types <- c(types, "state")
        read_init <- bi_read(x, type=types, init.to.param=chain_init)
        ## only take last sample
        if (length(read_init) > 0) {
          x$options[["init"]] <- extract_sample(read_init, "last")
        } else {
          x$options[["init"]] <- NULL
        }
      }
      if (!is.null(x$options)) file_args <- union(file_args, "init")
    }
  }

  ## loop over global options that are file args
  for (file in file_args) {
    arg <- x$options[[file]]
    ## unset global option (we set the file option instead later)
    x$options[[file]] <- NULL
    if (class(arg) == "libbi") {
      if (!arg$run_flag) {
        stop("The libbi object for '", arg, "' should be run first (using sample, filter or optimise).")
      } else if (length(arg$output_file_name) == 0 || !file.exists(arg$output_file_name)) {
        stop("The libbi object for '", arg, "' does not contain an output file.")
      }
      file_options[[paste(file, "file", sep = "-")]] <-
        arg$output_file_name
      if (length(arg$time_dim) > 0) {
        x$time_dim <- arg$time_dim
      } else {
        x$time_dim <- "time"
      }
    } else if (is.list(arg)) {
      arg_file_name <-
        tempfile(pattern=paste(get_name(x$model), file, sep = "_"),
                 fileext=".nc",
                 tmpdir=absolute_path(x$working_folder))
      write_opts <- list(filename = arg_file_name,
                         variables = arg,
                         timed = TRUE)
      if (!missing(time_dim)) {
        x$time_dim <- time_dim
      }
      if (!missing(coord_dims)) {
        x$coord_dims <- coord_dims
      }
      if (file == "obs") ## guess coord for observation files
      {
        if (length(x$time_dim) == 0) {
          write_opts[["guess_time"]] <- TRUE
        }
        if (length(x$coord_dims) == 0) {
          write_opts[["guess_coord"]] <- TRUE
        } else {
          write_opts[["coord_dims"]] <- x$coord_dims
        }
      }
      if (length(x$time_dim) > 0) {
        write_opts[["time_dim"]] <- x$time_dim
      }
      write_opts[["dim_factors"]] <- x$dims
      file_dims <- do.call(bi_write, write_opts)
      x$dims[names(file_dims$dims)] <- file_dims$dims
      if (!is.null(file_dims$time_dim)) x$time_dim <- file_dims$time_dim
      if (file == "obs" && !is.null(file_dims$coord_dims)) x$coord_dims <- file_dims$coord_dims
      file_options[[paste(file, "file", sep = "-")]] <- arg_file_name
    } else if (is.character(arg)) {
      file_options[[paste(file, "file", sep = "-")]] <- arg
    } else if (is.null(arg)) {
      x$options[[paste(file, "file", sep = "-")]] <- arg
    } else {
      stop("'", file, "' must be a list, string or 'libbi' object, or NULL.")
    }
  }

  all_options <- option_list(getOption("libbi_args"), config_file_options,
                             x$options, new_options, file_options, list(...))

  if (length(client) > 0)
  {
    ## re-read options
    ## adjust options
    if (client != "rewrite") {
      ## clear cache
      x$.cache <- new.env(parent = emptyenv())
      ## check that model is not empty
      if (is_empty(x$model)) {
        stop("No model given.")
      }

      if (!("output-file" %in% names(all_options))) {
        x$output_file_name <- tempfile(pattern=paste(get_name(x$model), "output", sep = "_"),
                                       fileext=".nc",
                                       tmpdir=absolute_path(x$working_folder))
      } else {
        x$output_file_name <- absolute_path(all_options[["output-file"]], getwd())
      }
    }
    x$options <- all_options
    all_options[["output-file"]] <- x$output_file_name

    ## set obs-file to NULL if targeting prior or prediction
    obs_file_save <- x$options[["obs-file"]]
    if ("target" %in% names(all_options) &&
        all_options[["target"]] %in% c("prior", "join", "prediction")) {
      all_options[["obs-file"]] <- NULL
    }

    ## remove arguments of other clients
    retain_options <-
      setdiff(names(all_options),
                    setdiff(all_client_args, libbi_client_args[[client]]))
    all_options <- all_options[retain_options]

    run_model <- x$model
    run_model_modified <- FALSE

    if (output_all) {
      no_output_pattern <- "[[:space:]]*has_output[[:space:]]*=[[:space:]]*0[[:space:]]*"
      no_output <- grep(no_output_pattern, run_model)
      updated_lines <- sub(no_output_pattern, "", run_model[no_output])
      updated_lines <- gsub(",,", ",", updated_lines)
      updated_lines <- gsub("\\(,", "(", updated_lines)
      updated_lines <- gsub(",\\)", ")", updated_lines)
      updated_lines <- sub("()", "", updated_lines)
      run_model[no_output] <- updated_lines
      run_model_modified <- TRUE
    }

    if (proposal == "prior") {
      run_model <- propose_prior(run_model)
      run_model_modified <- TRUE
    }

    if (!missing(fix)) {
      run_model <- do.call(fix.bi_model, c(list(x=run_model), as.list(fix)))
      run_model_modified <- TRUE
    }

    if (sample_obs) {
      run_model <- obs_to_noise(run_model)
      run_model_modified <- TRUE
    }

    if (run_model_modified) {
      run_model_file_name <-
        tempfile(pattern=paste(get_name(run_model), "model", sep = "_"),
                 fileext=".bi",
                 tmpdir=absolute_path(x$working_folder))
      write_model(run_model, run_model_file_name)
      all_options[["model-file"]] <- run_model_file_name
    } else {
      all_options[["model-file"]] <- x$model_file_name
    }

    if (client == "rewrite") all_options <- all_options["model-file"]

    opt_string <- option_string(all_options)
    verbose <- ("verbose" %in% names(all_options) && all_options[["verbose"]] == TRUE)

    if (missing(log_file_name) && !verbose) {
      x$log_file_name <- tempfile(pattern="output", fileext=".txt",
                                  tmpdir=absolute_path(x$working_folder))
    } else if (!missing(log_file_name)) {
      x$log_file_name <- absolute_path(filename=log_file_name, dirname=getwd())
    }

    if (verbose || length(x$log_file_name) == 0) {
      log_redir_name <- ""
    } else {
      log_redir_name <- paste(">", x$log_file_name, "2>&1")
    }

    if (length(x$path_to_libbi) == 0) {
      if (is.null(getOption("path_to_libbi"))) {
        # Maybe the system knows where libbi is
        x$path_to_libbi <- Sys.which("libbi")
      } else {
        x$path_to_libbi <- getOption("path_to_libbi")
      }
      if (length(x$path_to_libbi) == 0){
        stop("Could not locate LibBi, please either provide the path to the libbi binary via the 'path_to_libbi' option, or set the PATH to contain the directory that contains the binary in ~/.Renviron or set it in your R session via options(path_to_libbi = \"insert_path_here\"). For instructions on how to install libbi, look at the RBi github page on https://github.com/libbi/rbi.")
      }
    }
    if (!grepl("libbi$", x$path_to_libbi)) {
      x$path_to_libbi <- paste0(x$path_to_libbi, "/libbi")
    }
    if (!file.exists(x$path_to_libbi)) {
      stop("Could not find libbi executable ", x$path_to_libbi)
    }
    base_command_string <- paste(x$path_to_libbi, client)

    cdcommand <- paste("cd", x$working_folder)
    x$command <- paste(c(cdcommand, paste(base_command_string, opt_string)), collapse=";")
    if (verbose) print("Launching LibBi with the following commands:")
    if (verbose)
      print(paste(c(x$command, log_redir_name), sep = "\n"))
    runcommand <- paste(x$command, log_redir_name)
    ret <- system(runcommand)
    if (ret > 0) {
      if (!verbose) {
        writeLines(readLines(x$log_file_name))
      }
      warning("LibBi terminated with an error.")
      x$error_flag <- TRUE
      return(x)
    }
    x$error_flag <- FALSE
    if (verbose) print("... LibBi has finished!")

    if (sample_obs && file.exists(x$output_file_name)) {
      nc <- nc_open(x$output_file_name, write=TRUE)
      for (obs_name in var_names(run_model, "obs")) {
        ncvar_rename(nc, paste0("__sample_", obs_name), obs_name)
      }
      nc_close(nc)
    }

    ## recover saved obs file name
    x$options[["obs-file"]] <- obs_file_save

    if (client == "rewrite") {
      model_lines <- readLines(x$log_file_name)
      first_model_line <-
        min(grep("^[[:space:]]*model[[:space:]]*", model_lines))
      model_lines <- model_lines[first_model_line:length(model_lines)]
      x <- bi_model(lines=model_lines)
    } else {
      x$run_flag <- TRUE
      if (x$run_flag && file.exists(x$output_file_name)) {
        x$timestamp <- file.mtime(x$output_file_name)
      }
      ## set model name back to original name
      set_name(x$model, model_name)
    }
  } else {
    ## if run from the constructor, just add all the options
    x$options <- all_options
  }
  return(x)
}

#' @export
sample <- function(x, ...) UseMethod("sample")
#' @name sample
#' @rdname sample
#' @title Using the LibBi wrapper to sample
#' @description
#' The method \code{sample} launches \code{libbi} to sample from a (prior, posterior or joint) distribution. See the options to \code{\link{run.libbi}} for how to specify the various components of sampling with LibBi, and the LibBi manual for all options that can be passed when the client is \code{sample}.
#'
#' If \code{x} is given as a 'bi_model', a \code{\link{libbi}} object will be created from the model
#' For the help page of the base R \code{sample} function, see \code{\link[base]{sample}}.
#' @param x a \code{\link{libbi} or \link{bi_model}} object, or the name of a file containing the model
#' @param ... options to be passed to \code{\link{run.libbi}}
#' @return a \code{\link{libbi}} object
#' @export
sample.libbi <- function(x, ...){
  run.libbi(x, client="sample", ...)
}
#' @rdname sample
#' @name sample
#' @export
sample.bi_model <- function(x, ...){
  run.libbi(libbi(model=x), client="sample", ...)
}
#' @export
sample.default <- function(x, ...){
  base::sample(x, ...)
}

#' @export
filter <- function(x, ...) UseMethod("filter")
#' @rdname filter
#' @name filter
#' @title Using the LibBi wrapper to filter
#' @description
#' The method \code{filter} launches \code{libbi} to filter state trajectories. See the options to \code{\link{run.libbi}} for how to specify the various components of sampling with LibBi, and the LibBi manual for all options that can be passed when the client is \code{filter}.
#'
#' If \code{x} is given as a 'bi_model', a \code{\link{libbi}} object will be created from the model
#' For the help page of the base R \code{filter} function, see \code{\link[stats]{filter}}.
#' @param x a \code{\link{libbi} or \link{bi_model}} object, or the name of a file containing the model
#' @param ... options to be passed to \code{\link{run.libbi}}
#' @return a \code{\link{libbi}} object
#' @export
filter.libbi <- function(x, ...){
  run.libbi(x, client="filter", ...)
}
#' @rdname filter
#' @name filter
#' @export
filter.bi_model <- function(x, ...){
  run.libbi(libbi(x), client="filter", ...)
}
#' @export
filter.default <- function(x, ...){
  stats::filter(x, ...)
}

#' @export
optimise <- function(x, ...) UseMethod("optimise")
#' @rdname optimise
#' @name optimise
#' @title Using the LibBi wrapper to optimise
#' @description
#' The method \code{optimise} launches \code{libbi} to optimise the parameters with respect to the likelihood or posterior distribution. See the options to \code{\link{run.libbi}} for how to specify the various components of sampling with LibBi, and the LibBi manual for all options that can be passed when the client is \code{optimise}.
#'
#' If \code{x} is given as a 'bi_model', a \code{\link{libbi}} object will be created from the model
#' For the help page of the base R \code{optimise} function, see \code{\link[stats]{optimise}}.
#' @param x a \code{\link{libbi} or \link{bi_model}} object, or the name of a file containing the model
#' @param ... options to be passed to \code{\link{run.libbi}}
#' @return a \code{\link{libbi}} object
#' @export
optimise.libbi <- function(x, ...){
  run.libbi(x, client="optimise", ...)
}
#' @rdname optimise
#' @name optimise
#' @export
optimise.bi_model <- function(x, ...){
  run.libbi(libbi(x), client="optimise", ...)
}
#' @export
optimise.default <- function(x, ...){
  stats::optimise(x, ...)
}

#' @export
rewrite <- function(x, ...) UseMethod("rewrite")
#' @rdname rewrite
#' @name rewrite
#' @title Using the LibBi wrapper to rewrite
#' @description
#' The method \code{rewrite} launches \code{LibBi} to rewrite a model to inspect its internal representation in \code{LibBi}
#'
#' If \code{x} is given as a 'bi_model', a \code{\link{libbi}} object will be created from the model
#' @param x a \code{\link{libbi} or \link{bi_model}} object, or the name of a file containing the model
#' @param ... options to be passed to \code{\link{run.libbi}}
#' @return a \code{\link{bi_model}} object
#' @export
rewrite.libbi <- function(x, ...){
  run.libbi(x, client="rewrite", ...)
}
#' @rdname rewrite
#' @name rewrite
#' @export
rewrite.bi_model <- function(x, ...){
  run.libbi(libbi(x), client="rewrite", ...)
}

#' @export
attach_file <- function(x, ...) UseMethod("attach_file")
#' @name attach_file
#' @rdname attach_file
#' @title Attach a new file to a \code{\link{libbi}} object
#' @description
#' Adds an (output, obs, etc.) file to a \code{\link{libbi}} object. This is useful to recreate a \code{\link{libbi}} object from the model and output files of a previous run
#' @param x a \code{\link{libbi}} object
#' @param file the type of the file to attach, one of "output", "obs", "input" or "init"
#' @param data name of the file to attach, or a list of data frames that contain the outputs
#' @param force attach the file even if one like this already exists in the libbi object
#' @param ... ignored
#' @inheritParams bi_open
#' @examples
#' bi <- libbi(model = system.file(package="rbi", "PZ.bi"))
#' example_output_file <- system.file(package="rbi", "example_output.nc")
#' bi <- attach_file(bi, "output", example_output_file)
#' @export
attach_file.libbi <- function(x, file, data, force=FALSE, ...){
  if (file == "output") {
    target_file_name <- x[["output_file_name"]]
  } else {
    target_file_name <- x[["options"]][[paste0(file, "-name")]]
  }
  if (length(target_file_name) > 0 &&
      nchar(target_file_name) > 0 &&
      !force) {
    stop("libbi object already contains ", file, " file; if you want to overwrite this,  use `force=TRUE`'")
  }
  if (is.character(data)) {
    target_file_name <- data
  } else {
    target_file_name <-
      tempfile(pattern=paste(get_name(x$model), file, sep = "_"),
               fileext=".nc", tmpdir=absolute_path(x$working_folder))
    write_opts <- list(filename = target_file_name, variables = data)
    if (file == "obs" && "coord_dims" %in% names(x)) {
      write_opts[["coord_dims"]] <- x$coord_dims
    }
    if ("time_dim" %in% names(x)) {
      write_opts[["time_dim"]] <- x$time_dim
    }
    do.call(bi_write, write_opts)
  }
  if (file == "output") {
    x$output_file_name <- target_file_name
    x$run_flag <- TRUE
    x$timestamp <- file.mtime(x$output_file_name)
    x$.cache <- new.env(parent = emptyenv())
    x$thin <- 1 ## output file will already be thinned
  } else {
    if (is.null(x$options)) x$options <- list()
    x$options[[paste0(file, "-file")]] <- target_file_name
  }
  return(x)
}

#' @export
save_libbi <- function(x, ...) UseMethod("save_libbi")
#' @name save_libbi
#' @rdname save_libbi
#' @title Write results of a \code{LibBi} run to an RDS file
#' @description
#' This saves all options, files and outputs of a \code{LibBi} run to an RDS file specified
#'
#' @param x a \code{\link{libbi}} object
#' @param filename name of the RDS file to save to
#' @param supplement any supplementary data to save
#' @param ... any options to \code{\link{saveRDS}}
#' @export
save_libbi.libbi <- function(x, filename, supplement, ...) {
  if (missing(filename)) {
    stop("Need to specify a file name")
  }
  assert_output(x)

  save_obj <- list(model=x$model,
                   dims=x$dims,
                   time_dim=x$time_dim,
                   coord_dims=x$coord_dims,
                   thin=1,
                   supplement=x$supplement,
                   output=bi_read(x))

  options <- x$options

  for (file_type in c("init", "input", "obs")) {
    file_option <- paste(file_type, "file", sep="-")
    if (file_option %in% names(x$options)) {
      save_obj[[file_type]] <- bi_read(x, file=file_type)
      options[[file_option]] <- NULL
    }
  }

  save_obj[["options"]] <- options

  if (!missing(supplement)) save_obj[["supplement"]] <- supplement

  saveRDS(save_obj, filename, ...)
}

#' @export
read_libbi <- function(x, ...) UseMethod("read_libbi")
#' @rdname read_libbi
#' @name read_libbi
#' @title Read results of a \code{LibBi} run from an RDS file. This completely reconstructs the saved \code{LibBi} object
#' @description
#' This reads all options, files and outputs of a \code{LibBi} run to an RDS file specified
#'
#' @param file name of the RDS file to read
#' @param ... any extra options to pass to \code{\link{read_libbi}} when creating the new object
#' @return a \code{\link{libbi}} object
read_libbi <- function(file, ...) {
  if (missing(file)) {
    stop("Need to specify a file to read")
  }

  read_obj <- readRDS(file)

  libbi_options <- list(...)

  pass_options <- c("model", "dims", "time_dim", "coord_dims", "options",
                    "thin", "init", "input", "obs")

  for (option in pass_options) {
    if (!(option %in% names(libbi_options)) &&
        option %in% names(read_obj)) {
      libbi_options[[option]] <- read_obj[[option]]
    }
  }

  output_file_name <-
    tempfile(pattern=paste(get_name(read_obj$model), "output", sep = "_"),
             fileext=".nc")
  bi_write(output_file_name, read_obj$output,
           time_dim=libbi_options$time_dim)

  new_obj <- do.call(libbi, libbi_options)
  new_obj <- attach_file(new_obj, file="output", data=output_file_name)
  new_obj$supplement <- read_obj$supplement

  return(new_obj)
}

#' @export
#' @name print
#' @title Print information about a \code{\link{libbi}} object
#' @description
#' This prints the model name, basic information such as number of iterations
#'   and timesteps run, as well as a list of variables.
#' @param x a \code{\link{libbi}} object
#' @param verbose logical; if TRUE, locations of files and working folder should be printed
#' @param ... ignored
#' @rdname print
print.libbi <- function(x, verbose=FALSE, ...){
  cat("Wrapper around LibBi\n")
  if (verbose) {
    cat("* path to working folder:", x$working_folder, "\n")
    cat("* path to model file:", x$model_file_name, "\n")
    if (length(x$output_file_name) > 0) {
      cat("* path to output_file:", x$output_file_name, "\n")
    }
  }
  cat("======================\n")
  cat("Model: ", get_name(x$model), "\n")
  if (x$run_flag) {
    assert_output(x)
    contents <- bi_contents(x$output_file_name)

    if ("clock" %in% contents) {
      clock <- bi_read(x, "clock")[["clock"]]
      cat("Run time: ", clock/1e6, " seconds\n")
    }
    states <- intersect(contents, var_names(x$model, "state"))
    noises <- intersect(contents, var_names(x$model, "noise"))
    params <- intersect(contents, var_names(x$model, "param"))
    obs <- intersect(contents, var_names(x$model, "obs"))
    niterations <- bi_dim_len(x$output_file_name, "np")
    if (niterations > 0) cat("Number of samples: ", niterations, "\n")
    if (length(states) > 0) cat("State trajectories recorded: ", paste(states, sep=", "), "\n")
    if (length(noises) > 0) cat("Noise trajectories recorded: ", paste(noises, sep=", "), "\n")
    if (length(obs) > 0) cat("Observation trajectories recorded: ", paste(obs, sep=", "), "\n")
    if (length(params) > 0) cat("Parameters recorded: ", paste(params), "\n")
  } else {
    if (x$error_flag) {
      cat("* LibBi terminated with an error\n")
    } else {
      cat("* LibBi has not been run yet\n")
    }
  }
}

#' @name summary
#' @rdname summary
#' @title Print summary information about a \code{\link{libbi}} object
#' @description
#' This reads in the output file of the \code{\link{libbi}} object (which has been run before) and prints summary information of parameters
#' @param object a \code{\link{libbi}} object
#' @param ... ignored
#' @export
summary.libbi <- function(object, ...){
  params <- c(bi_read(object, type="param"))
  summary_table <- t(vapply(params, function(object) {
    if (is.data.frame(object))
    {
      summary(object$value)
    } else
    {
      summary(object)
    }
  }, rep(0, 6)))
  return(summary_table)
}

#' @export
assert_output <- function(x, ...) UseMethod("assert_output")
#' @name assert_output
#' @rdname assert_output
#' @title Check that a LibBi wrapper has valid output
#' @description
#' This checks that the \code{\link{libbi}} object given has been run (via \code{\link{sample}}, \code{\link{filter}} or \code{\link{optimize}})) and the output file has not been modified since.
#' @param x a \code{\link{libbi}} object
#' @param ... ignored
#' @keywords internal
assert_output.libbi <- function(x, ...)
{
    if (!x$run_flag) {
      stop("The libbi object must be run first (using sample, filter or optimise).")
    }
    if (length(x$output_file_name) == 0 || !file.exists(x$output_file_name)) {
      stop("The libbi object for does not contain an output file.")
    }
    if (x$timestamp < file.mtime(x$output_file_name)) {
      stop("Output file ", x$output_file_name, " has been modified since LibBi was run.")
    }
}

#' @name predict
#' @rdname predict
#' @title Using the LibBi wrapper to predict
#' @description
#' The method \code{predict} is an alias for \code{sample(target="prediction")}. Usually, an \code{init} object or file should be given containing posterior samples.
#'
#' For the help page of the base R \code{optimise} function, see \code{\link[stats]{optimise}}.
#' @export
#' @param object a \code{\link{libbi}} object
#' @param ... ignored
predict.libbi <- function(object, ...) {
  sample(object, target="prediction", ...)
}

#' @export
join <- function(x, ...) UseMethod("join")
#' @name join
#' @rdname join
#' @title Join multiple \code{\link{libbi}} objects
#' @description
#' This function can be used to join multiple \code{\link{libbi}} objects into one (e.g., parallel MCMC runs into one long change)
#' @export
#' @param x a \code{\link{libbi}} object
#' @param ... ignored
join.libbi <- function(x, ...) {
  files_to_join <- list(...)
  if (missing(x) && length(files_to_join) > 0)
  {
    x <- files_to_join[[1]]
    files_to_join[[1]] <- NULL
  }
  output <- bi_read(x)
  for (to_join in files_to_join) {
    join_output <- bi_read(to_join)
    for (var in intersect(names(output), names(join_output))) {
      if (var=="clock") {
        output$clock <- output$clock + join_output$clock
      } else if (is.data.frame(output[[var]]) && is.data.frame(join_output[[var]]) &&
                 "np" %in% colnames(output[[var]]) && "np" %in% colnames(join_output[[var]])) {
        join_output[[var]]$np <- join_output[[var]]$np + max(output[[var]]$np) + 1
        output[[var]] <- rbind(output[[var]], join_output[[var]])
      }
    }
    for (var in setdiff(names(output), names(join_output))) {
      output[[var]] <- NULL
    }
  }
  attach_file(x, file="output", output, force=TRUE)
}

#' @rdname logLik
#' @name logLik
#' @title Using the LibBi wrapper to logLik
#' @description
#' The method \code{logLik} extracts the log-likelihood of a \code{libbi} object. This can be done, for example, after a call to \code{\link[rbi]{sample}} to inspect the chain log-likelihoods.
#'
#' For the help page of the base R \code{logLik} function, see \code{\link[stats]{logLik}}.
#' @param object a \code{\link{libbi}} object
#' @param ... options to be passed to \code{\link{run.libbi}}
#' @return a vector of log-likelihood
#' @export
logLik.libbi <- function(object, ...){
  assert_output(object)
  res <- bi_read(object)
  return(res$loglikelihood$value)
}
