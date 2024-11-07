# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

#' Get the package identifier for the user config location
#'
#' Retrieve an identifier to use as a OS-specific directory name when creating
#' the package config. This will be used for creating an OS-specific path for
#' storing user configuration files in a user's home directory.
#'
#' @return A character string: The package identifier for configuration
#'   purposes.
#' @export
#'
#' @examples
#' get_cfg_pkg_id
#'
get_cfg_pkg_id <- function() {
  "R-dimex"
}

#' Get the package config directory
#'
#' Retrieve the package config directory which is the directory for storing a
#' config template. If there is no pre-existing user config, this template gets
#' copied to the user config by [write_user_cfg()].
#'
#' @param template_dir_name A character string: the name of the config template
#'   sub-directory or NULL for the default. Default: 'config'
#'
#' @return A character string: the path to the package config template
#'   directory.
#' @export
#'
#' @seealso [write_user_cfg()]
#'
#' @examples
#' get_pkg_cfg_dir()
#' \dontrun{
#' get_pkg_cfg_dir("cfg")
#' }
#'
get_pkg_cfg_dir <- function(template_dir_name = NULL) {

  template_dir_name <- template_dir_name %||% "config"

  cfg_dir <- tryCatch({

    system.file(template_dir_name, package = "dimex", mustWork = TRUE)

  }, error = function(e) {
    cli::cli_abort(c(
      "Cannot access package config directory",
      # Note: In a dev environment, the config dir should be a sub-dir of 'inst'
      "i" = glue::glue("Package config directory '{template_dir_name}' must",
                       " exist at the package level."),
      "x" = "Error message received: \"{e[['message']]}\""
    ))
  })

  cfg_dir
}

#' Retrieve the user-specific config dir
#'
#' @param cfg_dir A character string specifying the config directory, or NULL
#'   for the default. Default: An OS-specific path for user configuration files
#'   in the user's home directory.
#'
#' @return A character string: the supplied config dir or the default config
#'   dir if none was provided.
#' @export
#'
#' @examples
#' get_user_cfg_dir()
#' get_user_cfg_dir(file.path("config"))
#'
get_user_cfg_dir <- function(cfg_dir = NULL) {
  # If cfg_dir is not provided, retrieve an OS-specific config path in the
  # user's home directory
  cfg_dir %||% rappdirs::user_config_dir(get_cfg_pkg_id())
}

#' Retrieve the user-specific config file name
#'
#' @param cfg_name A character string specifying the config file name, or NULL
#'   for the default. Default: 'config.yml'
#'
#' @return A character string: the supplied config name or the default config
#'   name if none was provided.
#' @export
#'
#' @examples
#' get_user_cfg_name()
#' get_user_cfg_name("conf.yml")
#'
get_user_cfg_name <- function(cfg_name = NULL) {
  cfg_name %||% "config.yml"
}

#' Retrieve the config file path
#'
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#'
#' @return A character string: the path to the config file, constructed from
#'   `cfg_dir` and `cfg_name` or defaults if these are not suplied.
#' @export
#'
#' @examples
#' get_user_cfg_path()
#' get_user_cfg_path(file.path("conf"), "configuration.yml")
#'
get_user_cfg_path <- function(cfg_dir = NULL, cfg_name = NULL) {
  file.path(get_user_cfg_dir(cfg_dir), get_user_cfg_name(cfg_name))
}

#' Write the user-specific config to file
#'
#' Write the user-specific configuration to file. If no config is supplied, this
#' function can create a fresh config file by copying the template to the
#' appropriate location.
#'
#' @inheritParams write_cfg_template
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams get_pkg_cfg_dir
#' @inheritParams get_cfg_template_name
#' @param overwrite A boolean to indicate whether an existing config file
#'   should be overwritten. Default: FALSE
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' write_user_cfg()
#' write_user_cfg(overwrite = TRUE)
#'
#' # Override a value in the config template before saving
#' cfg <- generate_cfg_template()
#' cfg <- modifyList(cfg, list(dat = list(raw = list(base = "Data-dir"))))
#' write_user_cfg(cfg)
#' }
write_user_cfg <- function(cfg = NULL, cfg_dir = NULL, cfg_name = NULL,
                           template_dir_name = NULL, template_name = NULL,
                           overwrite = NULL) {

  cfg_dir <- get_user_cfg_dir(cfg_dir)

  overwrite <- overwrite %||% FALSE

  cfg_path <- get_user_cfg_path(cfg_dir, cfg_name)

  if (!dir.exists(cfg_dir)) {
    dir.create(cfg_dir, recursive = TRUE)
  }

  if (file.exists(cfg_path) && !overwrite) {
    cli::cli_abort(c(
      "Config file already exists",
      "i" = "Config exists at '{cfg_path}'",
      "i" = "Set {.var overwrite} to TRUE to overwrite the existing config",
      "i" = "E.g. 'write_user_cfg(config, overwrite = TRUE)'.",
      # TODO: When a user config conversion func is available, update this:
      "i" = "*Note* NB: Any local changes will need to be *manually updated*.",
      "x" = "Cannot replace config since {.var overwrite} is FALSE."
    ))
  }

  if (!is.null(cfg)) {
    # Write the supplied config to file (note that yaml::write_yaml will
    # overwrite an existing file without warning; however, that is okay at this
    # point as we have already checked that if the file exists then the user
    # must have set 'overwrite = TRUE')
    yaml::write_yaml(cfg, cfg_path)
  } else {
    # Copy the template
    file.copy(get_cfg_template_path(template_dir_name, template_name), cfg_path,
              overwrite = overwrite)
  }

  invisible()
}

#' Read the YAML config file into a named list
#'
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#'
#' @return A named list of lists which corresponds to the loaded YAML config.
#' @export
#'
#' @examples
#' read_user_cfg()
#' \dontrun{
#' read_user_cfg(file.path("path", "to", "config"), "conf.yml"))
#' }
#'
read_user_cfg <- function(cfg_dir = NULL, cfg_name = NULL) {
  # Read the configuration from a YAML file
  yaml::read_yaml(get_user_cfg_path(cfg_dir, cfg_name))
}

#' Get a configuration to use for a run.
#'
#' @inheritParams write_cfg_template
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @param overrides A list containing settings with which to override the
#'   defaults. See section \emph{Details} for more information.
#'
#' @return A list containing settings for a run, constructed from defaults which
#'   would be overridden if `config` and/or `overrides` are supplied. See
#'   section \emph{Details} for more information.
#' @export

#' @details If `cfg` is supplied, this will be the returned config unless
#'   `overrides` is also supplied, in which case the values in `overrides` will
#'   be substituted within `cfg`. If both `cfg` and `overrides` are NULL then
#'   the returned config will be the saved user config. If `cfg` is NULL and
#'   `overrides` is supplied, the returned config will be the saved user config
#'   with the values in `overrides` in overrides substituted.
#'
#' @examples
#' get_cfg()
#' get_cfg(overrides = list(run = list(seed_val = 200)))
#'
get_cfg <- function(cfg = NULL, overrides = NULL, cfg_dir = NULL,
                    cfg_name = NULL) {

  cfg <- cfg %||% read_user_cfg(cfg_dir, cfg_name)

  # Apply supplied overrides
  if (!is.null(overrides)) {
    cfg <- modifyList(cfg, overrides)
  }

  cfg
}

#' Recursively list all keys in a config
#'
#' Retrieve a list of hierarchical config keys, each flattened into single
#'   strings where components in the hierarchy are separated by a dot. If no
#'   arguments are provided, this function retrieves the keys for the default
#'   config.
#'
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#'
#' @return A character vector of hierarchical keys, each flattened into single
#'   strings where components in the hierarchy are separated by a dot.
#' @export
#'
#' @examples
#' get_cfg_keys(cfg = generate_cfg_template())
#'
get_cfg_keys <- function(cfg_dir = NULL, cfg_name = NULL, cfg = NULL) {

  cfg <- cfg %||% read_user_cfg(cfg_dir, cfg_name)

  key_sep <- "."

  # Initialize an empty vector to store the keys
  keys <- c()

  # Iterate over each element in the list
  for (name in names(cfg)) {
    # Always add the top-level key
    keys <- c(keys, name)

    # If the element is a list, recurse
    if (is.list(cfg[[name]])) {
      # Add the current name and recurse on the inner list
      inner_keys <- get_cfg_keys(cfg_dir, cfg_name, cfg = cfg[[name]])

      # Combine the current name with the keys from the inner list
      # TODO: Replace paste with glue::glue_collapse
      full_keys <- paste(name, inner_keys, sep = key_sep)
      keys <- c(keys, full_keys)
    }
  }

  sort(keys)
}

#' Get a config value from the config
#'
#' Retrieve a config value from the user config or the supplied config.
#'
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#' @param key A single character string of hierarchical keys to look up with
#'   each level of the hierarchy separated by a dot.
#'
#' @return A list of the requested value retrieved from the config. If the key
#'   specification corresponds to a 'leaf' entry, this will be returned as a
#'   list of length 1; if it corresponds to a list, this will be returned as a
#'   list of length > 1.
#' @export
#'
#' @examples
#' # This is not a 'leaf' entry so returns a list of length > 1
#' x <- get_cfg_val("store.dat.raw", cfg = generate_cfg_template())
#'
#' # This is a 'leaf' entry so returns a list of length 1 (character)
#' x <- get_cfg_val("store.dat.raw.base_dir", cfg = generate_cfg_template())
#'
get_cfg_val <- function(key, cfg_dir = NULL, cfg_name = NULL, cfg = NULL) {

  cfg <- cfg %||% read_user_cfg(cfg_dir, cfg_name)

  key_sep <- "."

  if (is.null(key)) {
    cli::cli_abort(c(
      "{.var key} is invalid",
      "i" = paste0("{.var key} must be a single character string of",
                   " hierarchical keys with components separated by",
                   " '{key_sep}'."),
      "x" = "You've supplied a NULL {.var key}."
    ))
  }

  # Split the string into component parts
  keys <- stringr::str_split_1(key, pattern = escape_txt(key_sep))

  # TODO: Use purrr or base R?
  # val <- purrr::reduce(keys, function(x, y) x[[y]], .init = cfg)
  val <- Reduce(function(x, y) x[[y]], keys, init = cfg)

  if (is.null(val)) {
    lst_style <-  list('vec-sep' = key_sep, 'vec-last' = key_sep)
    cli::cli_abort(c(
      "Key '{key}' not found in config",
      "i" = paste0("The key must correspond to hierarchical names in the",
                   " config, separated by '{key_sep}'."),
      "x" = "You've supplied '{cli::cli_vec(key, style = {lst_style})}'."
    ))
  }

  val
}

#' Get a path value from the configuration
#'
#' @inheritParams ensure_valid_env
#' @inheritParams get_user_cfg_dir
#' @inheritParams get_user_cfg_name
#' @inheritParams write_cfg_template
#' @param dat_keys A character vector where each element is a key in the same
#'   format as expected by [get_cfg_val()]. These are single character strings
#'   of hierarchical keys to look up (with each level of the hierarchy separated
#'   by a dot).
#'
#' @return A character string: the requested path, retrieved from the default
#'   config or the given config, if provided.
#' @export
#'
#' @examples
#' get_dat_path("store.dat.wrangled.base_dir", cfg = generate_cfg_template())
#'
#' cfg <- get_cfg_val("store.dat.raw", cfg = generate_cfg_template())
#' dat_keys <- c("base_dir", "dir_names.shapefiles")
#' get_dat_path(dat_keys, "ref", cfg = cfg)
#'
get_dat_path <- function(dat_keys, env = NULL, cfg_dir = NULL, cfg_name = NULL,
                         cfg = NULL) {

  env <- env %||% "main"
  ensure_valid_env(env, null_ok = FALSE)

  cfg <- cfg %||% read_user_cfg(cfg_dir, cfg_name)

  sub_paths <- lapply(dat_keys, function(key) {
    val <- get_cfg_val(key, cfg_dir, cfg_name, cfg = cfg)
    # Convert the path into components
    rel_path <- get_path_components(val)
  })

  sub_paths <- unlist(lapply(sub_paths, path_from_components))

  # Get path taking env into account
  file.path(get_root(env), path_from_components(sub_paths))
}

#' Get the root path for an environment
#'
#' Retrieve the root (base) path for the given environment.
#'
#' @inheritParams ensure_valid_env
#'
#' @return A character string: the root path for the given environment.
#' @export
#'
#' @details
#' TODO: Add details for setting root paths according to env <br>
#' - DIMEX_STORE
#' - DIMEX_STORE_REF
#'
#' @examples
#' get_root()
#' get_root("main")
#' get_root("ref")
#'
get_root <- function(env = NULL) {

  env <- env %||% "main"

  ensure_valid_env(env, null_ok = FALSE)

  vars <- c(
    main = "DIMEX_STORE",
    ref = "DIMEX_STORE_REF"
  )

  chk_sys_env_vars(unname(vars))

  root <- if (env == "test") {
    here::here("tests", "testthat", "dat")
  } else {
    Sys.getenv(vars[env], unset = NA)
  }

  if (is.na(root)) {
    cli::cli_abort(c(
      "Could not find setting for root directory [{.var env}='{env}']",
      "i" = glue::glue("Please ensure system environment variable",
                       " {vars[env]} is set as specified in the",
                       " 'Configuration' documentation."),
      "x" = "System env var '{lookup[env]}' not set in .Renviron."
    ))
  }
  if (!dir.exists(root)) {
    cli::cli_abort(c(
      "Root directory not found [env='{env}']",
      "i" = "Directory specified by system env var '{vars[env]}' must exist.",
      "x" = "Directory '{root}' not found, not a directory or not accessible."
    ))
  }

  root
}
