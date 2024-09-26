#' get_factor_recode
#'
#' @param config_path path to the directory containing the config files for this dataset
#'
#' @return a named vector of expressions. There is one element (expression) per factor specified in the config files.
#' The expressions are calls to readr::fct_recode corresponding to the level recode mapping for specified in the
#' config files. Their names are the standard column names of the factor.
#' Thus when unquote-spliced into mutate, the returned vector generates a valid set of named arguments
#' for a mutate call that will recode the factor variables in the way specified.
#' @export
#'
#' @examples
get_factor_recode <- function(config_path) {

  # This function builds up the required components of the returned vector gradually by adding
  # columns to the column_mapping tibble specified in the config files.

  # Read in the mapping from provided column names to standard column names...
  column_mapping <- readRDS(file.path(config_path, "column_mapping.rds"))

  # get list of config files
  config_file_list <- Sys.glob(file.path(config_path, "*.rds"))

  # add column for file names of factor level config files in the config path
  column_mapping <- column_mapping %>%
    dplyr::mutate(
      config_file_name = file.path(config_path, paste0(standard, "_levels.rds")),
      factor_config_file = dplyr::if_else(config_file_name %in% config_file_list, config_file_name, NA_character_)
    )
  column_mapping <-  column_mapping %>% dplyr::select(-config_file_name)

  # add column for the level recode mapping specified in the config files
  get_level_mapping <- function(fn) {
    if (is.na(fn)) {
      return(NA)
    } else {
      readRDS(fn)
    }
  }

  get_level_mapping_v <- Vectorize(get_level_mapping)

  column_mapping <- column_mapping %>% dplyr::mutate(level_mapping = get_level_mapping_v(factor_config_file))

  # add a column containing a named vector that can be used to generate the arguments to fct_recode
  make_recode_vector <- function(level_mapping) {
    if (all(is.na(level_mapping))) {
      return(NA)
    } else {
      # take the tibble specifying the mapping from provided to standard levels, and
      # convert it to a named vector ready to be passed into forcats::fct_recode
      rlang::set_names(level_mapping %>% dplyr::pull(provided), level_mapping %>% dplyr::pull(standard))
    }
  }

  make_recode_vector_v <- Vectorize(make_recode_vector)

  column_mapping <- column_mapping %>% dplyr::mutate(recode_vector = make_recode_vector_v(level_mapping))

  # add a column of expressions that call fct_recode with the specified arguments.
  # Modify this expression so that new (standard) levels that are missing (NA) get
  # named "NULL". See documentation of readr::fct_recode.
  fix_factor_recode_nas <- function(recode_call) {
    call_arg_names <- names(recode_call)[3:length(recode_call)]
    call_arg_names[which(call_arg_names == "")] <- "NULL"
    new_names <- c(names(recode_call)[1:2], call_arg_names)
    names(recode_call) <- new_names
    recode_call
  }

  make_factor_recode_expr <- function(...) {
    standard <- list(...)[["standard"]]
    recode_vector <- list(...)[["recode_vector"]]

    if (all(is.na(recode_vector))) {
      return(NA)
    }

    call_fct <- rlang::expr(forcats::fct_recode(!!rlang::sym(standard), !!!recode_vector))

    fix_factor_recode_nas(call_fct)
  }

  column_mapping <- column_mapping %>% dplyr::mutate(factor_recode_expr = purrr::pmap(., function(...) make_factor_recode_expr(...)))

  # convert the column of fct_recode call expressions into a named vector
  # ready to be unquote-spliced into mutate in import_and_standardise
  column_mapping <- column_mapping %>% dplyr::filter(!is.na(factor_recode_expr))

  rlang::set_names(column_mapping %>% dplyr::pull(factor_recode_expr), column_mapping %>% dplyr::pull(standard))
}


#' get_import_col_types
#'
#' @param config_path path to the config files for a given dataset
#'
#' @return a column specification to be used for parsing the dataset, as a named vector of
#' expressions. The expressions are readr collectors, and the names specify the columns they
#' will parse.
#' @export
#'
#' @examples
get_import_col_types <- function(config_path) {

  # This function builds up the required components of the returned vector gradually by adding
  # columns to the column_mapping tibble specified in the config files.

  # Read in the mapping from provided column names to standard column names...
  column_mapping <- readRDS(file.path(config_path, "column_mapping.rds"))

  # get list of config files
  config_file_list <- Sys.glob(paste0(config_path, "*.rds"))

  # add column for file names of factor level config files in the config path
  column_mapping <- column_mapping %>%
    dplyr::mutate(
      config_file_name = paste0(config_path, standard, "_levels.rds"),
      factor_config_file = dplyr::if_else(config_file_name %in% config_file_list, config_file_name, NA_character_)
    ) %>%
    dplyr::select(-config_file_name)

  # add a column containing vectors of provided levels for each factor.

  get_provided_levels <- function(fn) {
    if (is.na(fn)) {
      return(NA)
    } else {
      readRDS(fn) %>% dplyr::pull(provided)
    }
  }

  gplv <- Vectorize(get_provided_levels)

  column_mapping <- column_mapping %>% dplyr::mutate(provided_levels = gplv(factor_config_file))

  # Set up datetime formats
  datetime_formats <- readRDS(file.path(config_path, "datetime_formats.rds"))
  column_mapping <- column_mapping %>% dplyr::left_join(datetime_formats, by = c("provided" = "column_name"))

  # Convert the vectors of provided factor levels to readr::col_factor expressions
  make_factor_import_spec <- function(...) {
    standard_col_name <- list(...)[["standard"]]
    provided_levels <- list(...)[["provided_levels"]]

    if (all(is.na(provided_levels))) {
      return(NA)
    }

    rlang::expr(readr::col_factor(levels = !!provided_levels))
  }

  column_mapping <- column_mapping %>% dplyr::mutate(importType = purrr::pmap(., function(...) make_factor_import_spec(...)))

  # Convert datetime formats to readr::col_datetime expressions
  make_datetime_import_spec <- function(...) {
    standard_col_name <- list(...)[["standard"]]
    datetime_format <- list(...)[["datetime_format"]]

    if (is.na(datetime_format)) {
      return(NA)
    }

    rlang::expr(readr::col_datetime(format = !!datetime_format))
  }

  column_mapping <- column_mapping %>% dplyr::mutate(importType = dplyr::if_else(is.na(importType),
    purrr::pmap(., function(...) make_datetime_import_spec(...)),
    importType
  ))

  # Any remaining columns specified as readr::character expressions

  make_char_import_spec <- function(...) {
    rlang::expr(readr::col_character())
  }

  column_mapping <- column_mapping %>% dplyr::mutate(importType = ifelse(is.na(importType),
    purrr::pmap(., function(...) make_char_import_spec(...)),
    importType
  ))


  # Label the types with the respective column names and return this as a named
  # vector
  colImportTypes <- column_mapping %>% dplyr::pull(importType)
  colImportNames <- column_mapping %>% dplyr::pull(provided)
  colImportTypes <- rlang::set_names(colImportTypes, colImportNames)
  colImportTypes
}


#' get_colname_mapping
#'
#' @param config_path path to the config files for a given dataset
#'
#' @return character vector specifying column renaming, of the form
#' c(new_name_1 = "old_name_1", new_name_2 = "old_name_2")
#' @export
#'
#' @examples
get_colname_mapping <- function(config_path) {
  # Read in the mapping from provided column names to standard column names...
  column_mapping <- readRDS(file.path(config_path, "column_mapping.rds"))

  column_mapping
}

#' import_and_standardise
#'
#' @param data_import_list list of named lists, each named list has four elements called
#' data_path, config_path, site, and facility. The first two of these are character strings specifying the paths
#' to i) a csv file to import data from and ii) a folder containing hospitalflow config
#' files. The latter two specify the site and facility (e.g. ED, IP, UCC).
#' @param remove_duplicates boolean to control whether data is de-duped (TRUE) or not (FALSE).
#'
#' @return list of tibbles, each containing standardised import of one data file from data_paths
#' @export
#'
#' @examples
import_and_standardise <- function(data_import_list, remove_duplicates = TRUE) {

  # get timezone from config
  datetime_formats <- readRDS(file.path(data_import_list[[1]]$config_path, "datetime_formats.rds"))

  # Take the data_import_list and for each element x, load the data located at data_path
  # using configuration specified by the files at config_path.
  # In particular, use the column specification generated by
  # get_import_col_types from the config files.
  data_config_list <- lapply(
    data_import_list,
    function(x) {
      list(
        data = readr::read_csv(x$data_path,
          locale = readr::locale(tz = x$time_zone),
          col_types = eval(rlang::call2(readr::cols,
            !!!get_import_col_types(x$config_path),
            .default = readr::col_skip()
          ))
        ),
        config_path = x$config_path,
        site = x$site,
        facility = x$facility,
        time_zone = x$time_zone
      )
    }
  )

  # Rename the columns in each imported dataset, using the name mapping onto standard hospitalflow
  # variable names specified in the config files at the specified config_path.
  data_config_list <- lapply(
    data_config_list,
    function(x) {
      list(
        data = standardise_column_names(
          x$data,
          get_colname_mapping(x$config_path) %>%
            dplyr::filter(provided %in% colnames(x$data))
        ),
        config_path = x$config_path,
        site = x$site,
        facility = x$facility,
        time_zone = x$time_zone
      )
    }
  )

  # Recode factors
  data_config_list <- lapply(
    data_config_list,
    function(x) {
      list(
        data = eval(rlang::call2(dplyr::mutate,
          .data = x$data,
          !!!get_factor_recode(x$config_path)
        )),
        config_path = x$config_path,
        site = x$site,
        facility = x$facility,
        time_zone = x$time_zone
      )
    }
  )

  # If no site column has been imported, create one and populate with the configured site value
  data_config_list <- lapply(data_config_list, function(x) {
    if ("site" %in% colnames(x$data)) {
      if (!is.na(x$site)) {
        warning(
          "Site is specified both as a column in the data and through config. Config value will be ignored."
        )
      }
    } else {
      x$data <- x$data %>% dplyr::mutate(site = x$site)
    }
    x
  })

  # Extract the data as a tibble for each imported file, and return as a list of these tibbles.
  # data_list <- lapply(data_config_list, function(x) x$data)

  # Add episode ids to each data tibble
  data_config_list <- lapply(data_config_list, function(x) {
    list(
      data = make_episode_ids(x$data),
      config_path = x$config_path,
      site = x$site,
      facility = x$facility,
      time_zone = x$time_zone
    )
  })

  # If required, de-dupe each tibble based on pseudo_id, and episode start and end time
  if (remove_duplicates) {
    data_config_list <- lapply(data_config_list, function(x) {
      list(
        data = dplyr::distinct(x$data, pseudo_id, start_datetime, end_datetime, .keep_all = TRUE),
        config_path = x$config_path,
        site = x$site,
        facility = x$facility,
        time_zone = x$time_zone
      )
    })
  }

  # Label this list of tibbles with the names of the files they came from
  data_filenames <- sapply(data_import_list, function(x) {
    tools::file_path_sans_ext(basename(x[["data_path"]]))
  })
  data_config_list <- setNames(data_config_list, data_filenames)

  # Return named list of lists with data tibbles and config metadata
  data_config_list
}


#' make_episode_ids
#'
#' @param episode_data
#'
#' @return episode_data with a new index column
#' @export
#'
#' @examples
make_episode_ids <- function(episode_data) {
  episode_data %>% dplyr::mutate(episode_id = dplyr::row_number())
}


#' bind_into_facilities
#'
#' @param data_config_list
#'
#' @return list of as many tibbles as there are unique values of facility in the provided config list. Each
#' tibble is formed by row binding all imported data for that facility.
#' @export
#'
bind_into_facilities <- function(data_config_list) {
  fac_list <- unique(sapply(data_config_list, function(x) x$facility))
  standardised_data <- lapply(fac_list, function(x) {
    fac_data_config_list <- filter_config_list(data_config_list, field = "facility", value = x)
    fac_data_list <- lapply(fac_data_config_list, function(x) x$data)
    dplyr::bind_rows(fac_data_list)
  })
  setNames(standardised_data, fac_list)
}


#' import_standardise_bind
#'
#' @param data_import_list list of named lists, each named list has four elements called
#' data_path, config_path, site, and facility. The first two of these are character strings specifying the paths
#' to i) a csv file to import data from and ii) a folder containing hospitalflow config
#' files. The latter two specify the site and facility (e.g. ED, IP, UCC).
#'
#' @return list of as many tibbles as there are unique values of facility in the provided config list. Each
#' tibble is formed by row binding all imported data for that facility. N.B. when sites with different time zones
#' are bound together, all datetimes are converted to have the same time zone by R default.
#' @export
#'
import_standardise_bind <- function(data_import_list) {
  data_config_list <- import_and_standardise(data_import_list)
  bind_into_facilities(data_config_list)
}


#' filter_config_list
#'
#' @param config_list a configuration list (of lists)
#' @param field the field of the inner lists to filter on
#' @param value the value of the field to filter to
#'
#' @return list of only those inner lists where field == value
#' @export
#'
filter_config_list <- function(config_list, field, value) {
  config_list <- lapply(config_list, function(x) {
    if (x[[field]] == value) {
      x
    } else {
      NA
    }
  })
  config_list[!is.na(config_list)]
}

# lgt__data_import_list <- list(list(data_path = "../lgt-data/data-extract-201901/CLAHRCExtractToSend_QEH_20190107_ED.csv",
#                                    config_path = "lgt-config/ed/"),
#                               list(data_path = "../lgt-data/data-extract-201901/CLAHRCExtractToSend_UHL_20190104_AW.csv",
#                                    config_path =  "lgt-config/aw/"),
#                               # list(data_path = "../lgt-data/data-extract-201901/CLAHRCExtractToSend_QEH_20190107_NCF.csv",
#                               #      config_path = "../lgt-config/ncf"),
#                               list(data_path = "../lgt-data/data-extract-201901/CLAHRCExtractToSend_UHL_20190104_ED.csv",
#                                    config_path = "lgt-config/ed/"),
#                               list(data_path = "../lgt-data/data-extract-201901/CLAHRCExtractToSend_QEH_20190107_IP.csv",
#                                   config_path = "lgt-config/inpatient/"),
#                               list(data_path = "../lgt-data/data-extract-201901/CLAHRCExtractToSend_UHL_20190104_IP.csv",
#                                    config_path = "lgt-config/inpatient/"),
#                               list(data_path = "../lgt-data/data-extract-201901/CLAHRCExtractToSend_QEH_20190107_AW.csv",
#                                    config_path = "lgt-config/aw/"))

# cw_data_import_list <- list(
#   list(data_path = "../cw-data/cw_ed_anonim.csv", config_path = "cw-config/inpatient/"),
#   list(data_path = "../cw-data/cw_ip_anonim.csv", config_path = "cw-config/inpatient/"),
#   list(data_path = "../cw-data/cw_ucc_anonim.csv", config_path = "cw-config/ucc/"),
#   list(data_path = "../cw-data/cw_ed_anonim.csv", config_path = "cw-config/ed/"))
