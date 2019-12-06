#' fars_read
#' @description This function reads data, and returns it in a data.frame
#' @param filename of data to be read
#' @return a data.frame of the file read
#' @note ERRORS: If the file does not exist, it stops the program and sends an error message to that effect.
#'                   Works improperly and gives a warning if given a list of years
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
#' @example f2015 <- make_filename(2015); fars_read(f2015)


fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' make_filename
#' @description creates a filename to access data accident data for specific years from the
#'              US National Highway Traffic Safety Administration's Fatality Analysis Reporting System
#' @param year the year or a list of years to be access--either as strings or numerics.  Can also take lists of these.
#' @return a string or list of strings indicating the filenames to be returned
#' @note ERRORS: None, but will return useless filenames for any inputs except 2013, 2014, and 2015
#' @export
#' @example fars_read(2015)


# modified for my computer
make_filename <- function(year) {
  year <- as.integer(year)
  temp<-sprintf("extdata/accident_%d.csv.bz2", year)
  system.file(temp,package = "FarsFunctions2")
}

#' fars_read_years
#' @description Takes a list of years, and returns a list containing data.frames of accidents
#' @param years a list of years to be access--should be a numeric.  Can also work with a single string or numeric
#' @return A list of data.frames containing accident data
#' @note ERRORS: Will give a warnings for any invalid years and return null values (valid years are 2013, 2014 and 2015)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @export
#' @example list_of_dataframes <- fars_read_years(c(2014, 2015))
#'


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years
#' @description  Returns a dataframe with the number of accidents by year and month
#' @param years takes a year or list of years to be accessed--can be as numerics or strings
#' @return a data.frame giving the number of accidents by year and month
#' @note ERRORS: Will give errors (and warnings) for invalid year inputs (valid years include 2013, 2014, and 2015)
#' @importFrom dplyr tbl_df
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyr spread
#' @importFrom readr read_csv
#' @export
#' @example df_accidents <- fars_summarize_years(c(2013, 2014, 2015))
#'

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#' @description  Creates a low-quality map of accidents in a state for a given year
#' @param state.num takes a state-number
#' @param year a year--not a list of years
#' @return a map showing accident locations for the year
#' @note ERRORS: Will give errors (and warnings) for invalid state or year inputs.
#'       Valid state inputs include most of the numbers from 1:56, excluding 3, 7, 14, 43, & 52.
#'       Many return bad maps. Valid year inputs include 2013, 2014, 2015.
#'       Will give a warning message and no map if there are no accidents to plot
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr tbl_df
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom readr read_csv
#' @export
#' @example fars_map_state(13, 2014)

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
