#'fars_read
#'
#'Reads a csv file and converts it to a data frame.
#'The file must be saved in the same directory as the function script.
#'
#'
#'@import  dplyr
#'@import  readr
#'
#'@param filename a string
#'
#'@return a tibble
#'
#'@examples fars_read("./data-raw/accident_2015.csv.bz2")
#'@export
#'

#library(FarsPackage)
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'make_filename
#'
#'Helper function allows user to input a year which is returned as the integer parameter %d in filename.
#'The sprintf function returns a formatted combination of text and variable values.
#'
#'@param year An integer value representing a year
#'
#'@return [1] "./data-raw/accident_2015.csv.bz2"
#'
#'@examples
#'make_filename(2015)
#'@export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("./FarsPackage/data-raw/accident_%d.csv.bz2", year)
}

#'fars_read_years
#'
#'Uses the fars_read and make_filename functions to read a list of inputted years and
#'  applies the year function over each element of the vector,
#'If year is valid calls fars_read function to read the csv file associated with each year
#'Uses the dplyr function mutate to select month and year from data.
#'
#'An error message is generated if an invalid year is entered in the supplied list
#'
#'@import  dplyr
#'
#'@param years A list of years such as c(2013,2014,2015)
#'@param year year function applied to each year in the list
#'@param file "accident_2015.csv.bz2"
#'
#'@return tibbles n = n years
#'
#'@examples
#'fars_read_years(c(2013,2014,2015))
#'@export

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


#'fars_summarize_years
#'
#'Uses helper functions years and fars_read_years function to read each csv file. the dplyr bind_rows function
#'combines the data frames into dat_list object then groups by year and month.
#'The year and number of accidents for each month are spread to columns.
#'
#'@import  dplyr
#'@import  tidyr
#'@import  magrittr
#'
#'@param years (c(2013,2014,2015))
#'
#'@return  a tibble


#'@examples
#'fars_summarize_years(c(2013,2014,2015))
#'
#'@export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'fars_map_state
#'
#'Uses maps  to plot accident location on state map identified by STATE field
#'  enter state.num and year
#'Includes helper functions make_filename() and fars_read() to read data for year entered.
#'Checks state.num input is a valid identifier with %in% unique(data$STATE).
#'Uses dplyr to filter by state number and create subset data.sub.
#'If number of rows in subset == 0 (0L integer) then display message "no accidents to plot".
#'Plots ponts (x,y) in subset on map where x latitude is > 90 and y longitude > 900
#'xlim and ylim functions set world coordinates
#'use points function in graphics  to set plot character type to 46 pch
#'(a rectangle of side 0,01 inch)
#'
#'@import  dplyr
#'@import  maps
#'@import  graphics
#'
#'@param state.num is an integer between 1 and 56
#'@param year is a string identifying a year
#'
#'@return a map resembling a U.S state with plotted accident locations
#'
#'@examples
#'fars_map_state(8, 2015)
#'@export


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
