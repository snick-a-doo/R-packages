## Copyright © 2022 Sam Varner
##
## This file is part of R-packages, a collection of R code that I find useful.
##
## R-packages is free software: you can redistribute it and/or modify it under the terms
## of the GNU General Public License as published by the Free Software Foundation, either
## version 3 of the License, or (at your option) any later version.
##
## R-packages is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
## without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
## PURPOSE.  See the GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along with R-packages
## If not, see <http://www.gnu.org/licenses/>.

## This file defines functions for handling data files and data sets.

library(tibble)

#' Read multiple tables into a single data frame.
#'
#' @param pattern A regular expression that matches file names. If the pattern has
#'     parenthesized groups, the part of the file name that matches the 1st group is
#'     placed in the 'tag' column for that file's entries. If there are no groups tags are
#'     generated from sequential integers.
#' @return A tibble formed by stacking the frames from each file and appending a 'tag'
#'     column.
#'
#' @examples
#' read.set('^ball-b-(\\d+)$')
#' @export
read.set <- function(pattern) {
    match <- list.files(pattern = pattern) %>%
        str_match(pattern)
    ## The 'tag' column is taken from the 1st submatch. If there isn't one, generate tags
    ## from consecutive integers.
    tags <- if(ncol(match) > 1) match[,2] else 1:nrow(match)
    ## Show the tags to indicate how many and which files have been read.
    print(tags)
    table <- mapply(function(f, tag) read.table(f) %>% mutate(tag = tag),
                    match[,1], tags) %>%
        apply(2, as_tibble) %>%
        ## Remove empty tables. Type deduction fails so bind_rows would choke.
        (function(ts) Filter(nrow, ts)) %>%
        bind_rows
}

#' Convert ISO date and time strings to a POSIXct time object.
#' Deprecated: Use lubridate::ymd_hms(paste(date.str, time.str)))
#' @export
when <- function(date.str, time.str, format='%Y-%m-%d %H:%M:%S') {
    print('Deprecated: Use lubridate::ymd_hms(paste(date.str, time.str))')
    as.POSIXct(paste(date.str, time.str), format=format)
}

#' Convert UNIX time to a POSIXct time object.
#' Deprecated: Use lubridate::datetime(time) and possibly with_tz()'
#' @export
utime <-function(time) {
    print('Deprecated: Use lubridate::datetime(time) and possibly with_tz()')
    as.POSIXct(time, origin='1970-01-01')
}
