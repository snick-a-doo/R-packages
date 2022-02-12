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

#' A helper function for read.set()
read.match <- function(match) {
    table <- read.table(match[1], header=T)
    table$tag <- match[2]
    table
}

#' Read multiple tables into a single data frame.
#'
#' @param prefix A string that starts the name of each file to be read.
#' @return A data frame formed by stacking the frames from each file. A 'tag' column is
#' added. For each set, the tag consists of the part of the name after the prefix.
#'
#' @export
read.set <- function(prefix) {
    pattern <- paste(prefix, '(.*)', sep='')
    files <- list.files(pattern=pattern)
    if (length(files) == 0)
        stop(paste(prefix, 'did not match any files.'))
    match <- regmatches(files, regexec(pattern, files))
    print(sapply(match, function(x) x[2]))
    # Read the first set, then bind the rest to it.
    set <- read.match(match[[1]])
    for (m in match[-1]) {
        ## Ignore empty files.
        table <- try(read.match(m))
        if (class(table) == 'data.frame' && nrow(table) > 0)
            set <- rbind(set, table)
    }
    set
}

#' Convert ISO date and time strings to a POSIXct time object.
#' @export
when <- function(date.str, time.str, format='%Y-%m-%d %H:%M:%S') {
    as.POSIXct(paste(date.str, time.str), format=format)
}

#' Convert UNIX time to a POSIXct time object.
#' @export
utime <-function(time) {
    as.POSIXct(time, origin='1970-01-01')
}
