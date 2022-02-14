 # Copyright © 2022 Sam Varner
#
# This file is part of R-packages, a collection of R code that I find useful.
#
# R-packages is free software: you can redistribute it and/or modify it under the terms
# of the GNU General Public License as published by the Free Software Foundation, either
# version 3 of the License, or (at your option) any later version.
#
# R-packages is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with R-packages
# If not, see <http://www.gnu.org/licenses/>.

# This file defines functions for signal processing.

#' Return a copy of the vector x with offset removed.
#'
#' @param x The original vector
#' @param func A function that returns the offset when applied to x. The default function
#' is first(). I.e. calling with no arguments returns x - x[1].
#'
#' @export
#'
#' @examples
#' zero(c(1,2,4,8,16))
#' zero(c(1,2,4,8,16), mean)
zero <- function(x, func=first) {
    x - func(x)
}

#' Return a copy of the vector x with drift and offset removed.
#'
#' @param x The original vector.
#' @return The result of subtracting a LOWESS smooth of x from x.
#' @export
flat <- function(x) {
    t <- 1:length(x)
    smooth <- loess(x ~ t)
    x - predict(smooth, t)
}

#' Return simple statistics in a list.
#'
#' @return A list of mean, standard deviation, maximum, minimum, and span.
#' @export
stats <- function(x) {
    min <- min(x)
    max <- max(x)
    list(mean=mean(x), sd=sd(x), min=min(x), max=max(x), span=max - min)
}

#' Return difference, fractional error, and percent error
#' @export
errors <- function(meas, act) {
    delta <- meas - act
    frac <- delta/act
    list(delta=delta, frac=frac, pct=100*frac)
}

#' Normalize the signal to its standard deviation.
#' @export
norm.sd <- function(x) {
    (x - mean(x))/sd(x)
}

#' Return a vector of integers to index the columns of data frame d.
index <- function(d) {
    1:nrow(d)
}

#' Return the distribution of point-to-point differences.
steps <- function(x) {
    sort(unique(abs(diff(x))))
}
