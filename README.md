# R-packages

R packages with useful stuff for me.

# Packages
## Signals
Signal processing functions.

* zero(x, func=first): Remove the offset of a vector by subtracting func(x).
* flat(x): Remove offset and drift by subtracting a LOESS smooth.
* stats(x): Return a list of mean, std. dev., min, max, and span.
* errors(x, y): Return a list of difference, fractional, and percent difference.
* index(x): Return an index vector for x. 

## Data
Reading and manipulating data.

* read.set(prefix): Return a data frame made by rbind()ing the frames for each file that begins with <tt>prefix</tt>. The part of the name after the prefix is placed in a 'tag' column of the frame.
* when(date, time, format=iso): Return a POSIXct object for the given date and time strings.
* utime(t): A convenience function for converting UNIX time to a POSIXct.

# Installation
From [Writing an R package from scratch](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
```
> library(devtools)
> library(roxygen2)
> create(<path-to-packages>/<name>)
```

Put files in <tt>name/R</tt> and document with Doxygen comments.
```
> document(<path-to-packages>/<name>)
> install(<path-to-packages>/<name>)
```

