\name{fctProgressBar}
\alias{fctProgressBar}
\alias{setFctProgressBar}
\alias{getFctProgressBar}
\alias{simpleCat}
\title{
Function Progress Bar
}
\description{
Text progress bar calling a function on progress.
}
\usage{
fctProgressBar(min = 0, max = 1, initial = 0, char = "=",
    width = NA, title, label, style = 1, fct = function(txt) cat(txt), label_name = "")
getFctProgressBar(pb)
setFctProgressBar(pb, value, title = "", label = NULL)
simpleCat(txt)
}
\arguments{
  \item{min, max}{
(finite) numeric values for the extremes of the progress bar.
Must have \code{min} < \code{max}.
}
  \item{initial, value}{
initial or new value for the progress bar.
See Details for what happens with invalid values.
}
  \item{char}{
he character (or character string) to form the progress bar.
If number of characters is >1, it is silently stripped to length 1
unless \code{style} is 5 or 6 (see Details).
}
  \item{width}{
the width of the progress bar, as a multiple of the width of char.
If \code{NA}, the default, the number of characters is that
which fits into \code{getOption("width")}.
}
  \item{style}{
the style taking values between 1 and 5. 3 and 4 are meant for use with loggers. The others work for console output. 3 displays a label for the timer.
}
  \item{pb}{
an object of class \code{"fctProgressBar"}.
}
  \item{title}{
Used as initial value for label in style 3.
}
  \item{label}{
ignored, for compatibility with other progress bars.
}
  \item{fct}{
a function with argument txt. This function is called whenever a progress happens to happen.
}
  \item{label_name}{
If this is not set to "", the title of the progressbar (style 3) will not be the function call starting the progressing action (\code{"pb*apply"}) but the contents of a variable named as set in \code{"label_name"} in the environment that was calling \code{"pb*apply"}, if existing and not empty.
}
  \item{txt}{
the text to log.
}
}
\details{
\code{fctProgressBar} will input a progress bar to a function given in the option \code{"fct"}. This could be a logger function.

\code{setFctProgessBar} will update the value. Missing (\code{NA}) and out-of-range values of value will be (silently) ignored. (Such values of \code{initial}
cause the progress bar not to be displayed until a valid value is set.)

The progress bar should be closed when finished with: for styles 1, 2, and 5 this outputs the final newline character (see \code{\link{closepb}}).
For styles 3 and 4 a "done." message will be logged (in case of 3 alos with the label).

For the meaning of the \code{"char"} see \code{"timerProgressBar"}.

\code{simpleCat} is a very simple example for a logger function
}

\value{
For \code{fctProgressBar} an object of class \code{"fctProgressBar"}
inheriting from \code{"txtProgressBar"}.

For \code{getFctProgressBar} and \code{setFctProgressBar},
a length-one numeric vector giving the previous
value (invisibly for \code{setFctProgressBar}).
}

\author{
Stephan Struckmann
}
\seealso{
The \code{fctProgressBar} implementation
follows closely the code of \code{\link[utils]{txtProgressBar}}.
}
\examples{
\dontrun{
# This is meant to run to a file:
# flog.appender(appender.file("/tmp/test.run.log"))
# logging would be called by futile.logger::flog.error(txt)
# However, for testing, we use console and owr own test function
# simpleCat()
pboptions(type = "fct", fct = simpleCat,
  label_name = "progress_label")
das_ist_ein_test <- function(a, b, c, d, e, f = 12) {
  progress_label <- "PROGRESS TEST"
  ok <- all(pblapply(1:10, function(x) { Sys.sleep(0.2); return(x) }) == 1:10)
  stopifnot(ok)
}
das_ist_ein_test(1,2 ,3, 1:12, f=17)
}
}
\keyword{ utilities }
