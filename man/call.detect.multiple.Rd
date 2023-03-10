% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call.detect.multiple.R
\name{call.detect.multiple}
\alias{call.detect.multiple}
\title{call.detect.multiple}
\usage{
call.detect.multiple(
  wave,
  threshold = 0.3,
  msmooth = c(500, 95),
  plot_it = FALSE,
  min_dur = 0.1,
  max_dur = 0.3,
  save_extra = 0
)
}
\arguments{
\item{wave}{wave object, e.g., from `load.wave` or `readWave`.}

\item{threshold}{rector of length 1 or 2. The fraction of the maximum of the normalised envelope to use as
threshold to detect start and end. If a vector of length 2 is supplied, the first is used to detect the
start and the second to detect the end (in case of echo).}

\item{msmooth}{used as argument for the `seewave::env` function. *A vector of length 2 to smooth the
amplitude envelope with a mean sliding window. The first component is the window length (in number of
points). The second component is the overlap between successive windows (in \%).* Default is `c(500, 95)`.}

\item{plot_it}{logical, if `TRUE`, returns three-panel plot of wave form, envelope and spectrogram to
current plotting window. Default is `FALSE`.}

\item{min_dur}{numeric, the minimal duration in seconds for a detection to be saved. Default is `0.1`.}

\item{max_dur}{numeric, the maximal duration in seconds for a detection to be saved. Default is `0.3`.}

\item{save_extra}{numeric, how much to add to start and end time in seconds. Can be used to make sure
the whole vocalisation is included.}
}
\value{
Returns a data frame with start = start time in samples and end = end time in samples for each
detection. Optionally also plots the wave form and detections to current window.
}
\description{
Detects multiple calls in a wave object using an amplitude envelope.
}
