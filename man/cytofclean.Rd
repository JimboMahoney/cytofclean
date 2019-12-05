\name{cytofclean}
\alias{cytofclean}
\title{cytofclean: Fast auto-gating of CyTOF FCS files based on Event length, Gaussian parameters and Beads}
\usage{
cytofclean_GUI()
}
\description{
Runs the cytofclean GUI to load FCS files for processing.
}
\details{
\code{cytofclean} takes FCS files from a Helios mass cytometer (CyTOF) and outputs a PNG plot of the results, as well as new FCS files.
The new files are written in a subdirectory CyTOFClean with _CC added to the filename (or timestamps if those files already exist).
It detects the mode (peak) in each of the Event Length, Centre, Offset and Residual and uses the SD (standard deviation) and skew (or bias) to determine
an appropriate position for the gate. The elimination of beads using the Ce140 Channel is simple - anything above 10^2 is considered a bead and removed.
}
\examples{
cytofclean_GUI()
}
\references{
\url{https://github.com/JimboMahoney/cytofclean}
}
\author{
James Wainwright
}
