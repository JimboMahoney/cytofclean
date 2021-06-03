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
It detects the density of each of the Event Length, Centre, Offset and Residual to determine the optimal gating strategy.
All Gaussin parameters and Bead Channels are transformed using asinh, cofactor 5 to perform the gating. However, the raw data is NOT changed. Only the analysis is performed on the transformed data.
The removal of beads is partially user-controlled. CyTOFClean will ask the user which bead channels do not contain other markers. It will then analyse each of these channels to determine which has the clearest bead signal, then set a gate just before this point.
The resulting files are typically 80% smaller, which decreases upload speeds and storage requirements on cloud-based services such as
Cytobank and Omiq.
}
\examples{
cytofclean_GUI()
}
\references{
\url{https://github.com/JimboMahoney/cytofclean}
}
\author{
JimboMahoney
}
