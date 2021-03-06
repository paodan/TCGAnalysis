% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DownloadExpr.R
\name{downloadByManifest}
\alias{downloadByManifest}
\title{Download data using the manifest file.}
\usage{
downloadByManifest(manifest, desDir = NULL)
}
\arguments{
\item{manifest}{manifest file downloaded from GDC Repository}

\item{desDir}{destination directory}
}
\value{
Destination directory
}
\description{
Download data using the manifest file.
}
\examples{
\dontrun{
mnf = system.file("inst", "extdata", "gdc_manifest_LUAD_htseq_counts_5.txt",
                  package = "TCGAnalysis")
downloadFolder = downloadByManifest(manifest = mnf)
downloadFolder
}
}
