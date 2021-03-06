% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchExprClinical.R
\name{matchExprClinical}
\alias{matchExprClinical}
\title{Match patient's expression data to clinical data using common case IDs}
\usage{
matchExprClinical(expr, jsonExpr, clinical)
}
\arguments{
\item{expr}{expression data read by readTCGA function}

\item{jsonExpr}{expression JSON data read by read_htseqCountsJSON function}

\item{clinical}{clinical data read by readTCGA function}
}
\value{
A list of three elements: (1) expression data; (2) clinical data; (3) IDs.
But the columns of expression data and the rows of clinical data are matched
by patient ID (bcr_patient_uuid column in TCGA clinical data). The clinical
data of the patients who have expression data but do not have clinical data
are filled by NAs. The clinical data of the patients who have clinical data
but do not have expression data are removed.
}
\description{
Match patient's expression data to clinical data using common case IDs
}
\examples{
\dontrun{

}
}
