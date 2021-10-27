
#' Download data using the manifest file.
#' @param manifest manifest file downloaded from GDC Repository
#' @param desDir destination directory
#' @return Destination directory
#' @export
#' @examples
#' \dontrun{
#' mnf = system.file("inst", "extdata", "gdc_manifest_LUAD_htseq_counts_5.txt",
#'                   package = "TCGAnalysis")
#' downloadFolder = downloadByManifest(manifest = mnf)
#' downloadFolder
#' }

downloadByManifest = function(manifest, desDir = NULL){
  if(is.null(desDir)){
    desDir = basename(tempdir())
  }
  dir.create(desDir, showWarnings = FALSE, recursive = TRUE)
  desDir = normalizePath(desDir)
  message("Files are downloaded in:\n", desDir)
  gdcPath = getGdcClient()
  cmd = paste(gdcPath, "download -m", manifest, "-d", desDir)
  message(cmd)
  system(cmd, wait = T)
  return(desDir)
}





