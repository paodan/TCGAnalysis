
#' Download data using the manifest file.
#' @param manifest manifest file downloaded from GDC Repository
#' @param desDir destination directory
#' @return Destination directory
#' @export
#' @examples
#' \dontrun{
#' mnf = system.file("inst", "extdata", "gdc_manifest_LUAD_expr_5.txt",
#'                   package = "TCGAnalysis")
#' downloadFolder = downloadByManifest(manifest = mnf)
#' downloadFolder
#' }

downloadByManifest = function(manifest, desDir = NULL){

  if(is.null(desDir)){
    desDir = basename(tempdir())
    dir.create(desDir, showWarnings = F)
    desDir = normalizePath(desDir)
    message("Files are downloaded in:\n", desDir)
  }

  gdcPath = getGdcClient()
  cmd = paste(gdcPath, "download -m", manifest, "-d", desDir)
  message(cmd)
  system(cmd, wait = T)
  return(desDir)
}


#' Read htseq.counts data
#'
#' @import data.table
#' @import dplyr
read_htseqCounts = function(files, remove_last = TRUE){
  y = lapply(files, function(x){
    data.table::fread(x)
  })
  z = y[[1]]
  for(mi in seq_along(y)[-1]){
    z = dplyr::left_join(z, y[[mi]], by = "V1")
  }
  if(remove_last){
    z = head(z, -5)
  }
  sampleIDs = gsub("\\.htseq\\.counts\\.gz", "", basename(files))
  colnames(z) = c("Gene", sampleIDs)
  z = as.data.frame(z)
  return(z)
}



#' Read data downloaded from TCGA GDC repository
#' @import data.table
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' x = readTCGA("./RtmpI9X5wD/")
#' head(x)
#' }
readTCGA = function(downloadFolder, dataType = "htseq.counts", ...){
  args = list(...)
  if("remove_last" %in% names(args)) {
    remove_last = args$remove_last
    stopifnot(is.logical(remove_last))
  } else {
    remove_last = TRUE
  }

  files0 = dir(downloadFolder, full.names = T, recursive = T)
  files = grep(paste0("*",dataType, ".gz$"), files0, value = T)
  data = switch(dataType,
                htseq.counts = read_htseqCounts(files, remove_last))
  return(data)
}


#' Obtain the gdc-client tool path
#' @return the gdc-clent path
#' @examples
#' \dontrun{
#' getGdcClient()
#' }
getGdcClient = function(){
  options()$gdcClientPath
}

#' Install gdc-client tool
#' @param gdcClientURL The URL for downloading gdc-client tool.
#' @return The full path of downloaded/installed gdc-client tool.
#' @examples
#' \dontrun{
#' # Download gdc-client from https://gdc.cancer.gov/access-data/gdc-data-transfer-tool.
#'
#' # By default install gdc-client of macOS version
#' installTools()
#'
#' # Install gdc-client of Linux (Ubuntu) version
#' installTools("https://gdc.cancer.gov/files/public/file/gdc-client_v1.6.1_Ubuntu_x64.zip")
#'
#' # Install gdc-client of Windows version
#' installTools("https://gdc.cancer.gov/files/public/file/gdc-client_v1.6.1_Windows_x64.zip")
#' }
#' @export
installTools = function(gdcClientURL = "https://gdc.cancer.gov/files/public/file/gdc-client_v1.6.1_OSX_x64.zip"){
  si = sessionInfo()
  os = strsplit(si$running, " ")[[1]][1]
  # desDir = system.file("doc", package = "ggplot2")
  if(sum(x[,1] == "TCGAnalysis") == 1){
    desFile = file.path(system.file("inst", "extdata", package = "TCGAnalysis"), "gdc-client")
  } else {
    desFile = "./gdc-client"
  }
  if(file.exists(desFile)){
    message("gdc-client exists in ", desFile)
  } else {
    cat("Downloading gdc-client\n")
    download.file(gdcClientURL, destfile = paste0(desFile, ".zip"))
    unzip(paste0(desFile, ".zip"))
  }
  desFile = normalizePath(desFile)
  options(gdcClientPath = desFile)
  cat("The full path of gdc-client is:", desFile, "\nIt is stored in options()$gdcClientPath")
  return(invisible(options()$gdcClientPath))
}
