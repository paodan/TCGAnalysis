
#' Obtain the gdc-client tool path
#' @return the gdc-clent path
#' @examples
#' \dontrun{
#' getGdcClient()
#' }
#' @export
getGdcClient = function(){
  tool = system.file("extdata", "gdc-client", package = "TCGAnalysis")
  if(tool == ""){
    stop("gdc-client tool not found, please install it via installTools function.")
  }
  return(tool)
}

#' Install gdc-client tool
#' @param gdcClientURL The URL for downloading gdc-client tool
#' @param manuallyDownloadFile The path of manually downloaded gdc-client .zip file
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
installTools = function(gdcClientURL = "https://gdc.cancer.gov/files/public/file/gdc-client_v1.6.1_OSX_x64.zip",
                        manuallyDownloadFile = NULL){
  desDir = system.file("extdata", package = "TCGAnalysis")
  desFile = file.path(desDir, "gdc-client")
  if(!is.null(manuallyDownloadFile)){
    unzip(manuallyDownloadFile, exdir = desDir)
  } else {
    # si = sessionInfo()
    # os = strsplit(si$running, " ")[[1]][1]
    tmpFile = paste0(tempfile(), ".zip")
    if(file.exists(desFile)){
      message("gdc-client exists in ", desFile)
    } else {
      cat("Downloading gdc-client\n")
      cat("If it fails to download gdc-client tool, then go to the GDC website
        to manually download it and unpress it, and then save it as:\n",
          desFile, ".\n")
      download.file(gdcClientURL, destfile = tmpFile)
      unzip(tmpFile, exdir = desDir)
    }
  }
  desFile = normalizePath(desFile)
  system(paste("chmod 755", desFile))
  cat("The gdc-client is installed as:", desFile, "\n")
  return(invisible(desFile))
}