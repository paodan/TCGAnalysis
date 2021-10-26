
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


# Read htseq.counts data
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
  sampleIDs = basename(files)
  colnames(z) = c("Gene", sampleIDs)
  z = as.data.frame(z)
  return(z)
}


# Read clinical data
read_clinical = function(files = dir("./Vignette/LUAD_expr_5/clinical",
                                     pattern = "clinical.*.xml$",
                                     recursive = TRUE, full.names = TRUE)){
  clinical = sapply(files, function(x){
    y = XML::xmlToDataFrame(x)
    apply(y, 2, function(i){
      if(!(is.na(i[1]))) {j = i[1]} else {j = i[2]}
      j
    })
  })
  clinical = t(clinical)
  rowNames = limma::strsplit2(limma::strsplit2(basename(rownames(clinical)), "clinical.")[,2], "\\.")[,1]
  rownames(clinical) = rowNames

  return(as.data.frame(clinical))
}


# Read clinical JSON data
read_clinicalJSON = function(file = system.file("extdata/gdc_clinical_LUAD_all.json", package = "TCGAnalysis")){
  js = rjson::fromJSON(file = file)
  clinicalList = lapply(js, function(x){
    data.frame(data_format = x$data_format,
               access = x$access,
               cases_id = x$cases[[1]]$case_id,
               project_id = x$cases[[1]]$project$project_id,
               file_name = x$file_name,
               data_category = x$data_category,
               file_size = x$file_size)
  })
  clinical = do.call("rbind", clinicalList)
  return(clinical)
}

# Read htseq.counts JSON data
read_htseqCountsJSON = function(file = system.file("extdata/gdc_htseq_counts_LUAD_all.json", package = "TCGAnalysis")){
  js = rjson::fromJSON(file = file)
  clinicalList = lapply(js, function(x){
    data.frame(data_format = x$data_format,
               access = x$access,
               cases_id = x$cases[[1]]$case_id,
               project_id = x$cases[[1]]$project$project_id,
               file_name = x$file_name,
               data_category = x$data_category,
               file_size = x$file_size)
  })
  clinical = as.data.frame(do.call("rbind", clinicalList))
  return(clinical)
}

#' Match patient's expression data to clinical data using common case IDs
#' @param expr expression data read by readTCGA function
#' @param jsonExpr expression JSON data read by read_htseqCountsJSON function
#' @param clinical clinical data read by readTCGA function
#' @importFrom stringr str_to_lower
#' @return A list of three elements: (1) expression data; (2) clinical data; (3) IDs.
#' But the columns of expression data and the rows of clinical data are matched
#' by patient ID (bcr_patient_uuid column in TCGA clinical data). The clinical
#' data of the patients who have expression data but do not have clinical data
#' are filled by NAs. The clinical data of the patients who have clinical data
#' but do not have expression data are removed.
#' @export
#' @examples
#' \dontrun{
#'
#' }
matchExprClinical = function(expr, jsonExpr, clinical){
  fileID_expr = colnames(expr)[-1]
  jsonExpr2 = jsonExpr[match(fileID_expr, jsonExpr$file_name), ]

  casesID = jsonExpr2[, "cases_id"]
  clinical2 = clinical[match(casesID, stringr::str_to_lower(clinical$bcr_patient_uuid)),]
  patientID = clinical2[, "bcr_patient_barcode"]

  expr = expr[, c("Gene", jsonExpr2$file_name)]
  colnames(expr)[-1] = patientID

  IDs = data.frame(patientID, exprFileID = fileID_expr, casesID)
  return(list(expr = expr, clinical = clinical2, IDs = IDs))
}


#' Read data downloaded from TCGA GDC repository
#' @importFrom data.table fread
#' @import dplyr
#' @import rjson
#' @import XML
#' @import limma
#' @export
#' @examples
#' \dontrun{
#' expr = readTCGA("~/Downloads/LUAD_expr5/htseq_counts/", dataType = "htseq.counts")
#' head(expr)
#' clinical = readTCGA("~/Downloads/LUAD_expr5/clinical/", dataType = "clinical")
#' head(clinical)
#' }
readTCGA = function(downloadFolder,
                    dataType = c("htseq.counts", "clinical"),
                    ...){
  dataTypePatterns = c(htseq.counts = "*htseq\\.counts\\.gz$",
                       clinical = "_clinical\\..*\\.xml$")
  dataType = match.arg(dataType)
  args = list(...)
  if("remove_last" %in% names(args)) {
    remove_last = args$remove_last
    stopifnot(is.logical(remove_last))
  } else {
    remove_last = TRUE
  }

  files0 = dir(downloadFolder, full.names = T, recursive = T)
  files = grep(dataTypePatterns[dataType], files0, value = T)
  data = switch(dataType,
                htseq.counts = read_htseqCounts(files, remove_last),
                clinical = read_clinical(files))
  return(data)
}



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
