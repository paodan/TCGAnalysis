
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

#' Read htseq.counts JSON data
#' @export
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
