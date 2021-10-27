
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

