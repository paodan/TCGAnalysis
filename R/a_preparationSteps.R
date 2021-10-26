#' Preparation steps

preparationSteps = function(){
  cat("Go to GDC Data Portal (https://portal.gdc.cancer.gov/repository) to
      search the datasets you need.\n")
  cat("Typically you need clinical data and for example gene expression data of
      a particular cancer (for example LUAD).\n")
  cat("First, let's download the clinical data of TCGA-LUAD samples as an example:\n\n")
  cat("On the upleft corner, click 'Cases'
      -> Primary Site (bronchus and lung)
      -> Program (TCGA)
      -> Project (TCGA-LUAD)
      -> Disease Type (adenomas and adenocarcinomas)\n")
  cat("Again on the upleft corner, click 'Files'
      -> Data Category (clinical)\n")
  cat("Again on the upleft corner, click 'Files'
      -> Data Category (clinical)\n")
}
