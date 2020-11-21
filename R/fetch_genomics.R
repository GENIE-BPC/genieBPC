#' fetch_genomics
#'
#' Creates mutation, fusion and cna files corresponding to the samples of interest by intersecting gene panels
#' from the different institutions.
#' @param samples_object output object of the opt_samples function (also works with fetch_samples function).
#' @param maf maf file to be used for mutations
#' @param fusion fusion file to be used for fusions
#' @param cna cna file to be used for cna
#' @import
#' dplyr
#' dtplyr
#' tibble

fetch_genomics <- function(samples_object, maf, fusion, cna){

  keep_panels <- as.character(unlist(panel_names %>%
                                       filter(Sequence.Assay.ID %in% dat_opt_samples$Sequence.Assay.ID) %>%
                                       select(Panel)))

  genes <- Reduce(intersect, lapply(keep_panels,function(x){
    get(x)
  }))

  # subset to those genes/samples #
  mut_common <- maf %>%
    filter(
      Tumor_Sample_Barcode %in% as.character(samples_object$sample_ID),
      Hugo_Symbol %in% genes
    )
  fusion_common <- fusion %>%
    filter(
      Tumor_Sample_Barcode %in% as.character(samples_object$sample_ID),
      Hugo_Symbol %in% genes
    )
  cna_common <- cna %>%
    filter(Hugo_Symbol %in% genes) %>%
    select(c("Hugo_Symbol",gsub("-",".",as.character(samples_object$sample_ID))))

  return(list("maf"=mut_common,
              "fusion"=fusion_common,
              "cna"=cna_common))
}
