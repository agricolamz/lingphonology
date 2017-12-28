#' Apply converter to vector of strings
#'
#' This function returns a voctro with transcription and IPA symbols, simultaneously checking this dataframe for mistakes.
#' @param correspondence a character vector, containing symbol correspondences to IPA symbols in orthographical system of user's files.
#' @param ipa a character vector, containing IPA symbols.
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' apply_converter(zilo_dict$zilo_cyrillic, zilo_correspondences)
#' @export

apply_converter <- function(x, converter){
  return(make_conversion(x, converter)$ipa)
}
