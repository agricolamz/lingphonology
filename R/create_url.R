#' Create a link to PHOIBLE database
#'
#' This function creates a to PHOIBLE database using a vector of IPA symbols.
#' @param ipa a character vector, containing IPA symbols.
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' create_url(c("ɬ", "i", "pʼ"))
#' @export

create_url <- function(ipa) {

  # get url from the phoible dataset ----------------------------------------
  results <-
    merge(data.frame(segment = ipa, stringsAsFactors = FALSE),
          phoible[, c("segment", "url")],
          by = "segment")

  return(results$url)
}
