#' Create IPA-transcription correspondence
#'
#' This function returns a dataframe with IPA id, IPA symbol and transcription. If user doesn't provide any data, the function returns a template that is used in lingphonology, so users can save it to the disc and open it with appropriate software.
#' @param correspondence a character vector, containing symbol correspondences to IPA symbols in orthographical system of user's files
#' @param ipa a character vector, containing symbol correspondences to IPA symbols in orthographical system of user's files
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' create_correspondence(c("p", "t", "k"), c("pʰ", "tʰ", "kʰ"))
#' @export

create_correspondence <- function(correspondence, ipa) {
  # check whether crayon package is installed
  crayon_installed <- "crayon" %in% rownames(installed.packages())

  # change correspondence to string -----------------------------------------
  correspondence <- as.character(correspondence)

  # create a dataframe with IPA (and/for user's values) ---------------------
  corresp_df <- data.frame(transcription = correspondence,
                                  ipa,
                                  stringsAsFactors = FALSE)

  # check IPA provided by user ----------------------------------------------
  in_phoible <- ipa %in% phoible$segment
  not_in_phoible <- ipa[!in_phoible]

  # not IPA "g"
  if (TRUE %in% grepl("g", not_in_phoible)) {
    warning(
      paste0(
        "Probably, you use not apropriate ",
        if (crayon_installed == TRUE) {
          crayon::cyan(crayon::bold(
            'sign "g" for the velar stop '))
        } else{
          'sign "g" for the velar stop '
        },
        "in your IPA transcription (check correspondences: ",
        paste(grep("g", ipa), collapse = ", "),
        ")."
      ),
      call. = FALSE
    )
  }

  # not IPA ":"
  if (TRUE %in% grepl(":", not_in_phoible)) {
    warning(
      paste0(
        "Probably, you use not apropriate ",
        if (crayon_installed == TRUE) {
          crayon::cyan(crayon::bold(
            'sign ":" for the gemination and long vowels'))
        } else{
          'sign ":" for the gemination and long vowels'
        },
        " in your IPA transcription (check correspondences: ",
        paste(grep(":", ipa), collapse = ", "),
        ")."
      ),
      call. = FALSE
    )
  }

  # check transcription provided by user ------------------------------------
  if(FALSE %in% (duplicated(corresp_df) == duplicated(correspondence))){
    duplicated_cor <-
      which((duplicated(corresp_df) == duplicated(correspondence)) %in% FALSE)
    check <- which(correspondence == correspondence[duplicated_cor])
    warning(
      paste0(
        "Probably, you provided ",
        if (crayon_installed == TRUE) {
          crayon::cyan(crayon::bold(
            'omographic correspondences '))
        } else{
          'omographic correspondences '
        },
        "(check correspondences: ",
        paste(check, collapse = ", "),
        ")."
      ),
      call. = FALSE
    )
  }

  return(corresp_df)
}
