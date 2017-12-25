#' Check IPA-transcription correspondence
#'
#' This function returns a dataframe with transcription and IPA symbols, simultaneously checking this dataframe for mistakes.
#' @param correspondence a character vector, containing symbol correspondences to IPA symbols in orthographical system of user's files.
#' @param ipa a character vector, containing IPA symbols.
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' check_correspondence(c("лъ", "и", "пI"), c("ɬ", "i", "pʼ"))
#' @export

check_correspondence <- function(correspondence, ipa) {
  # check whether crayon package is installed
  crayon_installed <- "crayon" %in% rownames(installed.packages())

  # change correspondence to string -----------------------------------------
  correspondence <- as.character(correspondence)

  # create a dataframe with IPA (and/for user's values) ---------------------
  corresp_df <- data.frame(transcription = correspondence,
                                  ipa,
                                  stringsAsFactors = FALSE)

  # remove duplicates -------------------------------------------------------
  corresp_df <- unique(corresp_df)

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

  # not IPA "'"
  if (TRUE %in% grepl("'", not_in_phoible)) {
    warning(
      paste0(
        "Probably, you use not apropriate ",
        if (crayon_installed == TRUE) {
          crayon::cyan(crayon::bold(
            paste('sign "', "'", '" for the ejectives')))
        } else{
          paste('sign "', "'", '" for the ejectives')
        },
        " in your IPA transcription (check correspondences: ",
        paste(grep("'", ipa), collapse = ", "),
        ")."
      ),
      call. = FALSE
    )
  }

  # not in phoible
  if (length(not_in_phoible) > 0) {
    warning(
      paste0(
        "Symbols ",
        if (crayon_installed == TRUE) {
          crayon::cyan(crayon::bold(
            paste(not_in_phoible, collapse = ", ")))
        } else{
          paste(not_in_phoible, collapse = ", ")
        },
        " are absent in our version of the IPA.",
        " Check correspondences: ",
        paste(which(ipa %in% not_in_phoible), collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  # check transcription provided by user ------------------------------------
  if(TRUE %in% duplicated(corresp_df$transcription)){
    duplicated_cor <- which(duplicated(corresp_df$transcription) %in% TRUE)
    check <- which(corresp_df$transcription == corresp_df$transcription[duplicated_cor])
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
