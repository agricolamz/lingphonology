#' Create IPA-transcription correspondence
#'
#' This function returns a dataframe with IPA id, IPA symbol and transcription. If user doesn't provide any data, the function returns a template that is used in lingphonology, so users can save it to the disc and open it with appropriate software.
#' @param correspondence a character vector, containing symbol correspondences to IPA symbols in orthographical system of user's files
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' create_correspondence()
#' @export

create_correspondence <- function(correspondence = "") {
  # change correspondence to string -----------------------------------------
  correspondence <- as.character(correspondence)

  # list of IPA symbols; new symbols should be added to the end! --------------
  ipa <- c("i", "y", "ɨ", "ʉ", "ɯ", "u", "ɪ", "ʏ", "ʊ",
           "ɪ̈",
           "ʊ̈",
           "e", "ø", "ɘ", "ɵ", "ɤ", "o",
           "e̞",
           "ø̞",
           "ə",
           "ɵ̞",
           "ɤ̞",
           "o̞",
           "ɛ", "œ", "ɜ", "ɞ", "ʌ", "ɔ", "æ", "ɐ", "a", "ɶ", "a",
           "ä",
           "ɒ̈",
           "ɑ", "ɒ")

  # create a dataframe with IPA (and/for user's values) ---------------------
  correspondence_df <- data.frame(
    ipa,
    transcription = correspondence,
    stringsAsFactors = FALSE
  )

  # return whole list or remove unused IPA symbols --------------------------
  if (length(correspondence) == 1) {
    correspondence_df
  } else{
    correspondence_df[correspondence_df$transcription != "", ]
  }
}
