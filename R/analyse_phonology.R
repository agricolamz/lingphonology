#' Analyse data and create a report
#'
#' This function takes checked transcription-IPA correspondences and checked corpus or word list and generates a phonological report.
#' @param correspondences a checked dataframe with transcription-IPA correspondences.
#' @param corpus variable containing corpus.
#' @param path path, where function will create a report. By default report is created in working directory.
#' @param format a string that define the output format: "word" or "html" (by default).
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' analyse_phonology()
#' @export

analyse_phonology <- function(correspondences,
                              corpus,
                              path = NULL,
                              format = "html") {

  # change path -------------------------------------------------------------
  if(is.null(path)){
    path = paste0(getwd(), "/")
  }

  # creates base RMarkdown file ---------------------------------------------
  report <- phonological_report
  report <- gsub("child = '", paste0("child = '", path), report)

  # change format from .html to .doc ----------------------------------------
  if(format == "word"){
    report[3] <- "output: word_document"
    report <- report[-c(4:12)]
  }

  writeLines(report, paste0(path, "report.Rmd"))

  # creates consonats.Rmd file ----------------------------------------------
  writeLines("", paste0(path, "consonants.Rmd"))

  # creates vowels.Rmd file -------------------------------------------------
  writeLines("", paste0(path, "vowels.Rmd"))

  # creates syllable_structure.Rmd file -------------------------------------
  writeLines("", paste0(path, "syllable_structure.Rmd"))

  # creates report.html file ------------------------------------------------
  rmarkdown::render(paste0(path, "report.Rmd"), quiet = TRUE)

  # open in a browser -------------------------------------------------------
  if (format == "html") {
    utils::browseURL(paste0(path, "report.html"))
  }
}
