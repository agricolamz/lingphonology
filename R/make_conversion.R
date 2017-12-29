#' Split and make conversion
#'
#' This is an auxiliary function, that takes a vector of strings for conversion. Returns a dataframe with notconverted vector, converted vector and convertions separated by space.
#' @param x character vector of words that should be converted.
#' @param converter correspondenses dataframe with columns "transcription" and "ipa"
#' @author George Moroz <agricolamz@gmail.com>

make_conversion <- function(x, converter) {
  # save not converted strings ----------------------------------------------
  string_old <- x

  # add # sign for begining and end of the word -----------------------------
  x <- paste0("#", x, "#")
  converter <-
    rbind(converter, data.frame(transcription = "#", ipa = "#"))

  # create an id column for all correspondenses -----------------------------
  converter$id <- 1:nrow(converter)

  # calculate number of characters in all convertion pair -------------------
  converter$nchar <- nchar(converter$transcription)
  nchar <-
    sort(as.integer(names(table(
      converter$nchar
    ))), decreasing = TRUE)

  # create dataframes grouped by number of symbols --------------------------
  lapply(seq_along(nchar), function(i) {
    assign(paste0("change_", i),
           converter[converter$nchar == i,
                          c("transcription", "ipa", "id")],
           envir = parent.env(environment()))
  })

  # make a convertion to id starting from longest ---------------------------
  lapply(nchar, function(k) {
    df <- get(paste0("change_", seq_along(nchar))[k])
    sapply(1:nrow(df), function(j) {
      x <<- gsub(df[j, "transcription"], paste("", df[j, "id"], ""), x)
    })
  })

  # make a convertion to ipa starting from higher id ------------------------
  sapply(nrow(converter):1, function(j) {
    x <<- gsub(converter[j, "id"],
               paste("", converter[j, "ipa"], ""),
               x)
  })

  # remove some spaces ------------------------------------------------------
  x <- gsub(" +", " ", x)
  x <- gsub("^ | $", "", x)

  # create final dataframe --------------------------------------------------
  return(
    data.frame(
      separated = x,
      ipa = gsub(" |#", "", x),
      transcription = string_old,
      stringsAsFactors = FALSE
    ))
}
