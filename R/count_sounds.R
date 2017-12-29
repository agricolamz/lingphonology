#' Count sounds in a word list using converter
#'
#' This function returns a dataframe with counts for all unique converted IPA combinations with context (if context argument greater then 0).
#' @param x a character vector with list of words for analysis.
#' @param converter a converter with transcription-IPA correspondences.
#' @param context integer value that define the context size.
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#' count_sounds(zilo_dict$zilo_cyrillic, zilo_converter)
#' @export

count_sounds <- function(x, converter, context = 0) {
  separated <- make_conversion(x, converter)$separated

  if (context == 0) {
    result <- sapply(seq_along(separated), function(i) {
      unlist(strsplit(separated[i], " "))
    })
    result <- data.frame(table(unlist(result)),
                          stringsAsFactors = FALSE)
    colnames(result) <- c("sound", "n")
    result$sound <- as.character(result$sound)
    result <- result[-which(result$sound == "#"),]
    result <- result[order(result$n, decreasing = TRUE),]
    rownames(result) <- 1:nrow(result)
    return(result)
  } else{
    result <- data.frame(
      left_context = character(0),
      sound = character(0),
      right_context = character(0)
    )

    lapply(seq_along(separated), function(i) {
      word <- c(rep(" ", context - 1),
                unlist(strsplit(separated[i], " ")),
                rep(" ", context - 1))

      datalist <-
        lapply((1 + context):(length(word) - context), function(j) {
          data.frame(
            left_context = paste(word[(j - context):(j - 1)], collapse = ""),
            sound = word[j],
            right_context = paste(word[(j + 1):(j + context)], collapse = ""),
            stringsAsFactors = FALSE
          )
        })

      final_df <- Reduce(function(x, y) {
        merge(
          x,
          y,
          all = TRUE,
          by = c("left_context", "sound", "right_context")
        )
      }, datalist)
      result <<- rbind(result, final_df)
    })

    result$left_context <- gsub(" ", "", result$left_context)
    result$sound <- gsub(" ", "", result$sound)
    result$right_context <- gsub(" ", "", result$right_context)
    result <- data.frame(table(result))
    names(result)[4] <- "n"
    result <- result[result$n > 0, ]
    result <- result[order(result$n, decreasing = TRUE),]
    rownames(result) <- 1:nrow(result)
    return(result)
  }
}
