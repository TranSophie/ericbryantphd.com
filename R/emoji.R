emoji <- function(...) {
  paste0(sapply(c(...), emo::ji), collapse = "")
}
