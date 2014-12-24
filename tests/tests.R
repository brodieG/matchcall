library(unitizer)
txt <- 'c("", {
  c(integer(3L), 1:3)
  c(integer(), 1:3, 1L)         # TRUE
  c(integer(), c(1, 2, 3), 1L)  # TRUE
} )
c("", {
  lst <- list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
} )
c("", {
  TRUE
} )'
saveRDS(
  attr(parse(text=txt, keep.source=TRUE), "srcfile")$parseData,
  paste0("~/parsecomp", interactive())
)

# library(unitizer)

# unitize("unitizer/matchcall.R", search.path.clean=TRUE)
