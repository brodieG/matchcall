#' Remove DLLs when package is unloaded

.onUnload <- function(libpath) {
  library.dynam.unload("matchcall", libpath)
}
