.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  requireNamespace("utils",quitely=TRUE)
  if (interactive())
  {
    v = utils::packageVersion("osu")
    packageStartupMessage("osu", v, ": This is my first R Package!\n")
	base::cat(" The osu package is attached!!!!\n")
  }   
   
}

.onLoad <- function(libname, pkgname) {
  
  
   base::cat(" The osu package is loaded!!!!\n")
   
}

.onUnload <- function(libpath) {
 	base::cat(" The osu package is unloaded!!!!\n")
}

 