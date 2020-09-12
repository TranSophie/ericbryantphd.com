source("renv/activate.R")

options(blogdown.hugo.dir = normalizePath("renv/bin")) 
# blogdown::install_hugo(version = "v0.74.3")

if (interactive()) {
  message(R.version.string, " -- using .Rprofile")
  message("Repositories:", paste("\n ", getOption("repos")))
  message("Libraries:", paste("\n ", .libPaths()))
}
