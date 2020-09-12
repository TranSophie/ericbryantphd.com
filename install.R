renv::restore()

if (blogdown::hugo_version() != numeric_version("0.74.3")) {
  options(blogdown.hugo.dir = normalizePath("renv/bin")) 
  blogdown::install_hugo(version = "v0.74.3")
}
