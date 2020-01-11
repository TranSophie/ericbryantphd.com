#' Add references to BibTex by PMID
#' 
#' Given a vector of PMIDs, add BibLaTeX entries to reference library.
#' 
#' @param pmids    <chr|int|dbl> Vector of PMIDs. Coerced to character vector.
#'                               Duplicate PMIDs are discarded.
#' @param bib_dir  <string>      Path to reference library directory.
#'                               Defaults to working directory, `"."`.
#' @param bib_name <string>      Name of BibTeX library file.
#'                               Defaults to `"library.bib"`
#' @param print    <flag>        Should new entries be printed in the console?
#'                               Defaults to `TRUE`.
#' 
#' @details PMIDs in BibLaTeX entries are stored in `eprint` field where
#' `eprinttype == "pubmed"`.
#' PMID lookup requires an internet connection (see [RefManageR::GetPubMedByID]).
#' BibLaTeX library file will be created if it does not already exist.
#' If an entry already exists in the BibLaTeX library
#' (as determined by reference key) the existing entry will be kept, thus
#' preserving manually edited entries.
#' Entries are given a reference key in the format `YYYY-MM-First-Last`
#' (i.e. year, numeric month, first author last name, last author last name).
#' Non-ASCII characters in author names are removed from reference keys.
#' Missing information will be missing from key.
#' Entries are merged and sorted using key as unique identifiers.
#' 
#' @importFrom RefManageR ReadBib WriteBib GetPubMedByID
#' @importFrom tibble as_tibble has_name
#' @importFrom stringi stri_c stri_replace_na stri_extract
#' @export
requireNamespace("RefManageR", quietly = TRUE)
requireNamespace("tibble", quietly = TRUE)
requireNamespace("stringi", quietly = TRUE)

# Add references to Bibtex library based on PMIDs either by hand or from YAML file
ref_add_pmids <- function(pmids, bib_dir = ".", bib_name = "library.bib", print = TRUE) {

  # Clean and coerce pmids
  pmids <- unique(as.character(pmids))
  
  # Read/create BibTeX library
  path <- file.path(bib_dir, bib_name)
  if (!file.exists(path)) file.create(path)
  bib <- RefManageR::ReadBib(path)
  
  # Ensure eprint and eprinttype fields exist in bib object
  if (is.null(bib$eprint))     bib$eprint     <- NA_character_
  if (is.null(bib$eprinttype)) bib$eprinttype <- NA_character_
  
  # Only look up new PMIDs
  if (length(bib))
    pmids_new <- setdiff(pmids, with(tibble::as_tibble(bib), eprint[eprinttype == 'pubmed']))
  else
    pmids_new <- pmids
  
  if (!length(pmids_new)) { message('No new PMIDs to add'); return(invisible()) }
  
  # Lookup new entries
  bib_new <- RefManageR::GetPubMedByID(pmids_new)
  
  if (!length(bib_new)) { message('No new PMIDs could be retrieved'); return(invisible()) }
  
  names(bib_new) <- .ref_normalize_keys(bib_new)
  bib_new$url <- stringi::stri_c('https://www.ncbi.nlm.nih.gov/pubmed/', bib_new$eprint)
  bib_merged    <- merge(bib, bib_new, 'key')
  
  if (print) print(bib_new, .opts = list(style = "Biblatex"))
  
  # Write to file
  message(
    'Adding entries for:\n  ', 
    stringi::stri_c('@', names(bib_new), collapse = '\n  ')
  )
  RefManageR::WriteBib(sort(bib_merged, sorting = 'debug'), path)
}


# Constructs, consistent keys
.ref_normalize_keys <- function(bib) {
  df <- tibble::as_tibble(bib)
  
  if (!tibble::has_name(df, 'month')) df$month <- NA
  
  key <- with(df, {
    year <- stringi::stri_replace_na(year, '0000')
    month_number <- .ref_month(month)
    ascii_author <- iconv(author, from = 'UTF-8', to = 'ASCII', sub = '')
    first <- stringi::stri_extract(ascii_author, regex = '(\\w+)(?=(\\s*and))')
    last  <- stringi::stri_extract(ascii_author, regex = '(\\w+)$')
    stringi::stri_c(year, month_number, first, last, sep = "-", ignore_null = TRUE)
  })

  return(key)
}

# Mappings to two digit month numbers, 00 if missing
.ref_month <- function(x, na = '00') {
  x <- tolower(as.character(x))
  month_from_to <-
    c('1'         = '01',
      '2'         = '02',
      '3'         = '03',
      '4'         = '04',
      '5'         = '05',
      '6'         = '06',
      '7'         = '07',
      '8'         = '08',
      '9'         = '09',
      '10'        = '10',
      '11'        = '11',
      '12'        = '12',
      '01'        = '01',
      '02'        = '02',
      '03'        = '03',
      '04'        = '04',
      '05'        = '05',
      '06'        = '06',
      '07'        = '07',
      '08'        = '08',
      '09'        = '09',
      '10'        = '10',
      '11'        = '11',
      '12'        = '12',
      'jan'       = '01',
      'feb'       = '02',
      'mar'       = '03',
      'apr'       = '04',
      'may'       = '05',
      'jun'       = '06',
      'jul'       = '07',
      'aug'       = '08',
      'sep'       = '09',
      'oct'       = '10',
      'nov'       = '11',
      'dec'       = '12',
      'january'   = '01',
      'february'  = '02',
      'march'     = '03',
      'april'     = '04',
      'may'       = '05',
      'june'      = '06',
      'july'      = '07',
      'august'    = '08',
      'september' = '09',
      'october'   = '10',
      'november'  = '11',
      'december'  = '12'
    )
  stringi::stri_replace_na(month_from_to[x], na)
}

.exit <- function(msg, ret = NULL) {
  message(msg)
  if (!is.null(ret)) return(invisible(ret))
}
