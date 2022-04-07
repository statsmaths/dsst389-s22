#############################################################################
# Custom Functions
#
# This is a space to put any custom functions that you want to create; you
# can also re-define functions I wrote, but I generally find it advisable to
# create an function with a different name

dsst_wiki_get_links_table_all <- function(
  obj,
  table_num = 1L,
  column_num = NULL,
  print_first_rows = FALSE
)
{
  tree <- xml2::read_html(obj$parse$text[[1]])
  tables <- xml2::xml_find_all(tree, xpath = ".//table")

  .assert(length(tables) >= table_num,
          sprintf("Asking for table %d of %d", table_num, length(tables)))

  this_table <- tables[[table_num]]
  rows <- xml2::xml_find_all(this_table, ".//tr")

  if (print_first_rows)
  {
    fr <- map_chr(tables, ~ xml2::xml_text(xml2::xml_find_first(..1, ".//tr")))
    fr <- stringi::stri_sub(fr, 1L, options()$width - 5L)
    print(fr)
  }

  # grab links for all columns or specified column
  if (is.null(column_num))
  {
    links <- xml2::xml_find_all(rows, xpath = ".//a")
    links <- xml2::xml_attr(links, "href")
  } else {
    links <- map(rows, function(u) {
      td <- xml2::xml_find_all(u, ".//td")
      res <- ifelse(length(td) < column_num, "",
                    xml2::xml_attr(xml2::xml_find_all(td[[column_num]], ".//a"),
                                   "href"))
      res
    })
    links <- flatten_chr(links)
  }

  # process the links
  links <- links[!is.na(links)]
  links <- links[stringi::stri_sub(links, 1L, 6L) == "/wiki/"]
  links <- links[stringi::stri_sub(links, 1L, 16L) != "/wiki/Wikipedia:"]
  links <- stringi::stri_sub(links, 7L, -1L)
  links <- links[!stringi::stri_detect(links, fixed = "#")]
  links <- unique(links)
  tibble(links = links)
}
