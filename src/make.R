library(tidyverse)
library(xml2)

# read the HTML dataset
obj <- read_html(file.path("index_src.html"))

# find the links that have a class attribute
links <- xml_find_all(obj, ".//a")
dates <- xml_attr(links, "class")
links <- links[!is.na(dates)]
dates <- dates[!is.na(dates)]

# what is today? might offset a bit to show things the day before needed
today <- as.character(Sys.Date() + 1L)

# take the dates after today; modify those links into spans, remove href
links <- links[dates > today]
xml_set_name(links, "span")
xml_set_attr(links, "href", NULL)

# write the file
write_html(obj, file.path("index.html"), options = "format")
