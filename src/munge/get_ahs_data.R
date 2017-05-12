library(rvest)
library(stringr)

pg <- read_html("https://data.gov.in/catalog/annual-health-survey-clinical-anthropometric-bio-chemical-cab-survey")
pg2 <- read_html("https://data.gov.in/catalog/annual-health-survey-clinical-anthropometric-bio-chemical-cab-survey?title=&file_short_format=&page=2")

pg_urls <- pg %>%
  html_nodes("a") %>%   # find all links
  html_attr("href")     # get urls
  
pg %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("/catalog/annual-health-survey-clinical-anthropometric-bio-chemical-cab-survey\\?title=&file_short_format=&page=") %>%
  .[[1]]

p1 <- pg %>%
  html_nodes(pg_urls[[116]]) %>%
  html_text()

page_links <- pg_urls %>%
str_subset("/catalog/annual-health-survey-clinical-anthropometric-bio-chemical-cab-survey\\?title=&file_short_format=&page=")

curr_dls <- pg_urls %>%
  str_subset("download") # find links ending in "download"
  
l1 <- jump_to()

html_session("https://data.gov.in/catalog/annual-health-survey-clinical-anthropometric-bio-chemical-cab-survey") %>%
  follow_link("3")
l1 <- html_session("https://data.gov.in/catalog/annual-health-survey-clinical-anthropometric-bio-chemical-cab-survey") %>%
  jump_to(page_links[2]) 

sess <- html_session("https://data.gov.in/catalog/annual-health-survey-clinical-anthropometric-bio-chemical-cab-survey?title=&file_short_format=&page=2")
