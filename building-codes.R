library(rvest)
library(tidyverse)

# extract table
dat <- read.csv("NBC funds.csv")

#save data
save(dat, file = "fundlinks.rda")

# building codes codes
# h1 <- read_html(dat$Link[1])
# current_price <- h1 %>% html_nodes("span") %>%
#  html_text %>%
#  .[9]


#extract latest date with NAV building codes
#date <- h1 %>% html_nodes("#popupPrixHistorique") %>% html_text()
#squished_date <- str_squish(date)
#extract_dates <- str_extract_all(squished_date,"\\d+-[a-zA-Z]+-\\d+")
#extract_dates[[1]][length(extract_dates[[1]])]
#str_extract_all(squished_date,"\\d+-[a-zA-Z]+-\\d+")[[1]][length(extract_dates[[1]])]


#extract latest date with NAV full code
dat_with_prices <- dat %>%
  as_tibble() %>%
  group_by(Link) %>%
  mutate(NAV = str_extract(read_html(Link) %>% html_nodes("span") %>%
                             html_text %>%
                             .[9], "\\d+.\\d+")) %>%
  mutate(Date = str_extract_all(str_squish(read_html(Link) %>% 
                                             html_nodes("#popupPrixHistorique") %>% 
                                             html_text()),"\\d+-[a-zA-Z]+-\\d+")[[1]]
         [length(str_extract_all(str_squish(read_html(Link) %>% 
                                              html_nodes("#popupPrixHistorique") %>% 
                                              html_text()),"\\d+-[a-zA-Z]+-\\d+")[[1]])]) %>%
  select(Lipper.ID, FundServ, Link, Date, NAV)

dat_with_prices

#building codes for extracting latest data including distributions
#h1 <- read_html(dat$Link[1])
#x <- h1 %>% html_nodes("#popupPaiements") %>% html_text()
#y <- str_squish(x)
#z <- str_extract_all(y,"\\d+-[a-zA-Z]+-\\d+")[[1]][length(str_extract_all(y,"\\d+-[a-zA-Z]+-\\d+")[[1]])]
#p <- str_extract_all(y, "\\d+-[a-zA-Z]+-\\d+\\s\\$\\d+.\\d+")[[1]]
#o <- str_subset(p,z)
#i <- str_extract(o, "\\d+\\.\\d+")

extract_latest_data <- dat_with_prices %>% 
  mutate(latest_distribution_date = str_extract_all(str_squish(read_html(Link) %>% 
           html_nodes("#popupPaiements") %>% 
           html_text()),"\\d+-[a-zA-Z]+-\\d+")[[1]]
         [length(str_extract_all(str_squish(read_html(Link) %>% 
                                              html_nodes("#popupPaiements") %>% 
                                              html_text()),"\\d+-[a-zA-Z]+-\\d+")[[1]])]) %>%
  mutate(distribution = str_extract(str_subset(str_extract_all(str_squish(read_html(Link) %>% 
                                                                            html_nodes("#popupPaiements") %>% 
                                                                            html_text()),
                                                               "\\d+-[a-zA-Z]+-\\d+\\s\\$\\d+.\\d+")[[1]],
                                               str_extract_all(str_squish(read_html(Link) %>% 
                                                                            html_nodes("#popupPaiements") %>% 
                                                                            html_text()),"\\d+-[a-zA-Z]+-\\d+")[[1]]
                                               [length(str_extract_all(str_squish(read_html(Link) %>% 
                                                                                    html_nodes("#popupPaiements") %>% 
                                                                                    html_text()),"\\d+-[a-zA-Z]+-\\d+")[[1]])]), 
                                    "\\d+\\.\\d+"))
         
extract_latest_data


