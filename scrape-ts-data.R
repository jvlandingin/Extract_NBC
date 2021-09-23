library(tidyverse)
library(rvest)

load("fundlinks.rda")

#extract latest NAV and date
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

#extract latest distribution and date
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


#view data
view_latest <- extract_latest_data %>% 
  ungroup %>%
  select(Lipper.ID, FundServ, Date, NAV, latest_distribution_date, distribution)


#export data to csv
view_latest_df <- as.data.frame(view_latest)
write_csv(view_latest, file = "latest_data.csv")


