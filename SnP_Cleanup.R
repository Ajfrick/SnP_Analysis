library(tidyverse)
library(stringr)
library(ajfhelpR)

extract_Day = function(x,format = "%Y-%m-%d", numeric = F){
  d = format(as.Date(x, format= format),"%d")
  if(numeric){return(as.numeric(d))}
  d
}

dat = read_csv("Data/S&P+Analysis.csv") %>%
  mutate_all(as.character)

SnP = dat %>% 
  rename_excel2R() %>% 
  mutate(SecOWins = as.numeric(str_extract(`2ndO_Wins`, "[\\d.]{1,}")),
         SecODiff = as.numeric(str_extract(`2ndO_Wins`, "(?<=\\()[\\d-.]{1,}(?=\\))")),
         W        = ifelse(str_detect(Rec., "\\d{1,2}-\\d{1,2}"),
                           str_extract(Rec., "^\\d{1,2}(?=-)"),
                           extract_Mo(as.Date(Rec., format = "%d-%B"))),
         W        = ifelse(is.na(W) & Team == "Ohio State" & Year == "2012", 12, W),
         L        = ifelse(str_detect(Rec., "\\d{1,2}-\\d{1,2}"),
                           str_extract(Rec., "\\d{1,2}$"),
                           extract_Day(as.Date(Rec., format = "%d-%B"))),
         L        = ifelse(is.na(W) & Team == "Ohio State" & Year == "2012", 0, L),
         `S&P+P`  = str_extract(`S&P+P`, "[\\d.]{1,}"),
         Conf.    = ifelse(Conf. == "Pac-10", "Pac-12", Conf.)) %>% 
  select(-c(`2ndO_Wins`, Rec.)) %>% 
  mutate_at(3:ncol(.), as.numeric) %>% 
  select(Team, Conf., W, L, everything()) %>% 
  mutate(GP = W + L)

colnames(SnP) = c("Team", "Conf","W", "L",
                  "SnP_pct", "SnP_val","SnP_rnk",
                  "SnPO_val","SnPO_rnk",
                  "SnPD_val","SnPD_rnk",
                  "SnPS_val","SnPS_rnk",
                  "SoS", "SoS_rnk","Yr", "SecO_W", "SecO_Diff", "GP")

SnP = SnP %>% select(Yr, Conf, Team, W, L, GP, 
                     SecO_W, SecO_Diff,
                     SnP_val, SnP_pct, SnP_rnk,
                     SnPO_val, SnPO_rnk,
                     SnPD_val, SnPD_rnk,
                     SnPS_val, SnPS_rnk,
                     SoS, SoS_rnk)
make_csv(SnP, 
         file = "Data/SnP_Clean.csv")
  