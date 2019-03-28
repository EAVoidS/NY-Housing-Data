### Importing Dataset ###
url17 <- "https://www1.nyc.gov/assets/hpd/downloads/misc/NYCHVS-2017-Occupied-File-for-ASA-Challenge-CSV.zip"
url91 <- "https://www1.nyc.gov/assets/hpd/downloads/misc/NYCHVS-1991-Occupied-File-for-ASA-Challenge-CSV.zip"
tmp17 <- tempfile()
tmp91 <- tempfile()
download.file(url17, destfile = tmp17)
download.file(url91, destfile = tmp91)
nych17 <- read.csv(unzip(tmp17), skip=1, stringsAsFactors = F)
nych91 <- read.csv(unzip(tmp91), skip=1, stringsAsFactors = F)
header17 <- read.csv(unzip(tmp17), nrows = 1)
header91 <- read.csv(unzip(tmp91), nrows = 1)
colnames(nych17) <- colnames(header17)
colnames(nych91) <- colnames(header91)
### Column Names of Conditions ###
library(dplyr)
walls17 <- paste0("X_d", c(1, 12,3,4))
win17 <- paste0("X_e", c(1,2,3))
stair17 <- paste0("X_f", c(1,2,4,5))
floor17 <- paste0("X_g", c(1,2,3,4))
conditions <- c(walls17,win17,stair17,floor17)
walls91 <- paste0("X_d", c(1,2,3,4))
win91 <- win17
stair91 <- stair17
floor91 <- paste0("X_g", c(1,2,3,4))
### Adding missing Collumns ###
nych17_new <- nych17 %>% mutate(X_d1 = 0, X_g1 = 0, X_g2 = 0)
nych91_new <- nych91 %>% mutate(X_d12 = 0, X_f4 = 0, X_f5 = 0, X_g12 = 0, X_31b = 0)
### Rest of the Dataset ###
cols_trim <- c("borough",
               conditions,
               paste0("X",c("_24a","_25c","_26a","_30a","_31b","_32b","_35a","_36a","_37a","_38a")),
               "hhinc","year")
trim17 <- nych17_new %>% select(cols_trim)
trim91 <- nych91_new %>% select(cols_trim)
colnames(trim17) == colnames(trim91)
merge_17_91 <- bind_rows(trim17,trim91)
head(merge_17_91)
merge_17_91_tmp <- merge_17_91 %>% mutate_at(vars(conditions),recode,
                                             `1` = 1,
                                             `8` = 0,
                                             `9` = 0) %>%
  mutate(X_36a = recode(X_36a, `1`=1, `2`=0, `8`=0)) %>%
  mutate(X_37a = recode(X_37a, `0`=1, `1`=0, `8`=0)) %>%
  mutate(cond_wall = 2*(X_d1 + X_d12 + X_d3 + X_d4 + X_36a + X_37a)) %>%
  mutate(cond_win = 2*(X_e1 + X_e2 + X_e3)) %>%
  mutate(cond_stair = 2*(X_f1 + X_f2 + X_f4 + X_f5)) %>%
  mutate(cond_floor = 2*(X_g1 + X_g2 + X_g3 + X_g4)) %>%
  mutate(cond_score = cond_wall + cond_win + cond_stair + cond_floor) %>%
  select(-one_of(conditions))
merge_17_91_tmp %>% group_by(year, borough) %>%
        summarize(max(cond_score), quantile(cond_score, .90), mean(cond_score))
### Ordering Columns ###
col_order <- c("year", "borough", "X_30a", "X_31b", "X_26a", "X_25c", "X_32b",
               "X_24a", "cond_wall", "cond_floor", "cond_win", "cond_stair",
               "X_35a", "X_38a")
merge_17_91_complete <- merge_17_91_tmp[,col_order]
