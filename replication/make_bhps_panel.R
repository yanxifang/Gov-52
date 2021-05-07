setwd("D:/User/Files/Academics/Spring 2021/GOV 52/Replication Project A/replication")

###############################################################################
# LOAD REQUIRED LIBRARIES                                                     #
###############################################################################

library("dplyr")
library("haven")
library("stringr")
library("plm")

###############################################################################
# Generate bhps-panel from data/ukhls
###############################################################################
# directory containing .dta files
trunk <- "ukhls/UKDA-6614-stata/stata/stata13_se/"

# cross-wave identifiers
xid <- read_dta(paste0(trunk, "bhps_wx/xwaveid_bh.dta"))
wave.conditions <- paste0("xid$b", letters[1:18], "_ivfio_bh==1")

# number of people interviewed in consecutive waves
for (i in 1:18) { 
  print(subset(xid, eval(parse(text=paste0(wave.conditions[1:i], collapse = "&")))) %>% nrow)
}

all.waves <- subset(xid, eval(parse(text=paste0(wave.conditions, collapse = "&"))))
all.waves <- xid 

# loop dta files (this reads in the individual and household data for all 18 waves)
for (i in 1:18) { 
  curr.wave <- str_pad(i, width = 2, pad = "0")
  curr.pref <- paste0("b", letters[i])
  
  eval(parse(text = paste0("bhls.id", curr.wave, "<-", 
                           "read_dta('", trunk, "bhps_w", i, "/", curr.pref, "_indresp.dta')"))
  )
  
  eval(parse(text = paste0("bhls.hh", curr.wave, "<-", 
                           "read_dta('", trunk, "bhps_w", i, "/", curr.pref, "_hhresp.dta')"))
  )
}

## Filling in variables that are missing in some waves
# reasons for moving not recorded in wave 1
bhls.id01 <- bhls.id01 %>% mutate(ba_plnew = NA, ba_movy1 = NA, ba_movy2 = NA)
# local authorities not recorded in waves 17 or 18
bhls.hh17 <- bhls.hh17 %>% mutate(bq_ladistc = NA)
bhls.hh18 <- bhls.hh18 %>% mutate(br_ladistc = NA)
# defining longitudinal weight for wave 1
bhls.id01$ba_lrwght <- bhls.id01$ba_xrwght
bhls.id01$ba_lewght <- bhls.id01$ba_xewght

# subset raw data to necessary cols
for (i in 1:18) { 
  
  curr.wave <- str_pad(i, width = 2, pad = "0")
  curr.pref <- paste0("b", letters[i])
  
  eval(parse(text = paste0("wave", curr.wave, "<- bhls.id", curr.wave, "%>%",  
                           "subset(select = c(", 
                           curr.pref, "_hid,",     # hhold id
                           curr.pref, "_pno,",     # person in household
                           curr.pref, "_plnew,",   # new address? 
                           curr.pref, "_mastat,",  # marital status
                           curr.pref, "_hlprb,",   # health problems
                           curr.pref, "_jbstat,",  # employed
                           curr.pref, "_jbstatl,", # employed in sep of last year
                           curr.pref, "_jbstatt,", # employed in sep of this year
                           curr.pref, "_movy1,",   # reason for moving 1
                           curr.pref, "_movy2,",   # reason for moving 2
                           curr.pref, "_vote1,",   # supporter of party
                           curr.pref, "_vote2,",   # closer to party
                           curr.pref, "_xrwght,",     # individual respondent cross-sectional weight
                           curr.pref, "_lrwght,",     # individual respondent longitudinal weight
                           curr.pref, "_xewght,",     # individual enumeration longitudinal weight
                           curr.pref, "_lewght,",     # individual respondent cross-sectional weight
                           curr.pref, "_vote4)) %>%",  # which party
                           "merge(y = subset(bhls.hh", curr.wave, ", select = c(", curr.pref, "_hid,", curr.pref, "_ladistc,", 
                           curr.pref, "_region,", curr.pref, "_tenure_dv,", curr.pref, "_xphsdf)))", "%>%",  
                           "merge(y = subset(all.waves, select = c(pid, pidp, ", curr.pref, "_hid,", curr.pref, "_pno,", "birthy)))")))
}

parse_obj = function(prefix, obj) eval(parse(text=paste0(prefix, obj)))

form_data = function(data, year, prefix) {
  if (year == 1991 | year == "1991") {
    transformed_data = transmute(
      data,
      pid,
      year = year,
      birthy,
      pno = parse_obj(prefix, "pno"),
      hlprb = ifelse(parse_obj(prefix, "hlprb") == -8, 1, 0), 
      divorce = ifelse(parse_obj(prefix, "mastat") == 4, 1, 0),
      emp = ifelse(parse_obj(prefix, "jbstat")==2,1,0),
      empl = ifelse(parse_obj(prefix, "jbstatl")==2,1,0), 
      empt = ifelse(parse_obj(prefix, "jbstatt")==2,1,0),
      lad = parse_obj(prefix, "ladistc"),
      reg = parse_obj(prefix, "region"),
      problems = ifelse(parse_obj(prefix, "xphsdf") == 1, 1, 0), 
      council = ifelse(parse_obj(prefix, "tenure_dv") == 3, 1, 0),
      labour = ifelse(parse_obj(prefix, "vote4") == 2, 1, 0),
      conservative = ifelse(parse_obj(prefix, "vote4") == 1, 1, 0),
      vote1 = ifelse(parse_obj(prefix, "vote1") == 1, 1, 0), 
      vote2 = ifelse(parse_obj(prefix, "vote1") == 2 & parse_obj(prefix, "vote2") == 2, 1, 0),
      xrwght = parse_obj(prefix, "xrwght"),
      lrwght = parse_obj(prefix, "lrwght"),
      xewght = parse_obj(prefix, "xewght"),
      lewght = parse_obj(prefix, "lewght"),
      moved = NA,
      evicted = NA
    )
  } else {
    transformed_data = transmute(
      data,
      pid,
      year = year,
      birthy,
      pno = parse_obj(prefix, "pno"),
      hlprb = ifelse(parse_obj(prefix, "hlprb") == -8, 1, 0), 
      divorce = ifelse(parse_obj(prefix, "mastat") == 4, 1, 0),
      emp = ifelse(parse_obj(prefix, "jbstat")==2,1,0),
      empl = ifelse(parse_obj(prefix, "jbstatl")==2,1,0), 
      empt = ifelse(parse_obj(prefix, "jbstatt")==2,1,0),
      lad = parse_obj(prefix, "ladistc"),
      reg = parse_obj(prefix, "region"),
      problems = ifelse(parse_obj(prefix, "xphsdf") == 1, 1, 0), 
      council = ifelse(parse_obj(prefix, "tenure_dv") == 3, 1, 0),
      labour = ifelse(parse_obj(prefix, "vote4") == 2, 1, 0),
      conservative = ifelse(parse_obj(prefix, "vote4") == 1, 1, 0),
      vote1 = ifelse(parse_obj(prefix, "vote1") == 1, 1, 0), 
      vote2 = ifelse(parse_obj(prefix, "vote1") == 2 & parse_obj(prefix, "vote2") == 2, 1, 0),
      xrwght = parse_obj(prefix, "xrwght"),
      lrwght = parse_obj(prefix, "lrwght"),
      xewght = parse_obj(prefix, "xewght"),
      lewght = parse_obj(prefix, "lewght"),
      moved = ifelse(parse_obj(prefix, "plnew") == 2, 1, 0),
      evicted = ifelse(parse_obj(prefix, "movy1") == 15 |  parse_obj(prefix, "movy2") == 15, 1, 0)
    )
  }
  return(transformed_data)
}

# putting it all together
df <- bind_rows(
  form_data(wave01, 1991, "wave01$ba_"),
  form_data(wave02, 1992, "wave02$bb_"),
  form_data(wave03, 1993, "wave03$bc_"),
  form_data(wave04, 1994, "wave04$bd_"),
  form_data(wave05, 1995, "wave05$be_"),
  form_data(wave06, 1996, "wave06$bf_"),
  form_data(wave07, 1997, "wave07$bg_"),
  form_data(wave08, 1998, "wave08$bh_"),
  form_data(wave09, 1999, "wave09$bi_"),
  form_data(wave10, 2000, "wave10$bj_"),
  form_data(wave11, 2001, "wave11$bk_"),
  form_data(wave12, 2002, "wave12$bl_"),
  form_data(wave13, 2003, "wave13$bm_"),
  form_data(wave14, 2004, "wave14$bn_"),
  form_data(wave15, 2005, "wave15$bo_"),
  form_data(wave16, 2006, "wave16$bp_"),
  form_data(wave17, 2007, "wave17$bq_"),
  form_data(wave18, 2008, "wave18$br_")
)

rm(list=ls(pattern="^bhls"))
rm(list=ls(pattern="^wave"))
gc()

write.csv(df, file="ukhls/bhps_panel.csv", row.names=FALSE)


####################################################################################
# Creating leads and lags
####################################################################################
## fill in missing values of variables for data years where participants were not observed 
## (between the first and last data years where they appeared)
enter_and_exit <- df %>%
  group_by(pid) %>%
  arrange(pid, year) %>%
  summarise("enter_year"=first(year),
            "exit_year"=last(year),
            "n_year"=length(unique(year)))
enter_and_exit_num <- enter_and_exit
enter_and_exit_num$enter_year <- as.numeric(as.character(enter_and_exit_num$enter_year))
enter_and_exit_num$exit_year <- as.numeric(as.character(enter_and_exit_num$exit_year))
enter_and_exit_num$n_possible_year <- enter_and_exit_num$exit_year - enter_and_exit_num$enter_year + 1

pids_rejoining <- enter_and_exit_num %>%
  filter(n_possible_year != n_year) %>%
  select(pid) %>%
  pull()

pids_all_years <- enter_and_exit_num %>%
  filter(n_year == 18) %>%
  select(pid) %>%
  pull()
rm(enter_and_exit)
rm(enter_and_exit_num)

intermediate.years.df <- df
intermediate.years.df$year <- as.numeric(as.character(intermediate.years.df$year))
intermediate.years.df$is_missing_year <- FALSE

for (i in 1:length(pids_rejoining)) {
  one_pid_rejoining <- pids_rejoining[i]
  idx_one_pid_rejoining <- which(intermediate.years.df$pid == one_pid_rejoining)
  years_one_pid_rejoining <- intermediate.years.df[idx_one_pid_rejoining, "year"]
  possible_years <- seq(min(years_one_pid_rejoining), 
                        max(years_one_pid_rejoining), 
                        by=1)
  missing_years <- setdiff(possible_years, years_one_pid_rejoining)
  intermediate.years.df <- intermediate.years.df %>%
    add_row(pid=one_pid_rejoining,
            year=missing_years,
            is_missing_year=TRUE)
  if ((i %% 100) == 0) {
    print(i)
  }
}

## define leads and lags (*takes a few minutes to run*)
recoded.lead.lag.df <- intermediate.years.df %>% 
  group_by(pid) %>% arrange(pid, year) %>% mutate(
    age = as.numeric(year)-as.numeric(birthy),
    lag.empl = dplyr::lag(empl),
    lag.div = dplyr::lag(divorce), 
    lag.hlprb = dplyr::lag(hlprb), 
    d.reg = ifelse(reg != dplyr::lag(reg), 1, 0), 
    d.lad = ifelse(lad != dplyr::lag(lad), 1, 0), 
    lag.lad = dplyr::lag(lad), 
    council.lag1 = dplyr::lag(council), 
    council.exit = ifelse(dplyr::lag(council) == 1 & council == 0, 1, 0), 
    labour.lag1   = dplyr::lag(labour), 
    labour.lag2 = dplyr::lag(labour, 2), 
    labour.lag3 = dplyr::lag(labour, 3), 
    labour.lag4 = dplyr::lag(labour, 4), 
    labour.lag5 = dplyr::lag(labour, 5), 
    labour.lead1   = dplyr::lead(labour), 
    labour.lead2 = dplyr::lead(labour, 2), 
    labour.lead3 = dplyr::lead(labour, 3), 
    labour.lead4 = dplyr::lead(labour, 4), 
    labour.lead5 = dplyr::lead(labour, 5), 
    conservative.lag1   = dplyr::lag(conservative), 
    conservative.lag2 = dplyr::lag(conservative, 2), 
    conservative.lag3 = dplyr::lag(conservative, 3), 
    conservative.lag4 = dplyr::lag(conservative, 4), 
    conservative.lag5 = dplyr::lag(conservative, 5), 
    conservative.lead1   = dplyr::lead(conservative), 
    conservative.lead2 = dplyr::lead(conservative, 2), 
    conservative.lead3 = dplyr::lead(conservative, 3), 
    conservative.lead4 = dplyr::lead(conservative, 4), 
    conservative.lead5 = dplyr::lead(conservative, 5), 
    vote1.lag1   = dplyr::lag(vote1), 
    vote1.lag2 = dplyr::lag(vote1, 2), 
    vote1.lag3 = dplyr::lag(vote1, 3), 
    vote1.lag4 = dplyr::lag(vote1, 4), 
    vote1.lag5 = dplyr::lag(vote1, 5), 
    vote1.lead1   = dplyr::lead(vote1), 
    vote1.lead2 = dplyr::lead(vote1, 2), 
    vote1.lead3 = dplyr::lead(vote1, 3), 
    vote1.lead4 = dplyr::lead(vote1, 4), 
    vote1.lead5 = dplyr::lead(vote1, 5), 
    vote2.lag1   = dplyr::lag(vote2), 
    vote2.lag2 = dplyr::lag(vote2, 2), 
    vote2.lag3 = dplyr::lag(vote2, 3), 
    vote2.lag4 = dplyr::lag(vote2, 4), 
    vote2.lag5 = dplyr::lag(vote2, 5), 
    vote2.lead1   = dplyr::lead(vote2), 
    vote2.lead2 = dplyr::lead(vote2, 2), 
    vote2.lead3 = dplyr::lead(vote2, 3), 
    vote2.lead4 = dplyr::lead(vote2, 4), 
    vote2.lead5 = dplyr::lead(vote2, 5),
    d.lad.lag1   = dplyr::lag(d.lad), 
    d.lad.lag2 = dplyr::lag(d.lad, 2), 
    d.lad.lag3 = dplyr::lag(d.lad, 3), 
    d.lad.lag4 = dplyr::lag(d.lad, 4), 
    d.lad.lag5 = dplyr::lag(d.lad, 5), 
    d.lad.lead1  = dplyr::lead(d.lad), 
    d.lad.lead2 = dplyr::lead(d.lad, 2), 
    d.lad.lead3 = dplyr::lead(d.lad, 3), 
    d.lad.lead4 = dplyr::lead(d.lad, 4), 
    d.lad.lead5 = dplyr::lead(d.lad, 5),
    supp.lab = ifelse(labour == 1 & vote1 == 1, 1, 
                      ifelse(year %in% 1993:2008, 0,
                             NA)),  # added the year constraint to reflect definition of e.pdf.lab01 dataset (only looking after 1992 general election)
    supp.lab.lag1   = dplyr::lag(supp.lab),  
    supp.lab.lag2 = dplyr::lag(supp.lab, 2), 
    supp.lab.lag3 = dplyr::lag(supp.lab, 3), 
    supp.lab.lag4 = dplyr::lag(supp.lab, 4), 
    supp.lab.lag5 = dplyr::lag(supp.lab, 5), 
    supp.lab.lead1   = dplyr::lead(supp.lab), 
    supp.lab.lead2 = dplyr::lead(supp.lab, 2), 
    supp.lab.lead3 = dplyr::lead(supp.lab, 3), 
    supp.lab.lead4 = dplyr::lead(supp.lab, 4), 
    supp.lab.lead5 = dplyr::lead(supp.lab, 5),
    
    supp.con = ifelse(conservative == 1 & vote1 == 1, 1, 
                      ifelse(year %in% 1993:2008, 0,
                             NA)),  
    supp.con.lag1   = dplyr::lag(supp.con),  
    supp.con.lag2 = dplyr::lag(supp.con, 2), 
    supp.con.lag3 = dplyr::lag(supp.con, 3), 
    supp.con.lag4 = dplyr::lag(supp.con, 4), 
    supp.con.lag5 = dplyr::lag(supp.con, 5), 
    supp.con.lead1   = dplyr::lead(supp.con), 
    supp.con.lead2 = dplyr::lead(supp.con, 2), 
    supp.con.lead3 = dplyr::lead(supp.con, 3), 
    supp.con.lead4 = dplyr::lead(supp.con, 4), 
    supp.con.lead5 = dplyr::lead(supp.con, 5))


## store datasets as 'pdata.frame' objects recognized in the plm package
pdf <- pdata.frame(df, index = c("pid", "year"))
recoded.lead.lag.df <- recoded.lead.lag.df %>% arrange(pid, year) 
recoded.lead.lag.df <- pdata.frame(recoded.lead.lag.df, index = c("pid", "year"))
### restricting to pids who reported an eviction in any data year
recoded.lead.lag.e.pdf <- recoded.lead.lag.df %>%
  group_by(pid) %>%
  mutate(ever.evict = ifelse(sum(evicted, na.rm = TRUE) > 0, 1, 0)) %>% 
  subset(ever.evict == 1) %>%
  mutate(ever.evict = NULL) %>%
  ungroup() %>%
  pdata.frame(index = c("pid", "year"))
rm(recoded.lead.lag.df)

write.csv(recoded.lead.lag.e.pdf, file = "ukhls/bhps_panel_evictees.csv",
          row.names = FALSE)
