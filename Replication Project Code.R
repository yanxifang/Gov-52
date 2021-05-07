########################################
#### Replication Project
#### GOV 52, Spring 2021
#### Yanxi Fang, Harvard University
########################################

# packages
library(tidyverse)
library(gridExtra)
library(stargazer)
library(forcats)
library(zoo)
library(xtable)
library(plm)
library(multiwayvcov)

# set working directory
setwd("D:/User/Files/Academics/Spring 2021/GOV 52/Replication Project/replication")

###################################################################
#### FIGURE 1: Real House Prices vs. Public Investment in Housing
###################################################################

# read data
real_house_prices <- read.csv("data/Figure-1-RealHousePriceIndex.csv")
public_expenditures <- read.csv("data/Figure-1-PublicInvestment.csv")

# clean and modify house-price data
real_house_prices$Quarter <- substr(real_house_prices$Quarter, 6, 7)
real_house_prices <- real_house_prices %>% #convert quarters to decimals
  mutate(year_qtr = ifelse(Quarter == "Q1", Year,
                    ifelse(Quarter == "Q2", Year+0.25,
                    ifelse(Quarter == "Q3", Year+0.50, Year+0.75))))
real_house_prices <- real_house_prices %>% #convert prices relative to 2001/Q1
  mutate(index_price_01 = 100*RealHousePriceIndex/82.75)

# clean and modify public-expenditures data
public_expenditures <- public_expenditures %>% #total share of GDP
  mutate(total_spending = PublicCapitalTransfers + DirectInvestment)

# plot data
figure1a <- ggplot(data = real_house_prices, aes(x = year_qtr, y = index_price_01)) +
                   geom_line(color = "midnightblue", size = 2) +
                   xlim(1996, 2021) +
                   ylim(50, 150) +
                   xlab("Year") + ylab("Percent of Index in 2001") +
                   ggtitle("Real House Price Index") +
                   geom_hline(yintercept = 100, lty = 2, color = "black", size = 1) +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))


figure1b <- ggplot(data = public_expenditures, aes(x = Year, y = total_spending)) +
                   geom_line(color = "midnightblue", size = 2) +
                   xlim(2000, 2020) +
                   ylim(0, 0.25) +
                   xlab("Year") + ylab("Percent of GDP") +
                   ggtitle("Public Investment in Housing") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

grid.arrange(figure1a, figure1b, ncol = 2,
             top = "Figure 1: Real House Prices vs Public Investment in Housing in OECD Countries")

figure1final <- grid.arrange(figure1a, figure1b, ncol = 2,
                             top = "Figure 1: Real House Prices vs Public Investment in Housing in OECD Countries")

ggsave("figure_1.png", figure1final, height = 4, width = 8)


###################################################################
#### FIGURE 2: Selected Housing Trends in London and England
###################################################################

# read data for 1st panel
low_incomes <- read.csv("data/Figure-2-IncomeLowQuartile.csv",
                        na.strings = c("-", "#"))
sl_rents <- read.csv("data/Figure-2-SL-Rents.csv", na.strings = "-")
la_rents <- read.csv("data/Figure-2-LA-Rents.csv",
                     na.strings = c("-", ".", "LSVT"))

# clean and modify data for 1st panel
low_incomes_long <- low_incomes %>%
  gather(year, income, X1999:X2019)
low_incomes_long$year <- substr(low_incomes_long$year, 2, 5)

sl_rents_long <- sl_rents %>%
  gather(year, sl_rent, X1997:X2019)
sl_rents_long$year <- substr(sl_rents_long$year, 2, 5)

sl_rents_income <- low_incomes_long %>%
  left_join(sl_rents_long, by = c("NewCode", "Area", "year")) %>%
  filter(Area == "London") %>%
  mutate(sl_rent_na = na.approx(sl_rent, na.rm = FALSE),
         sl_rent_real = sl_rent_na / income)

la_rents_long <- la_rents %>%
  gather(year, la_rent, X1997.98:X2018.2019)

la_rents_long$year <- substr(la_rents_long$year, 2, 5)

la_rents_income <- low_incomes_long %>%
  left_join(la_rents_long, by = c("NewCode", "Area", "year")) %>%
  filter(Area == "London") %>%
  mutate(la_rent_na = na.approx(la_rent, na.rm = FALSE),
         la_rent_real = la_rent_na / income)

figure2a_data <- sl_rents_income %>%
  left_join(la_rents_income, by = c("NewCode", "Area", "year")) %>%
  filter(year != 2019)

figure2a_data <- figure2a_data %>% #baseline year: 1999
  mutate(sl_rent_real_relative = 100*sl_rent_real/0.003685519) %>%
  mutate(la_rent_real_relative = 100*la_rent_real/0.003604723)

figure2a_data <- figure2a_data %>%
  select(Area, year, sl_rent_real_relative, la_rent_real_relative) %>%
  gather(variable, value, sl_rent_real_relative:la_rent_real_relative)

figure2a_data$year <- as.numeric(figure2a_data$year)

# plot 1st panel
figure2a <- ggplot(data = figure2a_data, aes(x = year, y = value, color = variable)) +
                   geom_line(aes(group = variable), size = 2) +
                   scale_color_manual(labels = c("Local Authorities", "Social Landlords"),
                                      values = c("cornflowerblue", "midnightblue")) +
                   scale_x_continuous(limits = c(1999, 2019)) +
                   scale_y_continuous(limits = c(60, 160)) +
                   xlab("Year") + ylab("Percent Increase (relative to 1999)") +
                   ggtitle("Real Social Rents in London") +
                   geom_hline(yintercept = 100, lty = 2, color = "black", size = 1) +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

# read data for 2nd panel
all_dwell <- read.csv("data/Figure-2-AllDwellings.csv")
social_dwell <- read.csv("data/Figure-2-CouncilHousingStock.csv")

# clean and modify data for 2nd panel
all_dwell_long <- all_dwell %>%
  gather(year, stock, X2001:X2018)
all_dwell_long$year <- substr(all_dwell_long$year, 2, 5)
all_dwell_series <- all_dwell_long %>%
  group_by(year) %>%
  summarize(allstock = sum(stock))

social_dwell <- social_dwell %>%
  rename(Area = Borough) %>%
  rename(NewCode = CurrentONSCode)
social_dwell_long <- social_dwell %>%
  gather(year, stock_p, X1994:X2020)
social_dwell_long$year <- substr(social_dwell_long$year, 2, 5)
social_dwell_series <- social_dwell_long %>%
  group_by(year) %>%
  summarize(pubstock = sum(stock_p))

figure2b_data <- all_dwell_series %>%
  left_join(social_dwell_series, by = "year") %>%
  mutate(allstock_relative = 100*allstock/3090402) %>%
  mutate(pubstock_relative = 100*pubstock/1060030)

figure2b_data <- figure2b_data %>%
  select(year, allstock_relative, pubstock_relative) %>%
  gather(variable, stock, allstock_relative:pubstock_relative)

figure2b_data$year <- as.numeric(figure2b_data$year)

# plot 2nd panel
figure2b <- ggplot(data = figure2b_data, aes(x = year, y = stock, color = variable)) +
                   geom_line(aes(group = variable), size = 2) +
                   scale_color_manual(labels = c("All Housing", "Council Housing"),
                                      values = c("cornflowerblue", "midnightblue")) +
                   scale_x_continuous(limits = c(2000, 2020)) +
                   scale_y_continuous(limits = c(60, 160)) +
                   xlab("Year") + ylab("Percent Increase (relative to 2001)") +
                   ggtitle("Housing Stock in London") +
                   geom_hline(yintercept = 100, lty = 2, color = "black", size = 1) +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

# read data for 3rd panel
repossess <- read.csv("data/Figure-2-Repossessions.csv")

# clean and modify data for 3rd panel
repossess <- repossess %>%
  separate(Quarter, c("year", "quarter"), sep = "-")
repossess$quarter <- substr(repossess$quarter, 2, 2)
repossess <- repossess %>%
  mutate(period = as.numeric(year) + (as.numeric(quarter) - 1)/4)
repossess <- repossess %>%
  mutate(repossess_lon = 100*RepossessionsLondon/293) %>%
  mutate(repossess_eng = 100*RepossessionsExclLondon/556)

# plot 3rd panel
figure2c_data <- repossess %>%
  select(period, repossess_lon, repossess_eng) %>%
  gather(variable, relative_repossess, repossess_lon:repossess_eng)

figure2c <- ggplot(data = figure2c_data, aes(x = period, y = relative_repossess, 
                                             color = variable)) +
                   geom_line(aes(group = variable), size = 2) +
                   scale_color_manual(labels = c("England", "London"),
                     values = c("cornflowerblue", "midnightblue")) +
                   scale_x_continuous(limits = c(2003, 2020)) +
                   scale_y_continuous(limits = c(75, 250)) +
                   xlab("Year") + ylab("Percent Increase (relative to 2003)") +
                   ggtitle("Private Landlord Repossessions") +
                   geom_hline(yintercept = 100, lty = 2, color = "black", size = 1) +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

# read data for 4th panel
median_inc <- read.csv("data/Figure-2-IncomeMedian.csv", na.strings = "#")
priv_rents <- read.csv("data/Figure-2-PrivateRentsIndex.csv")

# clean and modify data for 4th panel
median_inc_long <- median_inc %>%
  gather(year, median_inc, X1999:X2019)
median_inc_long$year <- substr(median_inc_long$year, 2, 5)
median_inc_long <- median_inc_long %>%
  filter(Area == "London" | Area == "England")
median_inc_engl <- median_inc_long %>%
  filter(Area == "England")
median_inc_lond <- median_inc_long %>%
  filter(Area == "London")
median_inc_wide <- median_inc_engl %>%
  left_join(median_inc_lond, by = c("year"))
median_inc_wide <- median_inc_wide %>%
  select(year, median_inc.x, median_inc.y) %>%
  rename(median_inc_eng = median_inc.x) %>%
  rename(median_inc_lon = median_inc.y)
median_inc_wide$year <- as.numeric(median_inc_wide$year)

priv_rents$month <- substring(priv_rents$month, 1, 3)
priv_rents$month_number <- fct_recode(as.factor(priv_rents$month),
                                      "1" = "Jan",
                                      "2" = "Feb",
                                      "3" = "Mar",
                                      "4" = "Apr",
                                      "5" = "May",
                                      "6" = "Jun",
                                      "7" = "Jul",
                                      "8" = "Aug",
                                      "9" = "Sep",
                                      "10" = "Oct",
                                      "11" = "Nov",
                                      "12" = "Dec")
priv_rents$month_fraction <- (as.numeric(as.character(priv_rents$month_number)) - 1)/12

figure2d_data <- priv_rents %>%
  left_join(median_inc_wide, by = "year")
figure2d_data <- figure2d_data %>%
  mutate(period = year + month_fraction)
figure2d_data <- figure2d_data %>%
  mutate(real_rent_lon = rents_london/median_inc_lon) %>%
  mutate(real_rent_eng = rents_england/median_inc_eng)
figure2d_data <- figure2d_data %>%
  select(period, real_rent_lon, real_rent_eng) %>%
  mutate(rel_rent_lon = 100*real_rent_lon/0.002526605) %>%
  mutate(rel_rent_eng = 100*real_rent_eng/0.003500859)

# plot panel 4
figure2d_data <- figure2d_data %>%
  select(period, rel_rent_lon, rel_rent_eng) %>%
  gather(variable, relative_rent, rel_rent_lon:rel_rent_eng)

figure2d <- ggplot(data = figure2d_data, aes(x = period, y = relative_rent, 
                                             color = variable)) +
                   geom_line(aes(group = variable), size = 2) +
                   scale_color_manual(labels = c("England", "London"),
                                      values = c("cornflowerblue", "midnightblue")) +
                   scale_x_continuous(limits = c(2005, 2020)) +
                   scale_y_continuous(limits = c(95, 120)) +
                   xlab("Year") + ylab("Percent Increase (relative to 2005)") +
                   ggtitle("Real Private Rents") +
                   geom_hline(yintercept = 100, lty = 2, color = "black", size = 1) +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

# read data for 5th panel
house_prices <- read.csv("data/Figure-2-HousePrices.csv")

# clean and modify data for 5th panel
house_prices$Quarter.month <- substring(house_prices$Quarter.month, 1, 3)
house_prices$Quarter.month <- fct_recode(as.factor(house_prices$Quarter.month),
                                         "1" = "Q1",
                                         "4" = "Q2",
                                         "7" = "Q3",
                                         "10" = "Q4",
                                         "1" = "Jan",
                                         "2" = "Feb",
                                         "3" = "Mar",
                                         "4" = "Apr",
                                         "5" = "May",
                                         "6" = "Jun",
                                         "7" = "Jul",
                                         "8" = "Aug",
                                         "9" = "Sep",
                                         "10" = "Oct",
                                         "11" = "Nov",
                                         "12" = "Dec")
house_prices$England <- substr(house_prices$England, 3, 9)
house_prices$London <- substr(house_prices$London, 3, 9)
house_prices <- house_prices %>%
  mutate(month_num = (as.numeric(as.character(Quarter.month))-1)/12)
house_prices$England <- as.numeric(gsub(",", "", house_prices$England))
house_prices$London <- as.numeric(gsub(",", "", house_prices$London))
house_prices <- house_prices %>%
  filter(Year >= 1999) # for consistency with the rest of the panels
house_prices <- house_prices %>%
  mutate(period = Year + month_num)
house_prices <- house_prices %>%
  mutate(eng_rel = 100*England/112428) %>%
  mutate(lon_rel = 100*London/179581)

# plot 5th panel
figure2e_data <- house_prices %>%
  select(period, eng_rel, lon_rel) %>%
  gather(variable, house_price, eng_rel:lon_rel)

figure2e <- ggplot(data = figure2e_data, aes(x = period, y = house_price, color = variable)) +
                   geom_line(aes(group = variable), size = 2) +
                   scale_color_manual(labels = c("England", "London"),
                                      values = c("cornflowerblue", "midnightblue")) +
                   scale_x_continuous(limits = c(1999, 2021)) +
                   scale_y_continuous(limits = c(99, 280)) +
                   xlab("Year") + ylab("Percent Increase (relative to 1999)") +
                   ggtitle("House Prices") +
                   geom_hline(yintercept = 100, lty = 2, color = "black", size = 1) +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

# read data for 6th panel
lon_home_own <- read.csv("data/Figure-2-HomeOwnership.csv")

# clean and modify data for 6th panel
lon_home_own <- lon_home_own %>%
  filter(Year >= 1999) %>%
  select(Year, Owned.total, Rented.total)

figure2f_data <- lon_home_own %>%
  select(Year, Owned.total, Rented.total) %>%
  gather(variable, percentage, Owned.total:Rented.total)

# plot 6th panel
figure2f <- ggplot(data = figure2f_data, aes(x = Year, y = percentage, color = variable)) +
                   geom_line(aes(group = variable), size = 2) +
                   scale_color_manual(labels = c("% Owning", "% Renting"),
                                      values = c("cornflowerblue", "midnightblue")) +
                   scale_x_continuous(limits = c(1999, 2021)) +
                   scale_y_continuous(limits = c(30, 70)) +
                   xlab("Year") + ylab("Absolute Percentage") +
                   ggtitle("Home Ownership Rates in London") +
                   geom_hline(yintercept = 100, lty = 2, color = "black", size = 1) +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

# bring everything together in Figure 2
grid.arrange(figure2a, figure2b, figure2c, figure2d, figure2e, figure2f, ncol = 3,
             top = "Figure 2: Housing Trends in London and England")

figure2final <- grid.arrange(figure2a, figure2b, figure2c, figure2d, figure2e, figure2f, ncol = 3,
                             top = "Figure 2: Housing Trends in London and England")

ggsave("figure_2.png", figure2final, height = 8, width = 12)


###########################################################################
#### FIGURE 3: Decline of Publicly-Owned Council Housing in Greater London
###########################################################################
# read data
pol_contr <- read.csv("data/politicalControl.csv") #political control
hsg_stock <- read.csv("data/Figure-3-CouncilHousingStock.csv")
rtb_sales <- read.csv("data/Figure-3-RTB-Sales.csv", na.strings = "..") #right-to-buy

# clean and manipulate data
labour_boros <- pol_contr %>%
  filter(year >= 1998 | year <= 2017) %>%
  mutate(labour = ifelse(control == "Labour", 1, 0)) %>%
  group_by(borough) %>%
  summarize(labour = mean(labour)) %>%
  filter(labour > 2/3) %>%
  pull(borough)
conserv_boros <- pol_contr %>% 
  filter(year >= 1998 | year <= 2017) %>%
  mutate(conservative =
           ifelse(control %in% c("Conservative", "Liberal Democrat"), 1, 0)) %>%
  group_by(borough) %>%
  summarise(conservative = mean(conservative)) %>%
  filter(conservative > 2/3) %>%
  pull(borough)
mixed_boros <- pol_contr %>%
  filter(year >= 1998 | year <= 2017) %>%
  pull(borough) %>%
  unique() %>%
  setdiff(labour_boros) %>%
  setdiff(conserv_boros)

hsg_stock_long <- hsg_stock %>%
  gather(year, stock, X1994:X2020)
hsg_stock_long$year <- substr(hsg_stock_long$year, 2, 5)
  
rtb_sales[is.na(rtb_sales)] <- 0
rtb_sales_long <- rtb_sales %>%
  gather(year, rtb_units, X1998.99:X2017..18)
rtb_sales_long$year <- as.numeric(str_extract(rtb_sales_long$year, pattern = "[0-9]+"))

hsg_stock_long$year <- as.numeric(hsg_stock_long$year)
rtb_sales_long$year <- as.numeric(rtb_sales_long$year)
all_hsg_sales <- hsg_stock_long %>%
  left_join(rtb_sales_long, by = c("year", "Area")) %>%
  mutate(all_sales = stock + rtb_units)

labour_series <- all_hsg_sales %>%
  filter(Area %in% labour_boros) %>%
  group_by(year) %>%
  summarise(all_sales = sum(all_sales))

conservative_series <- all_hsg_sales %>%
  filter(Area %in% conserv_boros) %>%
  group_by(year) %>%
  summarise(all_sales = sum(all_sales))

mixed_series <- all_hsg_sales %>%
  filter(Area %in% mixed_boros) %>%
  group_by(year) %>%
  summarise(all_sales = sum(all_sales))

london_series <- all_hsg_sales %>%
  group_by(year) %>%
  summarise(all_sales = sum(all_sales))

london_series <- london_series %>%
  mutate(party = "london_total") %>%
  rename(house_stock = all_sales) %>%
  mutate(adj_house_stock = house_stock/1000) %>%
  filter(year >= 1998 & year <= 2017)

conservative_series <- conservative_series %>%
  rename(cons_total = all_sales)
labour_series <- labour_series %>%
  rename(labr_total = all_sales)
mixed_series <- mixed_series %>%
  rename(mixd_total = all_sales)
combined_series <- conservative_series %>%
  left_join(labour_series, by = "year") %>%
  left_join(mixed_series, by = "year")

# plot figure 3
figure3a_data <- combined_series %>%
  select(year, cons_total, labr_total, mixd_total) %>%
  gather(party, house_stock, cons_total:mixd_total) %>%
  filter(year >= 1998 & year <= 2017)

figure3a_data <- figure3a_data %>%
  mutate(adj_house_stock = house_stock/1000)

figure3a <- ggplot(data = figure3a_data, aes(x = year, y = adj_house_stock, color = party)) +
                   geom_line(aes(group = party), size = 2) +
                   scale_color_manual(labels = c("Conservative", "Labour", "Mixed"),
                                      values = c("cornflowerblue", "midnightblue", "slategray4")) +
                   scale_x_continuous(limits = c(1998, 2018)) +
                   scale_y_continuous(limits = c(0, 400)) +
                   xlab("Year") + ylab("Number of Homes (Thousands)") +
                   ggtitle("Number of Council Homes") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

figure3b_data <- rbind(figure3a_data, london_series)

figure3b_data <- figure3b_data %>%
  mutate(initial_stock = ifelse(party == "cons_total", 79361,
                                ifelse(party == "mixd_total", 174379, 
                                       ifelse(party == "labr_total", 347329, 1203333))))
figure3b_data <- figure3b_data %>%
  mutate(pctage = 100*house_stock/initial_stock)

figure3b <- ggplot(data = figure3b_data, aes(x = year, y = pctage, color = party)) +
                   geom_line(aes(group = party), size = 2) +
                   scale_color_manual(labels = c("Conservative", "Labour", "London", "Mixed"),
                                      values = c("cornflowerblue", "midnightblue", "firebrick2", "slategray4")) +
                   scale_x_continuous(limits = c(1998, 2018)) +
                   scale_y_continuous(limits = c(60, 100)) +
                   xlab("Year") + ylab("Percent of Initial Stock") +
                   ggtitle("Council Homes: Percentage of Initial Stock") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

# bring everything together in Figure 3
grid.arrange(figure3a, figure3b, ncol = 2,
             top = "Figure 3: Decline of Publicly-Owned Council Housing in Greater London")

figure3final <- grid.arrange(figure3a, figure3b, ncol = 2,
                             top = "Figure 3: Decline of Publicly-Owned Council Housing in Greater London")

ggsave("figure_3.png", figure3final, height = 4, width = 8)

###########################################################################
#### FIGURE 4: Number of Guardian Articles Mentioning "Social Cleansing"
####                 in Relation to Housing in the UK, 1992-2019
###########################################################################

# read data for figure 4
guardian <- read.csv("data/Figure-4-Guardian.csv")

# manipulate data
guardian_grouped <- guardian %>%
  filter(is.na(validate) | validate == 1) %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  filter(Year < 2020)

# plot data
figure_4 <- ggplot(data = guardian_grouped, aes(x = Year, y = count)) +
                   geom_line(size = 2, color = "cornflowerblue") + 
                   geom_smooth(method = "loess", size = 2, color = "midnightblue", se = FALSE, lty = 2) +
                   scale_x_continuous(limits = c(1990, 2020)) +
                   scale_y_continuous(limits = c(-5, 70)) +
                   xlab("Year") + ylab("Articles Mentioning \"Social Cleansing\"") +
                   ggtitle("Figure 4: Number of *Guardian* Articles Mentioning \"Social Cleansing\" in Relation to Housing") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

ggsave("figure_4.png", figure_4, height = 4, width = 8)

#############################################################################
#### FIGURE 7: Support for the Labour Party Over Time by Socioeconomic Group
#############################################################################

# read data (generated from make_bsas_appended.R)
bsas_data <- read.csv("data/bsas_appended.csv",
                      na.strings = c("", ".", "NA"))

# clean and modify data
bsas_data$ses_grp <- fct_recode(bsas_data$ses_grp, 
                                  "I. Professional, employer, manager" = "professional, employer, manager",
                                  "II. Intermediate non-manual"= "intermediate non-manual",
                                  "III. Junior non-manual" = "junior non-manual",
                                  "IV. Skilled manual"="skilled manual",
                                  "V. Semi-skilled"="semi-skilled",
                                  "VI. Unskilled manual"="unskilled manual")

df_year_sesgrp_labour_weighted <- bsas_data %>% 
  filter(!is.na(ses_grp)) %>%
  group_by(year, ses_grp) %>%
  summarize("pct_labour"=sum(wtfactor[labour == 1], na.rm=TRUE) / 
              sum(wtfactor, na.rm=TRUE)) %>%
  as.data.frame()

df_year_sesgrp_labour_weighted <- df_year_sesgrp_labour_weighted %>%
  mutate(percent_labour = pct_labour*100)

figure_7a_data <- df_year_sesgrp_labour_weighted %>%
  filter(ses_grp == "I. Professional, employer, manager")
figure_7b_data <- df_year_sesgrp_labour_weighted %>%
  filter(ses_grp == "II. Intermediate non-manual")
figure_7c_data <- df_year_sesgrp_labour_weighted %>%
  filter(ses_grp == "III. Junior non-manual")
figure_7d_data <- df_year_sesgrp_labour_weighted %>%
  filter(ses_grp == "IV. Skilled manual")
figure_7e_data <- df_year_sesgrp_labour_weighted %>%
  filter(ses_grp == "V. Semi-skilled")
figure_7f_data <- df_year_sesgrp_labour_weighted %>%
  filter(ses_grp == "VI. Unskilled manual")

# plot data
figure7a <- ggplot(data = figure_7a_data, aes(x = year, y = percent_labour)) +
                   geom_point(size = 4, color = "cornflowerblue") + 
                   geom_smooth(method = "loess", size = 2, color = "midnightblue", se = FALSE) +
                   scale_x_continuous(limits = c(1980, 2020)) +
                   scale_y_continuous(limits = c(0, 60)) +
                   xlab("Year") + ylab("% Supporting Labour") +
                   ggtitle("I. Professional, Employer, Manager") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

figure7b <- ggplot(data = figure_7b_data, aes(x = year, y = percent_labour)) +
                   geom_point(size = 4, color = "cornflowerblue") + 
                   geom_smooth(method = "loess", size = 2, color = "midnightblue", se = FALSE) +
                   scale_x_continuous(limits = c(1980, 2020)) +
                   scale_y_continuous(limits = c(0, 60)) +
                   xlab("Year") + ylab("% Supporting Labour") +
                   ggtitle("II. Intermediate Non-Manual") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

figure7c <- ggplot(data = figure_7c_data, aes(x = year, y = percent_labour)) +
                   geom_point(size = 4, color = "cornflowerblue") + 
                   geom_smooth(method = "loess", size = 2, color = "midnightblue", se = FALSE) +
                   scale_x_continuous(limits = c(1980, 2020)) +
                   scale_y_continuous(limits = c(0, 60)) +
                   xlab("Year") + ylab("% Supporting Labour") +
                   ggtitle("III. Junior Non-Manual") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

figure7d <- ggplot(data = figure_7d_data, aes(x = year, y = percent_labour)) +
                   geom_point(size = 4, color = "cornflowerblue") + 
                   geom_smooth(method = "loess", size = 2, color = "midnightblue", se = FALSE) +
                   scale_x_continuous(limits = c(1980, 2020)) +
                   scale_y_continuous(limits = c(0, 60)) +
                   xlab("Year") + ylab("% Supporting Labour") +
                   ggtitle("IV. Skilled Manual") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

figure7e <- ggplot(data = figure_7e_data, aes(x = year, y = percent_labour)) +
                   geom_point(size = 4, color = "cornflowerblue") + 
                   geom_smooth(method = "loess", size = 2, color = "midnightblue", se = FALSE) +
                   scale_x_continuous(limits = c(1980, 2020)) +
                   scale_y_continuous(limits = c(0, 60)) +
                   xlab("Year") + ylab("% Supporting Labour") +
                   ggtitle("V. Semi-Skilled Manual") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

figure7f <- ggplot(data = figure_7f_data, aes(x = year, y = percent_labour)) +
                   geom_point(size = 4, color = "cornflowerblue") + 
                   geom_smooth(method = "loess", size = 2, color = "midnightblue", se = FALSE) +
                   scale_x_continuous(limits = c(1980, 2020)) +
                   scale_y_continuous(limits = c(0, 60)) +
                   xlab("Year") + ylab("% Supporting Labour") +
                   ggtitle("VI. Unskilled Manual") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

# bring everything together in Figure 7
grid.arrange(figure7a, figure7b, figure7c, figure7d, figure7e, figure7f, ncol = 3,
             top = "Figure 7: Support for the Labour Party Over Time, by Socioeconomic Group (1983-2018)")

figure7final <- grid.arrange(figure7a, figure7b, figure7c,
                             figure7d, figure7e, figure7f,
                             ncol = 3,
                             top = "Figure 7: Support for the Labour Party Over Time, by Socioeconomic Group (1983-2018)")

ggsave("figure_7.png", figure7final, height = 8, width = 12)


###########################################################################
#### TABLE 2: Class Composition In/Outside London Over Time, 1983-2018 (%)
###########################################################################

# use bsas_data from Figure 7 above
# modify and manipulate data
# note: authors' original code used here
table2_data_long <- bsas_data %>%
  filter(!is.na(ses_grp)) %>%
  mutate(decade = paste0(substr(year, 1, 3), "0s")) %>%
  group_by(decade, london, ses_grp) %>%
  summarise(n = sum(wtfactor)) %>%
  group_by(decade, london) %>%
  mutate(prop = round(n / sum(n) * 100)) %>%
  select(decade, london, ses_grp, prop)

table2_data <- table2_data_long %>%
  data.frame() %>%
  reshape(direction = "wide",
          idvar = c("london", "ses_grp"),
          timevar = "decade")

table2_data$ses_grp <- factor(table2_data$ses_grp,
                             levels = c("I. Professional, employer, manager",
                                        "II. Intermediate non-manual",
                                        "III. Junior non-manual",
                                        "IV. Skilled manual",
                                        "V. Semi-skilled",
                                        "VI. Unskilled manual"))
table2_data$london_str <- ifelse(table2_data$london > 0, "London",
                                "Outside London")

table2 <- xtable(
  table2_data %>%
    select(ses_grp, london_str, prop.1980s,
           prop.1990s, prop.2000s, prop.2010s) %>%
    arrange(ses_grp, london_str) %>%
    setNames(nm = c("", "", "1980s", "1990s", "2000s", "2010s")),
  digits = 0
)

print(table2, include.rownames = FALSE, file = "Table-2.tex")


#############################################################################
#### FIGURE 8: Labour's Coalition in London Over Time by Socioeconomic Group
#############################################################################

# use bsas_data from figure 7 above
# modify and manipulate data
# note: data manipulation is the authors' code
figure8_data <- bsas_data %>%
  filter(london == 1, labour == 1, !is.na(ses_grp)) %>%
  mutate(decade = paste0(substr(year, 1, 3), "0s")) %>%
  group_by(decade, ses_grp) %>%
  summarise(n = sum(wtfactor, na.rm = TRUE)) %>%
  group_by(decade) %>%
  mutate(p = n / sum(n))

figure8_data$pct <- 100*(figure8_data$p)

figure8_data$ses_grp <- factor(figure8_data$ses_grp,
                               levels = c("I. Professional, employer, manager",
                                          "II. Intermediate non-manual",
                                          "III. Junior non-manual",
                                          "IV. Skilled manual",
                                          "V. Semi-skilled",
                                          "VI. Unskilled manual"))

# plot the data (new code)
figure_8 <- ggplot(data = figure8_data, aes(x = decade, y = pct, color = ses_grp)) +
                   geom_line(aes(group = ses_grp), size = 2) +
                   geom_point(aes(color = ses_grp), size = 4) +
                   scale_color_manual(values = c("cornflowerblue", "midnightblue",
                                                "orange", "grey50", "springgreen4", "grey0")) +
                   scale_y_continuous(limits = c(0, 35)) +
                   xlab("Year") + ylab("% of Labour Supporters") +
                   ggtitle("Figure 8: Labour's Coalition in London Over Time by Socioeconomic Group, 1983-2018") +
                   theme_bw() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         legend.title = element_blank()) +
                   theme(plot.title = element_text(size = 10, hjust = 0.5),
                         axis.title = element_text(size = 10))

ggsave("figure_8.png", figure_8, height = 4, width = 8)


################################################################
#### TABLE 3: Correlates of Council Housing Reduction in Wards
################################################################

# read data
ward_data <- read.csv("data/Ward Level Data (most recent).csv")

# clean and manipulate data
ward_data <- ward_data %>% # convert NAs to 0s
  mutate(labseats06 = ifelse(is.na(labseats06), 0, labseats06))

ward_data <- ward_data %>% # standardize two variables
  mutate(log_medincome_z = scale(log_medincome01)) %>%
  mutate(log_crime_z = scale(lcrime01))

# group by borough/ward for fixed effects
table3_data <- pdata.frame(ward_data, index = c("borough", "ward"))

# model 1: simple  model
# four explanatory variables: 2001 concentration, 2006 labour strength, log crime, log median income
# also includes borough fixed effects
table3_model1 <- plm(council11 ~ council01 + labseats06 + log_crime_z + log_medincome_z,
                     model = "within", data = table3_data)
summary(table3_model1)

# model 2: model with interactions
# adding the interaction between labour strength and each of: log crime rate, log median income
# also includes borough fixed effects
table3_model2 <- plm(council11 ~ council01 + labseats06 + log_crime_z + log_medincome_z +
                                 labseats06*log_crime_z + labseats06*log_medincome_z,
                     model = "within", data = table3_data)
summary(table3_model2)

# model 3: complex model
# adding measures of "pragmatic privatization", housing demand indicators, demographic indicators
table3_model3 <- plm(council11 ~ council01 + labseats06 + log_crime_z + log_medincome_z + #basic model
                                 labseats06*log_crime_z + labseats06*log_medincome_z +    #interactions
                                 unemp01 + p.inc.supp01 + black01 + asian01 +             #demographics
                                 private01 + log_sales01 + log_price01 +                  #housing mkt. indicators
                                 log_dens01 + log_hholds01 + over.private01 +             #housing demand indicators
                                 noheat01 + ownbath01 + block01 + over.council01 +        #characteristics of council hsg.
                                 p.black.council01 + p.asian.council01 + low.dwell01,
                     model = "within", data = table3_data)
summary(table3_model3)

# use stargazer to summarize numerical estimates
stargazer(table3_model1, table3_model2, table3_model3, 
          dep.var.caption = "Council Housing Concentration in 2011",
          se = list(sqrt(diag(vcovHC(table3_model1, cluster = "group"))), 
                    sqrt(diag(vcovHC(table3_model2, cluster = "group"))), 
                    sqrt(diag(vcovHC(table3_model3, cluster = "group")))), 
          style = "apsr", 
          digits = 3, 
          column.labels = rep("All Boroughs", 4), 
          dep.var.labels = rep("", 4), 
          omit.stat = "f",
          out = "Table-3.tex")

# note: additional modifications were made to the stargazer table after it was generated
# specifically, for model 3, none of the covariates are reported, with the
    # exception of four council housing characteristics used by the authors

#######################################################################
#### TABLE 4: Interactions with Labour Strength within Labour Boroughs
#######################################################################

# use same data as table 3
# major difference: consider only labour boroughs instead of all boroughs

# manipulate data
table4_data <- pdata.frame(subset(ward_data, (lab.borough02==1 & lab.borough06 == 1)),
                        index = c("borough", "ward"))
table4_weak <- subset(table4_data, labseats06 < 3 | is.na(labseats06))
table4_strong <- subset(table4_data, labseats06 == 3)

# simple models (no interactions or control variables)
table4_a <- plm(council11 ~ council01 + log_crime_z + log_medincome_z,
                model = "within", data = table4_data)
table4_b <- plm(council11 ~ council01 + log_crime_z + log_medincome_z,
                model = "within", data = table4_weak)
table4_c <- plm(council11 ~ council01 + log_crime_z + log_medincome_z,
                model = "within", data = table4_strong)

# full models (with control variables, similar to model 3 in figure 3)
table4_d <- plm(council11 ~ council01 + log_crime_z + log_medincome_z +              #basic model
                            unemp01 + p.inc.supp01 + black01 + asian01 +             #demographics
                            private01 + log_sales01 + log_price01 +                  #housing mkt. indicators
                            log_dens01 + log_hholds01 + over.private01 +             #housing demand indicators
                            noheat01 + ownbath01 + block01 + over.council01 +        #characteristics of council hsg.
                            p.black.council01 + p.asian.council01 + low.dwell01,
                model = "within", data = table4_data)

table4_e <- plm(council11 ~ council01 + log_crime_z + log_medincome_z +              #basic model
                            unemp01 + p.inc.supp01 + black01 + asian01 +             #demographics
                            private01 + log_sales01 + log_price01 +                  #housing mkt. indicators
                            log_dens01 + log_hholds01 + over.private01 +             #housing demand indicators
                            noheat01 + ownbath01 + block01 + over.council01 +        #characteristics of council hsg.
                            p.black.council01 + p.asian.council01 + low.dwell01,
                model = "within", data = table4_weak)

table4_f <- plm(council11 ~ council01 + log_crime_z + log_medincome_z +              #basic model
                            unemp01 + p.inc.supp01 + black01 + asian01 +             #demographics
                            private01 + log_sales01 + log_price01 +                  #housing mkt. indicators
                            log_dens01 + log_hholds01 + over.private01 +             #housing demand indicators
                            noheat01 + ownbath01 + block01 + over.council01 +        #characteristics of council hsg.
                            p.black.council01 + p.asian.council01 + low.dwell01,
                model = "within", data = table4_strong)

stargazer(table4_a, table4_b, table4_c, table4_d, table4_e, table4_f,
          dep.var.caption = "Council Housing Concentration in 2011",
          se = list(sqrt(diag(vcovHC(table4_a, cluster = "group"))), 
                    sqrt(diag(vcovHC(table4_b, cluster = "group"))), 
                    sqrt(diag(vcovHC(table4_c, cluster = "group"))), 
                    sqrt(diag(vcovHC(table4_d, cluster = "group"))), 
                    sqrt(diag(vcovHC(table4_e, cluster = "group"))), 
                    sqrt(diag(vcovHC(table4_f, cluster = "group")))), 
          style = "apsr", 
          digits = 3, 
          omit.stat = "f",
          out = "Table-4.tex")


#############################################################
#### FIGURE 9: Interactions between Labour Strength, Crime,
####              and Median Income in Labour Boroughs
#############################################################

# use same data as table 3 (ward_data)
# modify data
#all wards in labour boroughs
labour_allwards <- subset(ward_data, (lab.borough02 == 1 & lab.borough06 == 1))
#weak labour wards in labour boroughs
labour_weakwards <- subset(ward_data, 
                          (lab.borough02 == 1 & lab.borough06 == 1) &
                          (labseats06 < 3 | is.na(labseats06)))
#strong labour wards in labour boroughs
labour_strngwards <- subset(ward_data,
                           (lab.borough02 == 1 & lab.borough06 == 1) &
                           (labseats06 == 3))

# run regressions
figure9_lm1 <- lm(council11 ~ council01 + lcrime01 + log_medincome01 + borough,
                  data = labour_allwards)
figure9_lm2 <- lm(council11 ~ council01 + lcrime01 + log_medincome01 + borough,
                  data = labour_weakwards)
figure9_lm3 <- lm(council11 ~ council01 + lcrime01 + log_medincome01 + borough,
                  data = labour_strngwards)
summary(figure9_lm1)
summary(figure9_lm2)
summary(figure9_lm3)

# generate prediction dataframes for crime as the variable
# other predictor variables are held constant at the mean level
figure9_predframe1 <- labour_allwards %>%
  select(ward, council11, lcrime01)
figure9_predframe1$log_medincome01 <- mean(labour_allwards$log_medincome01)   # average log-median-income
figure9_predframe1$council01 <- mean(labour_allwards$council01)               # average council housing in 2001
table(labour_allwards$borough) # Newham has the largest number of wards in the original data, and is considered the average
figure9_predframe1$borough <- "Newham"

figure9_predframe2 <- labour_weakwards %>%
  select(ward, council11, lcrime01)
figure9_predframe2$log_medincome01 <- mean(labour_weakwards$log_medincome01)   # average log-median-income
figure9_predframe2$council01 <- mean(labour_weakwards$council01)               # average council housing in 2001
table(labour_weakwards$borough) # Tower Hamlets has the largest number of wards in the original data, and is considered the average
figure9_predframe2$borough <- "Tower Hamlets"

figure9_predframe3 <- labour_strngwards %>%
  select(ward, council11, lcrime01)
figure9_predframe3$log_medincome01 <- mean(labour_strngwards$log_medincome01)  # average log-median-income
figure9_predframe3$council01 <- mean(labour_strngwards$council01)              # average council housing in 2001
table(labour_strngwards$borough) # Newham has the largest number of wards in the original data, and is considered the average
figure9_predframe3$borough <- "Newham"

# generate prediction dataframes for median income as the variable
# other predictor variables are held constant at the mean level
figure9_predframe4 <- labour_allwards %>%
  select(ward, council11, log_medincome01)
figure9_predframe4$lcrime01 <- mean(labour_allwards$lcrime01)                  # average log crime
figure9_predframe4$council01 <- mean(labour_allwards$council01)                # average council housing in 2001
figure9_predframe4$borough <- "Newham"

figure9_predframe5 <- labour_weakwards %>%
  select(ward, council11, log_medincome01)
figure9_predframe5$lcrime01 <- mean(labour_weakwards$lcrime01)                 # average log crime
figure9_predframe5$council01 <- mean(labour_weakwards$council01)               # average council housing in 2001
figure9_predframe5$borough <- "Tower Hamlets"

figure9_predframe6 <- labour_strngwards %>%
  select(ward, council11, log_medincome01)
figure9_predframe6$lcrime01 <- mean(labour_strngwards$lcrime01)                # average log crime
figure9_predframe6$council01 <- mean(labour_strngwards$council01)              # average council housing in 2001
figure9_predframe6$borough <- "Newham"

# generate predictions: crime as the variable
# for all wards, then weak wards, then strong wards
figure9_pred1 <- as_tibble(predict(figure9_lm1, newdata = figure9_predframe1, interval = "confidence"))
figure9_values1 <- cbind(labour_allwards, figure9_pred1)
figure9_values1 <- figure9_values1 %>%
  select(borough, ward, lcrime01, fit, lwr, upr) %>%
  mutate(crime = exp(lcrime01))

figure9_pred2 <- as_tibble(predict(figure9_lm2, newdata = figure9_predframe2, interval = "confidence"))
figure9_values2 <- cbind(labour_weakwards, figure9_pred2)
figure9_values2 <- figure9_values2 %>%
  select(borough, ward, lcrime01, fit, lwr, upr) %>%
  mutate(crime = exp(lcrime01))

figure9_pred3 <- as_tibble(predict(figure9_lm3, newdata = figure9_predframe3, interval = "confidence"))
figure9_values3 <- cbind(labour_strngwards, figure9_pred3)
figure9_values3 <- figure9_values3 %>%
  select(borough, ward, lcrime01, fit, lwr, upr) %>%
  mutate(crime = exp(lcrime01))

# generate predictions: median income as the variable
figure9_pred4 <- as_tibble(predict(figure9_lm1, newdata = figure9_predframe4, interval = "confidence"))
figure9_values4 <- cbind(labour_allwards, figure9_pred4)
figure9_values4 <- figure9_values4 %>%
  select(borough, ward, log_medincome01, fit, lwr, upr) %>%
  mutate(med_income = exp(log_medincome01))

figure9_pred5 <- as_tibble(predict(figure9_lm2, newdata = figure9_predframe5, interval = "confidence"))
figure9_values5 <- cbind(labour_weakwards, figure9_pred5)
figure9_values5 <- figure9_values5 %>%
  select(borough, ward, log_medincome01, fit, lwr, upr) %>%
  mutate(med_income = exp(log_medincome01))

figure9_pred6 <- as_tibble(predict(figure9_lm3, newdata = figure9_predframe6, interval = "confidence"))
figure9_values6 <- cbind(labour_strngwards, figure9_pred6)
figure9_values6 <- figure9_values6 %>%
  select(borough, ward, log_medincome01, fit, lwr, upr) %>%
  mutate(med_income = exp(log_medincome01))

# plots
figure9_ggplot1 <- ggplot(data = figure9_values1, aes(x = crime, y = fit)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm", color = "midnightblue", size = 2) +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(0, 35)) +
  xlab("Crimes Per 1,000 Residents") + ylab("Predicted 2011 Council Hsg Concentration (%)") +
  ggtitle("All Wards in Labour Boroughs") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10))

figure9_ggplot2 <- ggplot(data = figure9_values2, aes(x = crime, y = fit)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm", color = "midnightblue", size = 2) +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(0, 35)) +
  xlab("Crimes Per 1,000 Residents") + ylab("Predicted 2011 Council Hsg Concentration (%)") +
  ggtitle("Weak Labour Wards in Labour Boroughs") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10))

figure9_ggplot3 <- ggplot(data = figure9_values3, aes(x = crime, y = fit)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm", color = "midnightblue", size = 2) +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(0, 35)) +
  xlab("Crimes Per 1,000 Residents") + ylab("Predicted 2011 Council Hsg Concentration (%)") +
  ggtitle("Strong Labour Wards in Labour Boroughs") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10))

figure9_ggplot4 <- ggplot(data = figure9_values4, aes(x = med_income, y = fit)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm", color = "midnightblue", size = 2) +
  scale_x_continuous(limits = c(18000, 36000)) +
  scale_y_continuous(limits = c(0, 35)) +
  xlab("Median Annual Household Income (GBP)") + ylab("Predicted 2011 Council Hsg Concentration (%)") +
  ggtitle("All Wards in Labour Boroughs") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10))

figure9_ggplot5 <- ggplot(data = figure9_values5, aes(x = med_income, y = fit)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm", color = "midnightblue", size = 2) +
  scale_x_continuous(limits = c(18000, 36000)) +
  scale_y_continuous(limits = c(0, 35)) +
  xlab("Median Annual Household Income (GBP)") + ylab("Predicted 2011 Council Hsg Concentration (%)") +
  ggtitle("Weak Labour Wards in Labour Boroughs") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10))

figure9_ggplot6 <- ggplot(data = figure9_values6, aes(x = med_income, y = fit)) +
  geom_point(color = "cornflowerblue") +
  geom_smooth(method = "lm", color = "midnightblue", size = 2) +
  scale_x_continuous(limits = c(18000, 36000)) +
  scale_y_continuous(limits = c(0, 35)) +
  xlab("Median Annual Household Income (GBP)") + ylab("Predicted 2011 Council Hsg Concentration (%)") +
  ggtitle("Strong Labour Wards in Labour Boroughs") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 10))

grid.arrange(figure9_ggplot1, figure9_ggplot2, figure9_ggplot3, 
             figure9_ggplot4, figure9_ggplot5, figure9_ggplot6, ncol = 3,
             top = "Figure 9: Interactions Between Labour Strength, Crime, and Median Income in Labour Boroughs")

figure9final <- grid.arrange(figure9_ggplot1, figure9_ggplot2, figure9_ggplot3, 
                             figure9_ggplot4, figure9_ggplot5, figure9_ggplot6, ncol = 3,
                             top = "Figure 9: Interactions Between Labour Strength, Crime, and Median Income in Labour Boroughs")

ggsave("figure_9.png", figure9final, height = 8, width = 12)


################################
#### REPLICATION EXTENSION ####
################################

# log transformations to the council housing concentration (2001 and 2011) variables

# use same data from table 3, but group by borough and ward for fixed effects
table5_data <- pdata.frame(ward_data, index = c("borough", "ward"))

# log transformations
table5_data <- table5_data %>%
  mutate(log_council11 = log(council11)) %>%
  mutate(log_council01 = log(council01))

# model 1: simple  model
# four explanatory variables: log of 2001 concentration, 2006 labour strength, log crime, log median income
# also includes borough fixed effects
table5_model1 <- plm(log_council11 ~ log_council01 + labseats06 + log_crime_z + log_medincome_z,
                     model = "within", data = table5_data)
summary(table5_model1)

# model 2: model with interactions
# adding the interaction between labour strength and each of: log crime rate, log median income
# also includes borough fixed effects
table5_model2 <- plm(log_council11 ~ log_council01 + labseats06 + log_crime_z + log_medincome_z +
                       labseats06*log_crime_z + labseats06*log_medincome_z,
                     model = "within", data = table5_data)
summary(table5_model2)

# model 3: complex model
# adding measures of "pragmatic privatization", housing demand indicators, demographic indicators
table5_model3 <- plm(log_council11 ~ log_council01 + labseats06 + log_crime_z + log_medincome_z +
                       labseats06*log_crime_z + labseats06*log_medincome_z +    #interactions
                       unemp01 + p.inc.supp01 + black01 + asian01 +             #demographics
                       private01 + log_sales01 + log_price01 +                  #housing mkt. indicators
                       log_dens01 + log_hholds01 + over.private01 +             #housing demand indicators
                       noheat01 + ownbath01 + block01 + over.council01 +        #characteristics of council hsg.
                       p.black.council01 + p.asian.council01 + low.dwell01,
                     model = "within", data = table5_data)
summary(table5_model3)

# use stargazer to summarize numerical estimates
stargazer(table5_model1, table5_model2, table5_model3, 
          dep.var.caption = "Log of Council Housing Concentration in 2011",
          se = list(sqrt(diag(vcovHC(table5_model1, cluster = "group"))), 
                    sqrt(diag(vcovHC(table5_model2, cluster = "group"))), 
                    sqrt(diag(vcovHC(table5_model3, cluster = "group")))), 
          style = "apsr", 
          digits = 3, 
          column.labels = rep("All Boroughs", 4), 
          dep.var.labels = rep("", 4), 
          omit.stat = "f",
          out = "Table-5.tex")









