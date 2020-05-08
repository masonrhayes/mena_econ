library(tidyverse)
library(readxl)
library(openxlsx)
library(ggthemes)
library(tidyquant)
library(data.table)

# Inequality data for Turkey
turkey_inequality <- read_xlsx("turkey_income_data.xlsx")
view(turkey_inequality)
colnames(turkey_inequality) <- c("percentile", "year", "income_share")

# Greece data
greece_inequality <- read_xlsx("greece_income_data.xlsx")
colnames(greece_inequality) <- c("percentile", "year", "income_share")
greece_inequality <- greece_inequality %>%
  group_by(percentile) %>%
  spread(key = percentile, value = income_share)
colnames(greece_inequality) <- c("year", "bottom50gr", "middle40gr", "top10gr", "top1gr")
# Reformat the data to be more readable
turkey_inequality_formatted <- turkey_inequality %>%
  group_by(percentile) %>%
  spread(key = percentile, value = income_share)
colnames(turkey_inequality_formatted) <- c("year", "bottom50tr", "middle40tr", "top10tr", "top1tr")

view(turkey_inequality_formatted)

# Inequality data for middle east
middle_east_inequality <- read_csv("middle_east_inequality.csv")
colnames(middle_east_inequality) <- c("year", "top1me", "bottom50me", "top10me")
tail(middle_east_inequality)

# Bulgaria data
bulgaria_inequality <- read_xlsx("bulgaria_income_data.xlsx") %>%
  setNames(., c("percentile", "year", "income_share")) %>%
  group_by(percentile) %>%
  spread(key = percentile, value = income_share) %>%
  setNames(., c("year", "bottom50bg", "middle40bg", "top10bg", "top1bg"))
view(bulgaria_inequality)

# Egypt data
egypt_inequality <- read_xlsx("egypt_income_data.xlsx") %>%
  setNames(., c("percentile", "year", "income_share")) %>%
  group_by(percentile) %>%
  spread(key = percentile, value = income_share) %>%
  setNames(., c("year", "bottom50eg", "middle40eg", "top10eg", "top1eg"))
view(egypt_inequality)

# Lebanon data
lebanon_inequality = read_xlsx("lebanon_income_data.xlsx") %>%
  setNames(., c("percentile", "year", "income_share"))%>%
  group_by(percentile) %>%
  arrange(percentile) %>%
  spread(key = percentile, value = income_share) %>%
  setNames(., c("year", "bottom50lb", "middle40lb", "top10lb", "top1lb"))

view(lebanon_inequality)
# Graph of inequality
turkey_inequality_formatted %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = bottom50tr, color = "Bottom 50%")) + 
  geom_line(aes(y = middle40tr, color = "Middle 40%")) +
  geom_line(aes(y = top10tr, color = "Top 10%")) +
  theme_economist() +
  scale_color_economist() +
  labs(title = "Pre-Tax Income Share in Turkey, 1990-2016", x = "Year", y = "Pre-Tax Income Share")


me_tr_inequality <- left_join(middle_east_inequality, turkey_inequality_formatted, by = "year") 
gr_bg_inequality <- left_join(greece_inequality, bulgaria_inequality, by = "year")
all_inequality <- left_join(me_tr_inequality, gr_bg_inequality, by = "year")
all_inequality <- left_join(all_inequality, egypt_inequality, by = "year")
all_inequality <- left_join(all_inequality, lebanon_inequality, by = "year")

view(all_inequality)
inequality_graph <- all_inequality %>%
  filter(year > 2001) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = top10gr/bottom50gr, color = "Greece")) +
  geom_line(aes(y = top10tr/bottom50tr, color = "Turkey")) +
  geom_line(aes(y = top10bg/bottom50bg, color = "Bulgaria")) +
  geom_line(aes(y = top10eg/bottom50eg, color = "Egypt")) +
  geom_line(aes(y = top10me/bottom50me, color = "Middle East")) +
  scale_color_wsj() +
  theme_minimal() +
  labs(x = "year", y = "Ratio of Share of Top Decile to Share of Bottom 50%")
inequality_graph

# Find ROC of each group
find_roc <- function(x) (ROC(x))
roc_all_inequality <- all_inequality %>%
  mutate_at(vars(top1me:top1lb), find_roc) 
roc_all_inequality <- na.omit(roc_all_inequality)

# Find mean change in bottom 50% share since 2002
mean_change_in_bottom50_since_2002 <- roc_all_inequality %>%
  filter(year > 2001) %>%
  select(bottom50me, bottom50tr, bottom50gr, bottom50bg, bottom50eg, bottom50lb) %>%
  colMeans()

mean_change_in_bottom50_since_2002

roc_all_inequality %>%
  filter(year > 2001) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = bottom50gr, color = "Bottom 50% GR")) +
  geom_line(aes(y = bottom50tr, color = "Bottom 50% TR")) +
  geom_line(aes(y = bottom50me, color = "Bottom 50% ME")) +
  geom_line(aes(y = bottom50bg, color = "Bottom 50% BG")) +
  geom_hline(yintercept = mean(mean_change_in_bottom50_since_2002)) +
  scale_color_wsj()+
  theme_economist() +
  labs(title = "Change of Bottom 50 Income Share in TR, GR, BG, and ME", x = "year", y = "rate of change of income share")

roc_all_inequality %>%
  mutate(top10_cummean = cummean(top10gr + top10tr + top10me +
                                   top10bg + top10eg + top10lb))%>%
  filter(year > 2001) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = top10gr, color = "Top 10% GR")) +
  geom_line(aes(y = top10tr, color = "Top 10% TR")) +
  geom_line(aes(y = top10bg, color = "Top 10% BG")) +
  geom_line(aes(y = top10me, color = "Top 10% ME")) +
  geom_line(aes(y = top10eg, color = "Top 10% EG")) +
  geom_line(aes(y = top10lb, color = "Top 10% LB")) +
  geom_line(aes(y = top10_cummean)) +
  scale_color_hue()+
  theme_economist() +
  labs(title = "Change of Top 10% Income Share", x = "year", y = "rate of change of income share")


roc_all_inequality %>%
  mutate(bottom50_cummean = cummean(bottom50gr + bottom50tr + bottom50me +
                                   bottom50bg + bottom50eg + bottom50lb))%>%
  filter(year > 2001) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = bottom50gr, color = "Bottom 50% GR")) +
  geom_line(aes(y = bottom50tr, color = "Bottom 50% TR")) +
  geom_line(aes(y = bottom50bg, color = "Bottom 50% BG")) +
  geom_line(aes(y = bottom50me, color = "Bottom 50% ME")) +
  geom_line(aes(y = bottom50eg, color = "Bottom 50% EG")) +
  geom_line(aes(y = bottom50lb, color = "Bottom 50% LB")) +
  geom_line(aes(y = bottom50_cummean)) +
  scale_color_hue()+
  theme_economist() +
  labs(title = "Change of Bottom 50% Income Share", x = "year", y = "rate of change of income share")


# Add Transparency International data
# 
ti_data = read_xlsx("2019_CPI_FULLDATA/CPI2019.xlsx", sheet = 3) %>%
  filter(Country == "Turkey" | Country == "Bulgaria" |
           Country == "Greece" | Country == "Egypt" |
           Country == "Lebanon") %>%
  select(Country, cpi2012, cpi2013, cpi2014, cpi2015,
         cpi2016, cpi2017, cpi2018, cpi2019) %>%
  t()  %>%
  as_tibble() %>%
  mutate(year = 2011:2019) %>%
  setNames(., c("cpi_gr", "cpi_bg", "cpi_tr", "cpi_eg", "cpi_lb", "year"))

dim(ti_data)
view(ti_data)
ti_data = ti_data[2:dim(ti_data)[1],]
ti_data = ti_data %>% 
  mutate_if(is.character, as.numeric) %>%
  setNames(., c("cpi_gr", "cpi_bg", "cpi_tr", "cpi_eg", "cpi_lb", "year"))

view(all_inequality)
all_inequality = left_join(all_inequality, ti_data, by = "year")
view(all_inequality)

b50tr = all_inequality %>%
  filter(year > 2011) %>%
  pull(bottom50tr)
cpiTR = all_inequality %>%
  filter(year > 2011) %>%
  pull(cpi_tr)


# Freedom House data
# 
fh_data = openxlsx::read.xlsx("2020_Country_and_Territory_Ratings_and_Statuses_FIW1973-2020.xlsx", sheet = "df", fillMergedCells = TRUE)
view(fh_data)

# Find correlation between Freedom House score and Transparency International corruption score
fh_score = fh_data %>%
  filter(category == "PR" | category == "CL") %>%
  select(year, turkey_total) %>%
  setNames(., c("year", "fh_score_TR")) %>%
  na.omit() %>%
  as_tibble()

all_data = left_join(all_inequality, fh_score, by = "year")
view(all_data)

modeltest = lm(fh_score_TR ~ year + bottom50tr + middle40tr, data = all_data)
summary(modeltest)

plot(all_data$fh_score_TR, all_data$middle40tr)

# more FH Data

fh2_data = openxlsx::read.xlsx("2020_Aggregate_Category_and_Subcategory_Scores_FIW_2003-2020.xlsx", sheet = "data") %>%
  select(year, Total) %>%
  setNames(., c("year", "FH_Index_TR"))

all_data = left_join(all_data, fh2_data, by = "year")

# Evolution of FH index in Turkey
all_turkey_data %>%
  filter(year > 2001) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = FH_Index_TR)) +
  theme_minimal() +
  scale_color_economist() +
  labs(y = "Freedom House Index")

# gini index

all_turkey_data %>%
  filter(year > 2001) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = gini_index)) +
  theme_minimal() +
  scale_color_economist() +
  labs(y = "Gini Index")

model2 = lm(FH_Index_TR ~ bottom50tr + year, data = all_data)
summary(model2)

## add Credit Suisse WEALTh data for Turkey

year = 2000:2014
top_percentile_share = c(38.1, 38.9, 39.4, 40.0, 40.6, 41.3, 42.1, 42.9, 43.8, 45.4, 47.3, 49.1, 51.3, 52.3, 54.3)
top_decile_share = c(66.7, 67.2, 67.7, 68.0, 68.5, 69.0, 69.6, 70.2, 70.7, 71.9, 73.1, 74.3, 75.7, 76.4, 77.7)
med_wealth_years = 2000:2019
med_wealth = c(3615, 1677, 3407, 4627, 6022, 6205, 6722, 8847,
               9812, 7499, 8931, 8186, 9235, 11965, 9322, 9412,
               9209, 8772, 7819, 6568)
avg_wealth_per_adult_smoothed = c(7359, NA, NA, NA, NA, 24470, NA,NA, NA, NA, 33542, NA, NA, NA, NA, 35423, 
                                  35183, 32851, 28602, 27242)
wealth_data_tr = data.table(med_wealth_years, med_wealth, avg_wealth_per_adult_smoothed) %>%
  setNames(., c("year", "median_wealth", "avg_wealth_smoothed"))
view(wealth_data_tr)

turkey_wealth = data.table(year, top_decile_share, top_percentile_share) %>%
  mutate(top_decile_share = top_decile_share/100) %>%
  mutate(top_percentile_share = top_percentile_share/100)

# Add GINI data from World Bank

gini_index_tr = read_xls("turkey_gini.xls", sheet = "turkey_data")
view(gini_index_tr)

# Add HDI data from UN
# 

hdi_tr = read_xlsx("turkey_hdi.xlsx", sheet = "hdi")

all_turkey_data = left_join(turkey_inequality_formatted, fh2_data, by = "year") %>%
  left_join(ti_data, by = "year") %>%
  left_join(turkey_wealth, by = "year") %>%
  left_join(gini_index_tr, by = "year") %>%
  left_join(hdi_tr, by = "year") %>%
  left_join(wealth_data_tr, by = "year") %>%
  select(-c(cpi_gr, cpi_bg, cpi_eg, cpi_lb))

view(all_turkey_data)

all_turkey_data %>%
  filter(year > 2001) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = top_decile_share, color = "Top 10% Wealth Share")) +
  geom_line(aes(y = top10tr, color = "Top 10% Income"))

all_turkey_data %>%
  filter(year > 2001 & year < 2008) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = top10tr, color = "Top 10%")) +
  geom_line(aes(y = bottom50tr, color = "Bottom 50%")) +
  geom_line(aes(y = middle40tr, color = "Middle 40%"))

all_turkey_data %>%
       #filter(year >= 2001 & year < 2008) %>%
       mutate(bottom90tr = bottom50tr + middle40tr) %>%
  select(year, bottom50tr, middle40tr, bottom90tr, top10tr, top1tr)

all_turkey_data %>%
  filter(year > 2001) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = FH_Index_TR, color = "Freedom index")) +
  geom_line(aes(y = gini_index, color = "Gini Index")) +
  theme_minimal() +
  scale_color_economist()


all_turkey_data %>%
  mutate(bottom90tr = middle40tr + bottom50tr) %>% 
  filter(year > 2002) %>%
  view() %>%
  ggplot(aes(year)) +
  geom_line(aes(y = top10tr/bottom50tr, color = "p90p100/p0p50")) +
  geom_line(aes(y = top10tr/middle40tr, color = "p90p100/p50p90")) +
  scale_color_economist()

# Income vs Wealth
all_turkey_data %>%
  filter(year > 2001) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = top10tr, color = "Top 10%"))+
  geom_line(aes(y = middle40tr, color = "Middle 40%"))  +
  geom_line(aes(y = bottom50tr, color = "Bottom 50%")) +
  theme_minimal() +
  scale_color_economist() +
  labs(y = "Income Share", x = "Year")

view(all_turkey_data)
# Impute missing average wealth numbers
model_avg_wealth = lm(avg_wealth_smoothed ~ year + median_wealth, data = wealth_data_tr)

for (i in 1:length(wealth_data_tr$avg_wealth_smoothed)) {
  if (is.na(wealth_data_tr$avg_wealth_smoothed[i])){
    wealth_data_tr$avg_wealth_smoothed[i] = predict(model_avg_wealth, newdata = wealth_data_tr)[i]
  }
}

wealth_data_tr %>%
  mutate(roc_median_wealth = ROC(median_wealth)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = median_wealth, color = "Median Wealth Per Adult")) +
  geom_line(aes(y = avg_wealth_smoothed, color = "Average Wealth Per Adult")) +
  scale_color_economist() +
  theme_minimal() +
  labs(title = "Wealth in Turkey, 2000-2019", y = "Wealth (Constant USD)")

all_turkey_data = left_join(turkey_inequality_formatted, fh2_data, by = "year") %>%
  left_join(ti_data, by = "year") %>%
  left_join(turkey_wealth, by = "year") %>%
  left_join(gini_index_tr, by = "year") %>%
  left_join(hdi_tr, by = "year") %>%
  left_join(wealth_data_tr, by = "year") %>%
  select(-c(cpi_gr, cpi_bg, cpi_eg, cpi_lb))


view(all_turkey_data)

#all_turkey_data = all_turkey_data %>%
#  mutate(bottom90tr = bottom50tr + middle40tr)

model_wealth = lm(median_wealth ~ bottom90tr + FH_Index_TR + top_decile_share, data = all_turkey_data)

summary(model_wealth)

all_turkey_data %>%
  filter(year == 2002 | year == 2008) %>%
  select(year, median_wealth)

view(all_turkey_data)
all_turkey_data %>%
  mutate(the_rest_share = 1 - top_decile_share) %>%
  filter(year >= 2000) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = top_decile_share, color = "Top 10%")) +
  geom_line(aes(y = top_percentile_share, color = "Top 1%")) +
  geom_line(aes(y = the_rest_share, color = "Bottom 90%")) +
  scale_y_continuous(breaks=seq(0.2,0.8,0.05)) +
  labs(title = "Wealth Inequality in Turkey, 2000-2014",
       y = "Wealth Share")


all_turkey_data %>%
  filter(year >= 2000) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = median_wealth, color = "Median")) +
  geom_line(aes(y = avg_wealth_smoothed, color = "Average (imputed)")) +
  theme_minimal() +
  scale_color_economist() +
  labs(y = "Wealth (Constant USD)")



# V-Dem Data -------------------

vdem_df = readRDS("Country_Year_V-Dem_Core_R_v10/Country_Year_V-Dem_Core_R_v10/V-Dem-CY-Core-v10.rds")

vdem = vdem_df %>%
  filter(country_name == "Turkey") %>%
  select(year, v2x_freexp, v2pepwrses, v2pepwrsoc, 
         v2x_libdem, v2x_egaldem, v2x_freexp_altinf,
         v2exbribe, v2exembez, v2xlg_legcon, v2xeg_eqdr,
         v2clacfree, v2x_jucon) %>%
  filter(year >= 1960)
# Political power by wealth
vdem %>%
  filter(year >= 2000) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = v2pepwrsoc)) +
  theme_minimal()

# V-Dem Lib Dem Index
vdem %>%
  filter(year >= 2000) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = v2x_libdem)) +
  theme_minimal() +
  labs(y = "Index")
# Egal index
vdem %>%
  filter(year >= 2000) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = v2x_egaldem)) +
  theme_minimal()

# Freedom of expression index
vdem %>%
  filter(year >= 2000) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = v2x_freexp_altinf)) +
  theme_minimal() +
  labs(y = "Freedom of Expression Index")

# Bribery and corruption
vdem %>%
  filter(year >= 1980) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = v2exbribe, color = "Executive Bribes")) +
  geom_line(aes(y = v2exembez, color = "Executive Embezzelment")) +
  theme_minimal() +
  scale_color_economist() +
  labs(y = "Index")

# Judicial and Legislative checks on executive power
vdem %>%
  filter(year >= 2000) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = v2xlg_legcon, color = "Legislative")) +
  geom_line(aes(y = v2x_jucon, color = "Judicial")) +
  theme_minimal() +
  scale_color_economist() +
  labs(y = "Index")

# Equal distribution of resources
vdem %>%
  filter(year >= 2000) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = v2xeg_eqdr)) +
  theme_minimal() +
  labs(y = "Resource Equality Index")

# Academic freedom
vdem %>%
  filter(year > 2000) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = v2clacfree)) +
  theme_minimal() +
  scale_color_economist()
  
