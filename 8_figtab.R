#-----------------------------------------------------------------------------#
#                    8. Generation of figures and tables                      #
#-----------------------------------------------------------------------------#

# Packages
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(data.table)
library(rio)


# 0. Utils for computing trends ----

linear_trend <- function(df, outcome, spunit){
  mod <- lm(as.formula(paste0(outcome, "~year")), data = df)
  data.frame(spunit = as.character(df[1,spunit]),
             int = coef(mod)[1],
             coef = coef(mod)[2],
             lower = confint(mod)[2,1],
             upper = confint(mod)[2,2],
             pval = summary(mod)$coefficients[2,4])
}


# 1. Trends by European regions ----

FWI_region <- read_csv("data/processed/FWI_region.csv")
pm25_region <- read_csv("data/processed/pm25_region.csv")
att_region <- read_csv("data/processed/attributable_region.csv") %>% 
  right_join(read_csv("data/processed/population_region.csv"), by = c("region"="region", "year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000))
FWI_trend <- split(FWI_region, f = FWI_region$region) |>
  map_df(linear_trend, outcome = "FWI_pop", spunit = "region")
pm25_trend <- split(pm25_region, f = pm25_region$region) |>
  map_df(linear_trend, outcome = "pm25_pop", spunit = "region")
att_trend <- split(att_region, f = att_region$region) |>
  map_df(linear_trend, outcome = "attr_stand", spunit = "region")

p1 <- ggplot() +
  geom_abline(data = FWI_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = FWI_region, aes(x=year, y=FWI_pop, col=region, group=region),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(1980, 2024, 4), minor_breaks = seq(1982, 2024, 4)) +
  xlab("") +
  ylab("Annual average FWI") +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")
 
p2 <- ggplot() +
  geom_abline(data = pm25_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = pm25_region, aes(x=year, y=pm25_pop, col=region, group=region),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2024, 2), minor_breaks = seq(2003, 2024, 1)) +
  theme_bw() +
  xlab("") +
  ylab(expression(Annual~average~`wildfire-PM`[2.5]~(mu*g/m^3))) +
  labs(colour = "") +
  theme_classic() +
  theme()

p3 <- ggplot() + 
  geom_abline(data = att_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = att_region, aes(x = year, y = attr_stand, col = region, group = region),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2024, 2), minor_breaks = seq(2003, 2024, 1)) +
  xlab("") +
  ylab(expression(Annual~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K")) +
  labs(colour = "") + 
  theme_classic() +
  theme(legend.position = "none")
 
p4 <- as_ggplot(get_legend(p2))
p2 <- p2 + theme(legend.position = "none")

pall <- ggpubr::ggarrange(ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)) +
  ggpubr::bgcolor("white") +
  ggpubr::border("white")

ggsave("figures/regiontrend_FWI_pm2.5_attrdeaths.png", pall, width = 8, height = 8, dpi = 300)

# Export trends
trendtab <- rbind(mutate(FWI_trend, metric = "FWI"),
                  mutate(pm25_trend, metric = "PM2.5"),
                  mutate(att_trend, metric = "attributable deaths/100K")) |>
  rename(region = spunit, pvalue = pval) |>
  mutate(trend = paste0(round(coef, 3), " (", round(lower, 3), ", ", round(upper,3), ")"),
         pvalue = round(pvalue, 4)) |>
  select(metric, region, trend, pvalue) |>
  arrange(desc(metric), region)

write_csv(trendtab, "figures/figures_regiontrend.csv")
rm("FWI_region", "FWI_trend", "att_region", "att_trend",
   "p1", "p2", "pall", "trendtab")


# 2. Trends by EU members vs non-members ----

FWI_eu <- read_csv("data/processed/FWI_eu.csv")
pm25_eu <- read_csv("data/processed/pm25_eu.csv")
att_eu <- read_csv("data/processed/attributable_eu.csv") %>% 
  right_join(read_csv("data/processed/population_eu.csv"), by = c("eu"="eu", "year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000))
FWI_trend <- split(FWI_eu, f = FWI_eu$eu) |>
  map_df(linear_trend, outcome = "FWI_pop", spunit = "eu")
pm25_trend <- split(pm25_eu, f = pm25_eu$eu) |>
  map_df(linear_trend, outcome = "pm25_pop", spunit = "eu")
att_trend <- split(att_eu, f = att_eu$eu) |>
  map_df(linear_trend, outcome = "attr_stand", spunit = "eu")

p1 <- ggplot() +
  geom_abline(data = FWI_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = FWI_eu, aes(x=year, y=FWI_pop, col=eu, group=eu),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#0077BB", "#CC3311")) +
  scale_x_continuous(breaks = seq(1980, 2024, 4), minor_breaks = seq(1982, 2024, 4)) +
  xlab("") +
  ylab("Annual average FWI") +
  labs(colour = "") +
  theme_classic() +
  theme()

p2 <- ggplot() +
  geom_abline(data = pm25_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = pm25_eu, aes(x=year, y=pm25_pop, col=eu, group=eu),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#0077BB", "#CC3311")) +
  scale_x_continuous(breaks = seq(2003, 2024, 2), minor_breaks = seq(2003, 2024, 1)) +
  theme_bw() +
  xlab("") +
  ylab(expression(Annual~average~`wildfire-PM`[2.5]~(mu*g/m^3))) +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")

p3 <- ggplot() + 
  geom_abline(data = att_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = FALSE) +
  geom_line(data = att_eu, aes(x = year, y = attr_stand, col = eu, group = eu),
            alpha = 0.7, lwd = 0.5) +
  scale_colour_manual(values = c("#0077BB", "#CC3311")) +
  scale_x_continuous(breaks = seq(2003, 2024, 2), minor_breaks = seq(2003, 2024, 1)) +
  xlab("") +
  ylab(expression(Annual~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K")) +
  labs(colour = "") + 
  theme_classic() +
  theme(legend.position = "none")
  
p4 <- as_ggplot(get_legend(p1))
p1 <- p1 + theme(legend.position = "none")

pall <- ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2) +
  ggpubr::bgcolor("white") +
  ggpubr::border("white")

ggsave("figures/EUtrend_FWI_pm25_attrdeaths.png", pall, width = 8, height = 8, dpi = 300)

# Export trends
trendtab <- rbind(mutate(FWI_trend, metric = "FWI"),
                  mutate(pm25_trend, metric = "PM2.5"),
                  mutate(att_trend, metric = "attributable deaths/100K")) |>
  rename(eu = spunit, pvalue = pval) |>
  mutate(trend = paste0(round(coef, 3), " (", round(lower, 3), ", ", round(upper,3), ")"),
         pvalue = round(pvalue, 4)) |>
  select(metric, eu, trend, pvalue) |>
  arrange(desc(metric), eu)

write_csv(trendtab, "figures/figures_EUtrend.csv")
rm("FWI_eu", "FWI_trend", "att_eu", "att_trend",
   "p1", "p2", "pall", "trendtab")


# 3. Trends by country ----

# Compute trends
FWI_country <- read_csv("data/processed/FWI_country.csv")
pm25_country <- read_csv("data/processed/pm25_country.csv")
attr_country <- read_csv("data/processed/attributable_country.csv") %>% 
  right_join(read_csv("data/processed/population_country.csv"), by = c("NUTS_0"="NUTS_0", "year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000))
FWI_trendpop <- split(FWI_country, f = FWI_country$NUTS_0) |>
  map_df(linear_trend, outcome = "FWI_spatial", spunit = "NUTS_0")
pm25_trendpop <- split(pm25_country, f = pm25_country$NUTS_0) |>
  map_df(linear_trend, outcome = "pm25_spatial", spunit = "NUTS_0")
attr_trendpop <- attr_country %>%
  filter(!is.na(attr_stand)) %>% # Remove rows with NA in 'attr_stand'
  split(.$NUTS_0) %>% 
  map_df(linear_trend, outcome = "attr_stand", spunit = "NUTS_0")

# Add complete names
euroregions <- import("data/raw/regions/[LCDE 2026] Country names and groupings.xlsx", skip=1) |>
  dplyr::select(1,4,5,9) 
euroregions <- euroregions[!duplicated(euroregions),]
names(euroregions) <- c("country", "NUTS_0", "eu", "region")
euroregions <- euroregions %>% mutate(eu=ifelse(eu=="Member", "Member","Not EU"))
# Change Cyprus to Southern Europe
euroregions$region[euroregions$NUTS_0=="CY"] <- "Southern Europe"

FWI_trendpop <- left_join(FWI_trendpop, euroregions, by = c("spunit" = "NUTS_0")) 
pm25_trendpop <- left_join(pm25_trendpop, euroregions, by = c("spunit" = "NUTS_0"))
attr_trendpop <- left_join(attr_trendpop, euroregions, by = c("spunit" = "NUTS_0"))

# Prepare for plotting
FWI_trendpop$Pvalue <- case_when(
  FWI_trendpop$pval > 0.2 ~ "> 0.2",
  FWI_trendpop$pval > 0.05 ~ "0.05 to 0.2",
  FWI_trendpop$pval <= 0.05 ~ "< 0.05")
FWI_trendpop$Pvalue <- fct_relevel(FWI_trendpop$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))

pm25_trendpop$Pvalue <- case_when(
  pm25_trendpop$pval > 0.2 ~ "> 0.2",
  pm25_trendpop$pval > 0.05 ~ "0.05 to 0.2",
  pm25_trendpop$pval <= 0.05 ~ "< 0.05")
pm25_trendpop$Pvalue <- fct_relevel(pm25_trendpop$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))

attr_trendpop$Pvalue <- case_when(
  attr_trendpop$pval > 0.2 ~ "> 0.2",
  attr_trendpop$pval > 0.05 ~ "0.05 to 0.2",
  attr_trendpop$pval <= 0.05 ~ "< 0.05")
attr_trendpop$Pvalue <- fct_relevel(attr_trendpop$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))

# Plotting
p1 <- ggpubr::ggdotchart(FWI_trendpop, x = "country", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend in Fire Weather Index") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p2 <- ggpubr::ggdotchart(pm25_trendpop, x = "country", y = "coef",
                 color = "region",
                 dot.size = "Pvalue",
                 sorting = "descending",
                 add = "segments",
                 add.params = list(color = "lightgray", size = 2),
                 font.label = list(color = "white", size = 9,
                                   vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab(expression(Trend~"in"~`wildfire-PM`[2.5])) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p3 <- ggpubr::ggdotchart(attr_trendpop, x = "country", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab(expression(Trend~"in"~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

pall <- ggpubr::ggarrange(p1, p2, p3,
                  nrow = 3, common.legend = T, legend = "bottom")

ggsave("figures/countrytrend_popw.png", pall,  width = 8, height = 11.5)
rm("ccodes", "FWI_country", "FWI_trendpop", "attr_country", "attr_trendpop", "p1", "p2", "p3", "pall",
   "pm25_country", "pm25_trendpop", "euroregions")


# 4. Inequalities ----

# Data prep
FWI_nuts2 <- read_csv("data/processed/FWI_nuts2.csv") |>
  filter(year %in% 2003:2024)
pm25_nuts2 <- read_csv("data/processed/pm25_nuts2.csv")
attr_nuts2 <- read_csv("data/processed/attributable_nuts.csv") %>% 
  left_join(read_csv("data/processed/population_nuts.csv"), by = c("NUTS_mort"="NUTS_mort", "year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000))
pm25_nuts2 <- full_join(FWI_nuts2, pm25_nuts2, by = c("NUTS_2", "year")) %>%
  full_join(attr_nuts2, by = c("NUTS_2" = "NUTS_mort", "year" = "year"))
pm25_nuts2 <- group_by(pm25_nuts2, NUTS_2) |>
  summarise(pm25 = mean(pm25_pop),
            FWI = mean(FWI_pop),
            attr_stand = mean(attr_stand))

ineq <- import("data/raw/deprivation/ilc_mddd21$defaultview_spreadsheet.xlsx", sheet="data for R")
ineq <- ineq %>% 
  mutate(across(3:7, as.numeric)) %>% 
  mutate(deprivation = rowSums(select(., 3:7), na.rm = T) /
           rowSums(!is.na(select(., 3:7)))) %>% 
  select(-c(2:7)) 
ineq <- ineq %>% 
  add_row(NUTS_2 = "NL31NL33", 
          deprivation = sum(ineq %>% filter(NUTS_2=="NL31") %>% pull(deprivation), 
                                                 ineq %>% filter(NUTS_2=="NL33") %>% pull(deprivation))/2) %>% 
  mutate(tertile = ntile(deprivation, 3),
         category = factor(tertile, levels = c(1, 2, 3),
                           labels = c("low", "medium", "high"))) %>%
  filter(NUTS_2!="NL31") %>% filter(NUTS_2!="NL33")

pm25_nuts2 <- inner_join(pm25_nuts2, ineq, by = c("NUTS_2" = "NUTS_2"))
FWImeans <- data.frame(category = c("low", "medium", "high"),
                        mean = tapply(pm25_nuts2$FWI, pm25_nuts2$category, mean),
                        sd = tapply(pm25_nuts2$FWI, pm25_nuts2$category, sd)) |>
  mutate(lab = paste0("Mean (SD): ", round(mean, 2), " (", round(sd, 2), ")"))
pm25means <- data.frame(category = c("low", "medium", "high"),
                        mean = tapply(pm25_nuts2$pm25, pm25_nuts2$category, mean),
                        sd = tapply(pm25_nuts2$pm25, pm25_nuts2$category, sd)) |>
  mutate(lab = paste0("Mean (SD): ", round(mean, 2), " (", round(sd, 2), ")")) 
attrmeans <- data.frame(category = c("Low", "Medium", "High"),
                        mean = tapply(pm25_nuts2$attr_stand, pm25_nuts2$category, mean),
                        sd = tapply(pm25_nuts2$attr_stand, pm25_nuts2$category, sd)) |>
  mutate(lab = paste0("Mean (SD): ", round(mean, 2), " (", round(sd, 2), ")")) 

# Plot
pm25_nuts2_fwi <- pm25_nuts2 %>%
  filter(!is.na(FWI)) %>% 
  filter(!is.na(category))
p1 <- ggboxplot(pm25_nuts2_fwi, x = "category", y = "FWI",
                color = "category", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter") +
  geom_text(data = FWImeans, aes(x = category, y = 0.1, label = lab), size = 3) +
  theme_bw() + 
  xlab("Deprivation") +
  ylab("Average FWI") +
  theme(legend.title = element_blank())

pm25_nuts2_pm25 <- pm25_nuts2 %>%
  filter(!is.na(pm25)) %>% 
  filter(!is.na(category))
p2 <- ggboxplot(pm25_nuts2_pm25, x = "category", y = "pm25",
                color = "category", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter") +
  geom_text(data = pm25means, aes(x = category,  y = 0.01, label = lab), size = 3) +
  theme_bw() + theme(legend.position = "none") +
  xlab("Deprivation") +
  ylab(expression(Average~`wildfire-PM`[2.5]~(mu*g/m^3)))

pm25_nuts2_attr <- pm25_nuts2 %>%
  filter(!is.na(attr_stand)) %>% 
  filter(!is.na(category))
p3 <- ggboxplot(pm25_nuts2_attr, x = "category", y = "attr_stand",
                color = "category", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter") +
  geom_text(data = pm25means, aes(x = category,  y = 0.01, label = lab), size = 3) +
  theme_bw() + theme(legend.position = "none") +
  xlab("Deprivation") +
  ylab(expression(Annual~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K"))

p4 <- as_ggplot(get_legend(p1))
p1 <- p1 + theme(legend.position = "none")

pall <- ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2) +
  ggpubr::bgcolor("white") +
  ggpubr::border("white")

ggsave("figures/inequalities_nuts2.png", pall,  width = 9, height = 9, dpi = 300)
rm("p1", "p2", "pall", "FWImeans",
   "pm25means", "attrmeans", "pm25_nuts2", "FWI_nuts2", "pm25_nuts2_fwi", 
   "pm25_nuts2_pm25", "pm25_nuts2_attr", "ineq")


# 5. Yearly death counts in Europe ----

attreuro_main <- read_csv("data/processed/attributable_euro.csv") %>% 
  left_join(read_csv("data/processed/population_euro.csv"), by = c("year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000),
         attrlower_stand = (attrlower/population_sum*100000),
         attrupper_stand = (attrupper/population_sum*100000)) %>% 
  mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
                            ", ", round(attrupper,2), ")"),
         attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
                       ", ", round(attrupper_stand,2), ")")) %>% 
  select(year, Ncountries, attr_main, attr_main_stand)

# Sensitivity analysis - lag0-1
attreuro_lag0to1 <- read_csv("data/processed/attributable_euro_sens_lag0-1.csv") %>% 
  left_join(read_csv("data/processed/population_euro.csv"), by = c("year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000),
         attrlower_stand = (attrlower/population_sum*100000),
         attrupper_stand = (attrupper/population_sum*100000)) %>% 
  mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
                            ", ", round(attrupper,2), ")"),
         attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
                                  ", ", round(attrupper_stand,2), ")")) %>% 
  select(year, attr_main, attr_main_stand)

# Sensitivity analysis - lag 0-7
attreuro_lag0to7 <- read_csv("data/processed/attributable_euro_sens_lag0-7.csv") %>% 
  left_join(read_csv("data/processed/population_euro.csv"), by = c("year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000),
         attrlower_stand = (attrlower/population_sum*100000),
         attrupper_stand = (attrupper/population_sum*100000)) %>% 
  mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
                            ", ", round(attrupper,2), ")"),
         attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
                                  ", ", round(attrupper_stand,2), ")")) %>% 
  select(year, attr_main, attr_main_stand)

# Sensitivity analysis - RR for total PM2.5 mass
attreuro_totpm25 <- read_csv("data/processed/attributable_euro_sens_RR_totalpm25.csv") %>% 
  left_join(read_csv("data/processed/population_euro.csv"), by = c("year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000),
         attrlower_stand = (attrlower/population_sum*100000),
         attrupper_stand = (attrupper/population_sum*100000)) %>% 
  mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
                            ", ", round(attrupper,2), ")"),
         attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
                                  ", ", round(attrupper_stand,2), ")")) %>% 
  select(year, attr_main, attr_main_stand)

# Sensitivity analysis - weekly RR and HIA
attreuro_weekly <- read_csv("data/processed/attributable_euro_weekly.csv") %>% 
  left_join(read_csv("data/processed/population_euro.csv"), by = c("year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000),
         attrlower_stand = (attrlower/population_sum*100000),
         attrupper_stand = (attrupper/population_sum*100000)) %>% 
  mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
                            ", ", round(attrupper,2), ")"),
         attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
                                  ", ", round(attrupper_stand,2), ")")) %>% 
  select(year, attr_main, attr_main_stand)

attr <- attreuro_main %>% 
  full_join(attreuro_weekly, by = c("year" = "year")) %>% 
  full_join(attreuro_lag0to1, by = c("year" = "year")) %>% 
  full_join(attreuro_lag0to7, by = c("year" = "year")) %>% 
  full_join(attreuro_totpm25, by = c("year" = "year"))

names(attr) <- c("Year", "Number of included countries", 
                 "Attributable deaths (95% CI): lag0", "Attributable deaths/100K (95% CI): lag0", 
                 "Attributable deaths (95% CI): Weekly HIA lag0", "Attributable deaths/100K (95% CI): Weekly HIA lag0",
                 "Attributable deaths (95% CI): lag0-1", "Attributable deaths/100K (95% CI): lag0-1",
                 "Attributable deaths (95% CI): lag0-7", "Attributable deaths/100K (95% CI): lag0-7",
                 "Attributable deaths (95% CI): Total PM2.5", "Attributable deaths/100K (95% CI): Total PM2.5")

write_csv(attr, "figures/attributable_euro_sens.csv")
rm(attreuro_main, attreuro_lag0to1, attreuro_lag0to7, attreuro_totpm25, attreuro_weekly, attr)


# 6. Top 20 death counts 2003-2024 by NUTS 2 ----

attnuts <- read_csv("data/processed/attributable_nuts.csv")  %>% 
  left_join(read_csv("data/processed/population_nuts.csv"), by = c("NUTS_mort"="NUTS_mort", "year"="year")) %>% 
  mutate(attr_stand = (attr/population_sum*100000),
         attrlower_stand = (attrlower/population_sum*100000),
         attrupper_stand = (attrupper/population_sum*100000)) %>% 
  mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
                            ", ", round(attrupper,2), ")"),
         attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
                                  ", ", round(attrupper_stand,2), ")")) %>% 
  filter(NUTS_0!="DE") %>% # excluded because not at NUTS2 level
  arrange(-attr) %>% 
  select(year, NUTS_mort, NUTS_0, attr_main, attr_main_stand) 
attnuts <- attnuts[1:20,]
attr <- attnuts %>% 
    mutate(country = case_when(
      NUTS_0 == "EL" ~ "Greece",
      NUTS_0 == "PT" ~ "Portugal",
      NUTS_0 == "IT" ~ "Italy",
      NUTS_0 == "ES" ~ "Spain")) %>% 
    select(year, NUTS_mort, country, attr_main, attr_main_stand)

names(attr) <- c("Year", "NUTS2", "Country", "Attributable deaths (95% CI)", "Attributable deaths/100K (95% CI)")
  
write_csv(attr, "figures/attributable_nuts_20.csv")
rm(attnuts, attr)

# clean
rm(list = ls())
