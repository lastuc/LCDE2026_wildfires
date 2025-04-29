#-----------------------------------------------------------------------------#
#                     3b. Clean and write weekly mortality data                #
#-----------------------------------------------------------------------------#

# Warning:
# Germany (DE), Ireland (IE), Croatia (HR), Slovenia (SI) available at the NUTS1 level
# No data for Macedonia (MK)
# The rest of the countries are available at NUTS 2 level
# Estonia (EE) mortality data is for NUTS16, but that makes no difference at the
# NUTS2 level.


# pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDE2026_wildfires/"
pathroot <- ""


# 1. Clean mortality ----

# Read data
mort <- fread(paste0(pathroot, "data/raw/mortality/estat_demo_r_mwk2_ts.tsv"))

# Take years 2003-2024
mort <- dplyr::select(mort, 1, contains(as.character(2003:2024)))

# Disentangle 1st column, take both sexes and NUTS codes
names(mort)[1] <- "meta"
mort <- group_by(mort, meta) |>
  mutate(sex = strsplit(meta, ",")[[1]][2],
         NUTS_mort = strsplit(meta, ",")[[1]][4]) |>
  ungroup() |>
  filter(sex == "T") |>
  filter(nchar(NUTS_mort)==4|(nchar(NUTS_mort)==3 & grepl("DE|IE|HR|SI", NUTS_mort))) |> 
  dplyr::select(-meta, -sex)

# Wide to long format, parse NAs and remove provisional flags
mort <- pivot_longer(mort, -NUTS_mort, names_to = "week", values_to = "deaths")
mort <- mutate(mort,
               deaths = ifelse(deaths == ":", NA, deaths),
               deaths = gsub(" p", "", deaths),
               deaths = as.numeric(deaths)) |> 
  filter(!is.na(deaths))
# Merge data of deaths for PT in 2023 and 2024
mort <- mort[!(grepl("2023|2024", mort$week) & grepl("PT", mort$NUTS_mort)), ]
mort_pt <- import(paste0(pathroot, "data/raw/mortality/PT_weeky_deaths_nuts2_2023_2024.xls"), which = "CleanedforR") |>
  fill(week, .direction = "down") |>
  mutate(week = paste0(str_extract(week, "\\d{4}"), "-W", str_extract(week, "\\d+")),
         NUTS_mort = paste0("PT", nuts)) |>
  select(-name, -nuts) |>
  filter(nchar(NUTS_mort)==4) 
mort <- bind_rows(mort, mort_pt)

# Discard XX/SIX NUTS2: unclassified and minor counts, no geometry available
mort <- mort[!grepl("XX", mort$NUTS_mort)&!grepl("SIX", mort$NUTS_mort),]

# Discard NO0B NUTS2 (Jan Mayen and Svalbard): always 0
mort <- mort[!grepl("NO0B", mort$NUTS_mort),]

# Discard extra Norway NUTS2: superseded (merged into larger NUTS2) in 2021 version
mort <- mort[!grepl("NO01|NO03|NO04|NO05", mort$NUTS_mort),] # no new data in 2024

# Discard more NUTS2 for which we have no exposure data: Canary Islands (ES70),
# Guadeloupe (FRY1), Martinique (FRY2), Guyane (FRY3), La Reunion (FRY4),
# Mayotte (FRY5), Acores (PT20), Madeira (PT30)
mort <- mort[!grepl("ES70|FRY1|FRY2|FRY3|FRY4|FRY5|PT20|PT30", mort$NUTS_mort),] 

# Merging deaths in the regions NL31 & NL33 (2015-2023), NL35 & NL36 due to 
# change in NUT2 boundaries, and name it NL31NL33 (2015-2024)
mort <- mort %>%
  filter(!NUTS_mort %in% c("NL31", "NL33")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("NL31", "NL33")) %>% 
              group_by(week) %>% 
              summarise(
                NUTS_mort = "NL31NL33", 
                deaths = sum(deaths))) %>%
  filter(!NUTS_mort %in% c("NL35", "NL36")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("NL35", "NL36")) %>% 
              group_by(week) %>% 
              summarise(
                NUTS_mort = "NL31NL33", 
                deaths = sum(deaths))) 

# Add the date of the week
mort <- mort %>% mutate(date = paste0(week, "-1"))
mort <- mort %>% mutate(date = str_replace(date, "-W(\\d)-", "-W0\\1-"))

# makes sure that it is the same date as weekly exposure
mort <- mort %>% mutate(date = ISOweek2date(date)-1)

# Extract the year and order by NUTS unit and week
mort <- mort %>% 
  mutate(year = year(date)) %>% 
  arrange(NUTS_mort, date)

# Check that all mortality codes have an available geometry
pointnuts <- vect(paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))
if(any(mort$NUTS_mort[!mort$NUTS_mort %in% pointnuts$NUTS_mort])){
  stop("Something has gone wrong when processing the weekly death counts.")
}

# Check that only North Macedonia has exposure but not mortality data
if(any(pointnuts$NUTS_mort[!pointnuts$NUTS_mort %in% mort$NUTS_mort] != "MK00")){
  stop("Something has gone wrong when processing the weekly death counts.")
}


# 2. Write ----
write_csv(mort, paste0(pathroot, "data/processed/mortality_weekly.csv"))
