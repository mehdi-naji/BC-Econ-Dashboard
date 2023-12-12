library(statcanR)
library(dplyr)
library(tidyverse)

# GDP ----
GDP_df <- statcan_download_data("36-10-0402-01", "eng")
GDP_df$REF_DATE <- as.Date(GDP_df$REF_DATE, origin = "2009-01-01")
GDP_df$Year <- format(GDP_df$REF_DATE, "%Y")

Sectors_GDP <- c("Agriculture, forestry, fishing and hunting",
                 "Mining, quarrying, and oil and gas extraction",
                 "Utilities",
                 "Construction",
                 "Manufacturing",
                 "Wholesale trade",
                 "44-45",
                 "Transportation and warehousing",
                 "Information and cultural industries",
                 "Finance and insurance",
                 "Real estate and rental and leasing",
                 "Professional, scientific and technical services",
                 "Management of companies and enterprises",
                 "Administrative and support, waste management and remediation services",
                 "Educational services \\[61\\]",
                 "Health care and social assistance",
                 "Arts, entertainment and recreation",
                 "Accommodation and food services",
                 "Other services (except public administration)",
                 "Public administration",
                 "\\[81\\]")

GDP_df <- GDP_df |>
            select (Year, GEO, `North American Industry Classification System (NAICS)` , SCALAR_FACTOR, Value, VALUE) |>
            filter (Value %in% c("Current dollars",
                                  "Chained (2017) dollars"))|>
            pivot_wider(names_from = Value, values_from = VALUE)

GDP_dashdata <- GDP_df[grepl(paste(Sectors_GDP, collapse = "|"),  GDP_df$`North American Industry Classification System (NAICS)`), ] |> 
  rename(NAICS = `North American Industry Classification System (NAICS)`,
         CurrentValue = `Current dollars`,
         Chained_2017 = `Chained (2017) dollars`) |>
  mutate(NAICS = case_when(
    NAICS == "Information and cultural industries [51]" ~ "Information, culture and recreation [51, 71]",
    NAICS == "Arts, entertainment and recreation [71]" ~ "Information, culture and recreation [51, 71]",
    TRUE ~ NAICS))|>
  group_by(Year, GEO, NAICS, SCALAR_FACTOR ) |>
  summarise(CurrentValue = sum(CurrentValue),
            Chained_2017 = sum(Chained_2017))

GDP_dashdata <- GDP_dashdata |>
  arrange(Year, GEO, NAICS, SCALAR_FACTOR) |> 
  group_by(GEO, NAICS, SCALAR_FACTOR) |>
  mutate(NGDPG = (CurrentValue/lag(CurrentValue) - 1)*100,
         RGDPG = (Chained_2017/lag(Chained_2017) - 1)*100
         )
## Population ----
pop_df <- statcan_download_data("17-10-0005-01", "eng")
pop_df$REF_DATE <- as.Date(pop_df$REF_DATE, origin = "2009-01-01")
pop_df$Year <- format(pop_df$REF_DATE, "%Y")

pop_df <- pop_df |>
  filter(`Age group` == "All ages",
         Sex == "Both sexes",
         Year > 1996,
         !(GEO %in% c("Northwest Territories including Nunavut",
                      "Canada"))) |>
  select(GEO, VALUE, Year) |>
  rename(Population = VALUE)
  

GDP_dashdata <- merge(GDP_dashdata, pop_df, by = c("GEO", "Year"), all.x = TRUE)

GDP_dashdata$CurrentValuePerCapita <- GDP_dashdata$CurrentValue/GDP_dashdata$Population
GDP_dashdata$Chained_2017PerCapita <- GDP_dashdata$Chained_2017/GDP_dashdata$Population


# Investment ----
INVS_df <- statcan_download_data("34-10-0035-01", "eng")

INVS_df$REF_DATE <- as.Date(INVS_df$REF_DATE, origin = "2006-01-01")
INVS_df$Year <- format(INVS_df$REF_DATE, "%Y")

Sectors_INVS <- c("Agriculture, forestry, fishing and hunting",
                  "Mining, quarrying, and oil and gas extraction",
                  "Utilities",
                  "Construction",
                  "Manufacturing",
                  "Wholesale trade",
                  "44-45",
                  "Transportation and warehousing",
                  "Information and cultural industries",
                  "Finance and insurance",
                  "Real estate and rental and leasing",
                  "Professional, scientific and technical services",
                  "Management of companies and enterprises",
                  "Administrative and support, waste management and remediation services",
                  "Educational services \\[61\\]",
                  "Health care and social assistance",
                  "Arts, entertainment and recreation",
                  "Accommodation and food services",
                  "Other services (except public administration)",
                  "Public administration",
                  "\\[81\\]")

INVS_dashdata <- INVS_df[grepl(paste(Sectors_INVS, collapse = "|"),  INVS_df$`North American Industry Classification System (NAICS)`), ] |> 
  filter(  `Capital and repair expenditures` == "Capital expenditures") |>
  select(Year, GEO, `North American Industry Classification System (NAICS)`, VALUE , SCALAR_FACTOR ) |>
  rename(NAICS = `North American Industry Classification System (NAICS)`) |>
  mutate(NAICS = case_when(
    NAICS == "Information and cultural industries [51]" ~ "Information, culture and recreation [51, 71]",
    NAICS == "Arts, entertainment and recreation [71]" ~ "Information, culture and recreation [51, 71]",
    TRUE ~ NAICS))|>
  group_by(Year, GEO, NAICS, SCALAR_FACTOR ) |>
  summarise(VALUE = sum(VALUE))


INVS_dashdata <- rename(INVS_dashdata, "Investment" = "VALUE")

# Labour Force ----

EMPL_df <- statcan_download_data("14-10-0023-01", "eng")

EMPL_df$REF_DATE <- as.Date(EMPL_df$REF_DATE, origin = "2009-01-01")
EMPL_df$Year <- format(EMPL_df$REF_DATE, "%Y")

Sectors_EMPL <- c("111", "112", "113", "114", "115",
                 "Mining, quarrying, and oil and gas extraction",
                 "Utilities",
                 "Construction",
                 "Manufacturing",
                 "Wholesale trade",
                 "Retail trade",
                 "Transportation and warehousing",
                 "Information and cultural industries",
                 "Finance and insurance",
                 "Real estate and rental and leasing",
                 "Professional, scientific and technical services",
                 "Management of companies and enterprises",
                 "Administrative and support, waste management and remediation services",
                 "Educational services",
                 "Health care and social assistance",
                 "Arts, entertainment and recreation",
                 "71",
                 "Accommodation and food services",
                 "Other services",
                 "Public administration")
EMPL_df$VALUE <- as.numeric(EMPL_df$VALUE)

EMPL_dashdata <- EMPL_df |>
  filter(grepl(paste(Sectors_EMPL, collapse = "|"),  `North American Industry Classification System (NAICS)`)) |>
  filter(Sex == "Both sexes",
         `Age group` == "15 years and over",
         `Labour force characteristics` %in% c("Employment", "Unemployment rate")) |>
  select(REF_DATE, GEO, `Labour force characteristics`, `North American Industry Classification System (NAICS)`, VALUE, Year) |>
  mutate_all(~replace(., is.na(.), 0))|>
  mutate(`North American Industry Classification System (NAICS)` = ifelse(grepl(paste(c("111", "113", "114"), collapse = "|"),  `North American Industry Classification System (NAICS)`),
                                                                          "Agriculture, forestry, fishing and hunting [11]", `North American Industry Classification System (NAICS)`))|>
  group_by(Year, GEO, `North American Industry Classification System (NAICS)`,   `Labour force characteristics`)|>
  summarise(VALUE = ifelse(`Labour force characteristics` == "Employmnet", sum(VALUE), max(VALUE))) |>
  rename(NAICS = `North American Industry Classification System (NAICS)`) |>
  mutate (NAICS = case_when(
    NAICS == "Mining, quarrying, and oil and gas extraction [21, 2100]" ~ "Mining, quarrying, and oil and gas extraction [21]",
    TRUE ~ NAICS))
EMPL_dashdata <- EMPL_dashdata[!duplicated(EMPL_dashdata), ]

EMPL_dashdata <- EMPL_dashdata |> 
  select(Year, GEO, NAICS, `Labour force characteristics`, VALUE) |>
  distinct() |>
  pivot_wider(names_from = `Labour force characteristics`,
              values_from = VALUE)

# Dashboard data ----
df <- merge(GDP_dashdata, EMPL_dashdata, by = c("Year", "GEO", "NAICS"))
df <- merge(df, INVS_dashdata, by = c("Year", "GEO", "NAICS"), all = TRUE)
write.csv(GDP_dashdata, file = "~/BC-Econ-Dashboard/data/processed/GDP_Industry_dash.csv")
write.csv(EMPL_dashdata, file = "~/BC-Econ-Dashboard/data/processed/EMPL_Industry_dash.csv")
write.csv(INVS_dashdata, file = "~/BC-Econ-Dashboard/data/processed/INVS_Industry_dash.csv")

write.csv(df, file = "~/BC-Econ-Dashboard/data/processed/GDPEMPL_Industry_dash.csv")

