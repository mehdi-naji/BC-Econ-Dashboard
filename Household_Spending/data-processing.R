library(statcanR)
library(dplyr)


df <- statcan_download_data("11-10-0223-01", "eng")

df$date <- as.Date(as.character(df$REF_DATE), format = "%Y")
df$Year <- format(df$date, "%Y")
df$Category <- df$`Household expenditures, summary-level categories`
df$Quintile <- df$`Before-tax household income quintile`
  
df <- df %>%
  arrange(Category, GEO, Quintile, Year) %>%  # Ensure data is sorted
  group_by(Category, GEO, Quintile) %>%
  mutate(GrowthRate = (VALUE - lag(VALUE))/lag(VALUE))  
  
df <- df |> select(Year,
                   GEO,
                   Category,
                   Quintile,
                   GrowthRate,
                   VALUE)

write.csv(df, file = "~/BC-Econ-Dashboard/data/processed/HouseholdSpending.csv")
