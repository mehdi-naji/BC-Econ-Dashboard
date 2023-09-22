library(statcanR)
library(dplyr)


df <- statcan_download_data("11-10-0223-01", "eng")

df <- df |> select(REF_DATE,
                   GEO,
                   `Before-tax household income quintile`,
                   `Household expenditures, summary-level categories`,
                   VALUE)

write.csv(df, file = "~/BC-Econ-Dashboard/data/processed/processed_df.csv")

