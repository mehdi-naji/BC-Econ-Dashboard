library(statcanR)
library(dplyr)


df <- statcan_download_data("11-10-0223-01", "eng")

df$date <- as.Date(as.character(df$REF_DATE), format = "%Y")
df$Year <- format(df$date, "%Y")

df <- df |> select(Year,
                   GEO,
                   `Before-tax household income quintile`,
                   `Household expenditures, summary-level categories`,
                   VALUE)

write.csv(df, file = "~/BC-Econ-Dashboard/data/processed/processed_df.csv")

df <- df |> filter(Year == 2016,
                   GEO == "British Columbia",
                   `Household expenditures, summary-level categories` == "Total expenditure",
                   `Before-tax household income quintile` != "All quintiles"
                   )

library(ggplot2)

ggplot(df) +
  aes(x=reorder(`Before-tax household income quintile`, VALUE), 
      y=VALUE, 
      fill=GEO) +
  geom_col(stat="identity", color="white", position=position_dodge()) +
  # scale_y_continuous(breaks = seq(-30, 40, by = 5))+
  theme(panel.grid.major.y = element_line(color = "grey",
                                          size = 0.5,
                                          linetype = 2))+ 
  ggtitle(paste0("Monthly Average of ", 
                 input$option)) +
  xlab("Month") + 
  ylab(paste("Average Monthly",
             input$option))+
  theme(text = element_text(size=12),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90))
