library(statcanR)
library(dplyr)

# Greenhouse Gas Emissions by industry and province 
# GHGE_df <- statcan_download_data("38-10-0098-01", "eng")
GHGE_df <- statcan_download_data("38-10-0097-01", "eng")

# GDP by industry and province
GDP_df <- statcan_download_data("36-10-0402-01", "eng")

# Labour Force characteristics by industry and province
EMPL_df <- statcan_download_data("14-10-0023-01", "eng")

###======================###
###  Cleaning GHGE_df  

GHGE_df$REF_DATE <- as.Date(GHGE_df$REF_DATE, origin = "2009-01-01")
GHGE_df$Year <- format(GHGE_df$REF_DATE, "%Y")


GDP_df$REF_DATE <- as.Date(GDP_df$REF_DATE, origin = "2009-01-01")
GDP_df$Year <- format(GDP_df$REF_DATE, "%Y")

EMPL_df$REF_DATE <- as.Date(EMPL_df$REF_DATE, origin = "2009-01-01")
EMPL_df$Year <- format(EMPL_df$REF_DATE, "%Y")
# matches <- grepl("wood", GHGE_df$Sector, ignore.case = TRUE)
# unique(GHGE_df$Sector[matches])

unique(GHGE_df$GEO)

GHGE_df <- GHGE_df |> 
  mutate (Secategory = case_when(
    Sector == "Total, industries" ~ "Total, industries",
    Sector == "Total, households" ~ "Total, households",
    Sector == "Total, industries and households" ~ "Total, industries and households",
    Sector %in% c("Crop and animal production [BS11A00]",
                  "Forestry and logging [BS11300]", 
                  "Fishing, hunting and trapping [BS11400]",
                  "Support activities for agriculture and forestry [BS11500]") ~
                        "Agriculture, Foresty, fishing and hunting",
    Sector %in% c("Oil and gas extraction [BS21100]",
                  "Coal mining [BS21210]",
                  "Metal ore mining [BS21220]",                                                                                                                         
                  "Non-metallic mineral mining and quarrying [BS21230]",                                                                                                
                  "Support activities for mining and oil and gas extraction [BS21300]") ~
                        "Mining, quarrying, and oil and gas extractoin",
    Sector %in% c("Electric power generation, transmission and distribution [BS22110]",                                                                                 
                  "Natural gas distribution, water, sewage and other systems [BS221A0]") ~
                        "Utilities",
    Sector %in% c("Residential building construction [BS23A00]",                                                                                                        
                  "Non-residential building construction [BS23B00]",                                                                                                    
                  "Transportation engineering construction [BS23C10]",                                                                                                  
                  "Oil and gas engineering construction [BS23C20]",                                                                                                     
                  "Electric power engineering construction [BS23C30]",                                                                                                  
                  "Communication engineering construction [BS23C40]",                                                                                                  
                  "Other engineering construction [BS23C50]",
                  "Other activities of the construction industry [BS23E00]",
                  "Repair construction [BS23D00]") ~
                        "Construction",
    Sector %in% c("Animal food manufacturing [BS31110]",
                  "Sugar and confectionery product manufacturing [BS31130]",
                  "Fruit and vegetable preserving and specialty food manufacturing [BS31140]",                                                                          
                  "Dairy product manufacturing [BS31150]" ,                                                                                                             
                  "Meat product manufacturing [BS31160]" ,                                                                                                              
                  "Seafood product preparation and packaging [BS31170]",                                                                                                
                  "Miscellaneous food manufacturing [BS311A0]",                                                                                                         
                  "Soft drink and ice manufacturing [BS31211]",                                                                                                         
                  "Breweries [BS31212]",                                                                                                                                
                  "Wineries and distilleries [BS3121A]",                                                                                                                
                  "Tobacco manufacturing [BS31220]",                                                                                                                   
                  "Textile and textile product mills [BS31A00]",                                                                                                        
                  "Clothing and leather and allied product manufacturing [BS31B00]",                                                                                    
                  "Wood product manufacturing [BS32100]",                                                                                                               
                  "Pulp, paper and paperboard mills [BS32210]",                                                                                                         
                  "Converted paper product manufacturing [BS32220]",                                                                                                    
                  "Printing and related support activities [BS32300]",                                                                                                  
                  "Petroleum and coal product manufacturing [BS32400]",                                                                                                 
                  "Basic chemical manufacturing [BS32510]",                                                                                                             
                  "Pesticide, fertilizer and other agricultural chemical manufacturing [BS32530]",                                                                      
                  "Pharmaceutical and medicine manufacturing [BS32540]",                                                                                                
                  "Miscellaneous chemical product manufacturing [BS325C0]",                                                                                             
                  "Plastic product manufacturing [BS32610]",                                                                                                            
                  "Rubber product manufacturing [BS32620]",                                                                                                             
                  "Non-metallic mineral product manufacturing (except cement and concrete products) [BS327A0]",                                                         
                  "Cement and concrete product manufacturing [BS32730]",                                                                                                
                  "Primary metal manufacturing [BS33100]",                                                                                                              
                  "Fabricated metal product manufacturing [BS33200]",                                                                                                   
                  "Machinery manufacturing [BS33300]",                                                                                                                  
                  "Computer and peripheral equipment manufacturing [BS33410]",                                                                                          
                  "Electronic product manufacturing [BS334B0]",                                                                                                         
                  "Electrical  equipment and component manufacturing [BS335A0]",                                                                                        
                  "Household appliance manufacturing [BS33520]",                                                                                                        
                  "Motor vehicle manufacturing [BS33610]",                                                                                                              
                  "Motor vehicle body and trailer manufacturing [BS33620]",                                                                                             
                  "Motor vehicle parts manufacturing [BS33630]" ,                                                                                                       
                  "Aerospace product and parts manufacturing [BS33640]",                                                                                                
                  "Railroad rolling stock manufacturing [BS33650]",                                                                                                     
                  "Ship and boat building [BS33660]",                                                                                                                   
                  "Other transportation equipment manufacturing [BS33690]",                                                                                             
                  "Furniture and related product manufacturing [BS33700]",                                                                                              
                  "Miscellaneous manufacturing [BS33900]")  ~
                            "Manufacturing",
    Sector %in% c("Wholesale trade [BS41000]") ~ "Wholesale trade",
    Sector %in% c("Retail trade [BS4A000]") ~ "Retail trade",
    Sector %in% c("Air transportation [BS48100]",
                  "Rail transportation [BS48200]",
                  "Water transportation [BS48300]",
                  "Truck transportation [BS48400]",
                  "Transit, ground passenger and scenic and sightseeing transportation, taxi and limousine service and support activities for transportation [BS48B00]",
                  "Pipeline transportation [BS48600]",                                                                                                                  
                  "Postal service and couriers and messengers [BS49A00]",                                                                                               
                  "Warehousing and storage [BS49300]") ~ 
                          "Transportation and warehousing",
    Sector %in% c("Motion picture and sound recording industries [BS51200]",                                                                                            
                  "Radio and television broadcasting [BS51510]",                                                                                                        
                  "Publishing, pay/specialty services, telecommunications and other information services [BS51B00]") ~
                          "Information and culture industries",
    Sector %in% c("Depository credit intermediation and monetary authorities [BS52B00]",                                                                                
                  "Insurance carriers [BS52410]") ~
                          "Finance and insurance",
    Sector %in% c("Lessors of real estate [BS53110]",                                                                                                                   
                  "Owner-occupied dwellings [BS5311A]",
                  "Rental and leasing services and lessors of non-financial intangible assets (except copyrighted works) [BS53B00]") ~
                          "Real estate and rental leasing",
    Sector %in% c("Legal, accounting and architectural, engineering and related services [BS541C0]",                                                                    
                  "Computer systems design and other professional, scientific and technical services [BS541D0]",                                                        
                  "Advertising, public relations and related services [BS54180]") ~
                          "Professional, scientific and technical services",
    Sector %in% c("Administrative and support services [BS56100]",
                  "Waste management and remediation services [BS56200]") ~
                          "Administrative and support, waste management and remediation services",
    Sector %in% c("Educational services [BS61000]") ~ "Educational services",
    Sector %in% c("Health care and social assistance [BS62000]") ~ "Health care and social assistance",
    Sector %in% c("Arts, entertainment and recreation [BS71000]") ~ "Arts, entertainment and recreation", 
    Sector %in% c("Accommodation and food services [BS72000]") ~  "Accommodation and food services",
    Sector %in% c("Repair and maintenance [BS81100]",                                                                                                                   
                  "Personal services and private households [BS81A00]", 
                  "Professional and similar organisations [BS81300]",
                  "Religious organizations [NP81310]") ~
                          "Other services"))
  # dd <- subset(GHGE_df, grepl("BS11", Sector))
    
###======================###
###  Cleaning GDP_df

df <- data.frame()
df_GHGE <- data.frame(GHGE_df = unique(GHGE_df$Sector))
df_GDP <- data.frame(GDP_df = unique(GDP_df$`North American Industry Classification System (NAICS)`))
df_EMPL <- data.frame(EMPL_df = unique(EMPL_df$`North American Industry Classification System (NAICS)`))



df <- GHGE_df |> filter(COORDINATE == 1.1,
                        Year == 2009) 

dd <- subset(GDP_df, grepl("212", `North American Industry Classification System (NAICS)`)) |> 
          filter(Year == 2019,
                 Value == "Chained (2012) dollars",
                 GEO == "Alberta")

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

GDP_dashdata <- GDP_df[grepl(paste(Sectors_GDP, collapse = "|"),  GDP_df$`North American Industry Classification System (NAICS)`), ] |> 
  filter(Year == 2019,
         Value == "Chained (2012) dollars",
         GEO == "Alberta")


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
         `Labour force characteristics` %in% c("Employment", "Unemployment rate"),
         GEO == "Alberta") |>
        select(REF_DATE, GEO, `Labour force characteristics`, `North American Industry Classification System (NAICS)`, Sex, `Age group`, UOM, SCALAR_FACTOR, VALUE, Year) |>
        mutate_all(~replace(., is.na(.), 0))|>
        mutate(`North American Industry Classification System (NAICS)` = ifelse(grepl(paste(c("111", "113", "114"), collapse = "|"),  `North American Industry Classification System (NAICS)`),
                                                                                "Agriculture, forestry, fishing and hunting", `North American Industry Classification System (NAICS)`))|>
        group_by(`North American Industry Classification System (NAICS)`, Year, REF_DATE, GEO, `Labour force characteristics`, Sex, `Age group`, UOM, SCALAR_FACTOR)|>
          summarise(VALUE = ifelse(`Labour force characteristics` == "Employmnet", sum(VALUE), max(VALUE)))

