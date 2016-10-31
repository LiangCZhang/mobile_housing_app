Download_HPI_AT_metro <- function(){
  
  message(paste0("Downloading data ", Sys.time()))
  
  read.csv(
    file = "http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_metro.csv", 
    stringsAsFactors = FALSE,
    header = FALSE,
    col.names = c('Location',
                  'Zip_Code',
                  'Year',
                  'Quarter', 
                  'Index',
                  'Change')
  ) %>% 
    tbl_df() %>% 
    select(
      Location,
      Year,
      Quarter,
      Index
    ) %>% 
    # Only use data after 2000
    filter(
      Year > 2000L
    ) %>% 
    mutate(
      Index = as.numeric(Index),
      Year_Quarter = Year + ((Quarter - 1) / 4)
    ) %>% 
    separate(
      Location,
      sep = ", ",
      c("City",
        "State")
    ) %>% 
    mutate(
      State = substr(
        State,
        1,
        2
      )
    ) %>% 
    mutate(
      City_State = paste0(City, '_', State)
    )
}

Index_HPI_AT_metro <- function(HPI_AT_metro, Index_Year){
  
  HPI_AT_metro %>% 
    filter(
      Year >= Index_Year
    ) %>% 
    group_by(
      City_State
    ) %>% 
    mutate(
      Change =
        round(log(Index / Index[Year_Quarter == as.numeric(Index_Year)]), 2)
    ) %>% 
    ungroup()
}
