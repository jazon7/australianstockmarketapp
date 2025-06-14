library(rvest)
library(tidyverse)
library(readxl)
library(httr)
library(writexl)

# README ------------------------------------------------------------------
# Execute this code to update the Australian Stock Market Annual Returns to
# latest available year. The bottleneck for data availability is CPI from the ABS. 
# The December CPI results are released in Feb/March the 
# following year.


# Code --------------------------------------------------------------------

#import the base dataset
australianstockmarket <- 
  readRDS("data/archive/australianstockmarket_2023.RDS") %>% 
  rbind(data.frame(
    year = 1882,
    stock_accumulation = NA, 
    stock_price = NA, 
    bonds = NA, 
    inflation = NA  
  ), .)
#set the update year as the first of March of the next year in the series
update_year <- 
  australianstockmarket$year %>% 
  max() + 2

update_date <-
  paste0(update_year,"-03","-01") %>% 
  as.Date


# function to extract the creation date of a file  -----------------------
find_create_date_of_file <- function(path) {
  return(as.Date(file.info(path)$ctime))
}

# function to remove na and extract year and month from date into --------
clean_df <- function(df){
  df <- na.omit(df)
  df$date <- as.Date(df$date)
  df$year <- year(ymd(df$date))
  df$month <- month(ymd(df$date))
  df$day <- as.numeric(format(
    df$date, 
    format="%d")) 
  return(df)
}


# function to create summary table of Dec Ave index value and % c --------
dec_summary <- function(df){
  df %>% 
    group_by(year, month) %>% 
    summarise(ave_index = round(mean(index),2)) %>%
    ungroup() %>% 
    filter(month == 12) %>% 
    as.data.frame() %>% 
    mutate(pct_change = 
             (ave_index-(lag(ave_index,1))) / 
             lag(ave_index,1)) %>% 
    as_tibble()
  
}


# function to downnload source data (csv,xls,xlsx) to data/source --------
download_file <- function(url, file_name, mode, type){
  
  headers = c(
    `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
  )
  
  dest_file <-
    paste0("./data/",
           file_name,
           type)
  
  if (!file.exists(dest_file)){
    print(paste0("Downloading... ", file_name, type, "....."))
    download.file(url = url,
                  destfile = dest_file,
                  mode = mode,
                  headers = headers)
  } else if (file.exists(dest_file)
             & find_create_date_of_file("data/cpi.xlsx") < update_date
             & Sys.Date() > update_date)
  {
    print(paste0("Updating... ", file_name, type, "....."))
    download.file(url = url,
                  destfile = dest_file,
                  mode = mode,
                  headers = headers)
    
  }else{
    print(paste0(file_name,type," is up to date"))
  }
  
}


# function to web scrape abs web page to find latest cpi release  --------
find_cpi_release <- function(){
  
  page <-
    read_html("https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release")
  
  links <-
    page %>%
    html_nodes("a")%>%
    html_attr("href")
  
  indx <-
    match(1,
          str_detect(links, "quarter"))
  
  chr <-
    links[indx]
  
  out <-
    sub(".*consumer-price-index-australia/", "", chr)
  
  return(out)
}

# function to get cpi based on year input ---------------------------------
get_cpi <- 
  function(yr){
    
    cpi_dec_exist <- 
      cpi %>% 
      filter(year == yr & 
               month == 12) %>% 
      pull()
    
    if(length(cpi_dec_exist == 0)){
      
      latest_return <- cpi %>% 
        filter(year == yr, month == 12) %>% 
        select(pct_change_yoy) %>% 
        pull()
    }
    else{
      
      latest_return <- NA
      
    }
    
    return(round(latest_return,3))
    
  }


# variable to store cpi file name to download from abs website -----------
cpi_xlsx_file <- 
  paste0("https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/",
         find_cpi_release(),
         "/640101.xlsx")

# download All Ordinaries Total Return index fromspglobal to data --------
url <- "https://www.spglobal.com/spdji/en/idsexport/file.xls?hostIdentifier=48190c8c-42c4-46af-8d1a-0cd5db894797&redesignExport=true&languageId=1&selectedModule=PerformanceGraphView&selectedSubModule=Graph&yearFlag=tenYearFlag&indexId=124632"

headers <- c(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
)

response <- httr::GET(url, add_headers(.headers=headers))

if (status_code(response) == 200) {
  writeBin(content(response, "raw"), "data/ord_tr.xls")
  print("File downloaded successfully")
} else {
  print(paste("Failed to download file. Status code:", status_code(response)))
}


# download All Ordinaries Price index from spglobal to data direc --------
url <- "https://www.spglobal.com/spdji/en/idsexport/file.xls?hostIdentifier=48190c8c-42c4-46af-8d1a-0cd5db894797&redesignExport=true&languageId=1&selectedModule=PerformanceGraphView&selectedSubModule=Graph&yearFlag=tenYearFlag&indexId=124533"

headers <- c(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
)

response <- GET(url, add_headers(.headers=headers))

if (status_code(response) == 200) {
  writeBin(content(response, "raw"), "data/ord.xls")
  print("File downloaded successfully")
} else {
  print(paste("Failed to download file. Status code:", status_code(response)))
}



# download Austrlaian Government 10 Year bonds to data directory ---------
download_file(url =
                "https://www.rba.gov.au/statistics/tables/xls/f02d.xlsx",
              file_name = "bonds",
              mode = "wb",
              type = ".xlsx")


# download cpi index from ABS --------------------------------------------
download_file(cpi_xlsx_file,
              file_name = 'cpi', 
              mode = "wb",
              type = ".xlsx")


#################################################################################
##function to get cpi based on year input
#################################################################################
get_cpi <- 
  function(yr){
    
    cpi_dec_exist <- 
      cpi %>% 
      filter(year == yr & 
               month == 12) %>% 
      pull()
    
    if(length(cpi_dec_exist == 0)){
      
      latest_return <- cpi %>% 
        filter(year == yr, month == 12) %>% 
        select(pct_change_yoy) %>% 
        pull()
    }
    else{
      
      latest_return <- NA
      
    }
    
    return(round(latest_return,3))
    
  }


######################################################################
#ASX market data
######################################################################
# read total return data into data frame
ord_tr <- 
  read_excel("data/ord_tr.xls", 
             skip = 6, 
             col_types = c("date","numeric"),
             col_names = c("date", "index"),
             trim_ws = T) 

# read price return data into data frame
ord <- 
  read_excel("data/ord.xls", 
             skip = 6, 
             col_types = c("date","numeric"),
             col_names = c("date", "index"),
             trim_ws = T) 

# clean all ordinaries total return 
ord_tr <- 
  clean_df(ord_tr)

# clean all ordinaries price return
ord <- 
  clean_df(ord)

#table of December average index value (total return) and % change YOY
dec_tr <- 
  dec_summary(ord_tr)

#table of December average index value (price) and % change YOY
dec_ord <- 
  dec_summary(ord)


######################################################################
#cpi data
######################################################################

# extract headers from cpi xlsx file
headers_cpi <- 
  read_excel("data/cpi.xlsx", 
             sheet = "Data1",
             col_names = FALSE,
             skip = 0, 
             n_max = 1)

#read cpi data into data frame
cpi <- 
  read_excel("data/cpi.xlsx",
             skip = 9,
             sheet = "Data1",
             col_types = "guess",
             trim_ws = T,
             col_names = T) 

headers_cpi <- 
  cbind(" " = "date", headers_cpi)

colnames(cpi) <- 
  headers_cpi

cpi <- 
  cpi %>% 
  select(c("date","Index Numbers ;  All groups CPI ;  Australia ;"))

# cpi <- 
#   cpi[c(1,10)] %>% 
#   as.data.frame() 


colnames(cpi)[2] <- 
  "index"

cpi$date <- 
  as.Date(cpi$date)

#create column calculating the percent change cpi year on year 
cpi <- 
  cpi %>% 
  mutate(pct_change_yoy = (index - lag(index,4)) / lag(index,4)) %>% 
  as_tibble()

cpi <- 
  clean_df(cpi)


######################################################################
#bonds data
######################################################################

#extract headers from bonds xls file
headers_bonds <- 
  read_excel("data/bonds.xlsx",
             col_names = FALSE,
             skip = 1, 
             n_max = 1)

#rename headers first column to date
headers_bonds[1] <- "date"

#read bonds data into data frame 
bonds <- 
  read_excel("data/bonds.xlsx",
             skip = 10,
             trim_ws = T,
             col_types = "guess")

#set column names of bonds dataframe from extracted headers
colnames(bonds) <- 
  headers_bonds

#convert date column to date format
bonds$date <- 
  as.Date(bonds$date, format = "%d-%b-%Y")

#select only 10 year government bonds column
bonds <- 
  bonds %>% 
  select(date,`Australian Government 10 year bond`)

#clean bonds data
bonds <- 
  clean_df(bonds)



####################################
#set current year from system date
####################################
current_year <- 
  as.integer(format(
    Sys.Date(),
    "%Y"))

####################################
#find max of given column.
####################################
find_max_column <- 
  function(df, col){
    
    year <- df %>% 
      select({{col}}) %>%
      max() %>% 
      as.integer()
    
    return(year)
  }

####################################
#find min of given column
####################################
find_min_column <- 
  function(df, col){
    
    year <- df %>% 
      select({{col}}) %>%
      min() %>% 
      as.integer()
    
    return(year)
  }

#################################################################################
##function to get cpi based on year input
#################################################################################
get_cpi <- 
  function(yr){
    
    cpi_dec_exist <- 
      cpi %>% 
      filter(year == yr & 
               month == 12) %>% 
      pull()
    
    if(length(cpi_dec_exist == 0)){
      
      latest_return <- cpi %>% 
        filter(year == yr, month == 12) %>% 
        select(pct_change_yoy) %>% 
        pull()
    }
    else{
      
      latest_return <- NA
      
    }
    
    return(round(latest_return,3))
    
  }

#################################################################################
#function to get all ordinaries total return based on year input
#################################################################################
get_total_return <- 
  function(yr){
    
    latest_return <- 
      dec_tr %>% 
      filter(year == yr, month == 12) %>% 
      select(pct_change) %>% 
      pull()
    
    return(round(latest_return,3))
    
  }

#################################################################################
#function to get all ordinaries price return based on year input
#################################################################################
get_price_return <- 
  function(yr){
    
    latest_return <- 
      dec_ord %>% 
      filter(year == yr, month == 12) %>% 
      select(pct_change) %>% 
      pull()
    
    return(round(latest_return,3))
    
  }

#########################################################################
#function to retreive the latest day of available data based on the year
#########################################################################
max_day_bonds <- 
  function(yr){
    
    out <- bonds %>% 
      filter(month == 12 & year == yr) %>% 
      summarise(max = max(day))
    return (as.numeric(out))
  }

#########################################################################
#function to get bonds based on year input
#########################################################################
get_bonds <- 
  function(yr){
    
    #set day as latest day of available bonds data
    dy <- 
      max_day_bonds(yr)
    
    return_bonds <- 
      bonds %>% 
      filter(year == yr & month == 12 & day == dy) %>% 
      select(`Australian Government 10 year bond`) %>% 
      pull()
    
    return(return_bonds/100)
    
  }


####################################
#function to update annual return data 
#with latest stock tr , price, bonds, and inflation
####################################
update_data <- function(df, latest_year) {
  new_row <-  c(latest_year,
                get_total_return(latest_year),
                get_price_return(latest_year),
                get_bonds(latest_year),
                get_cpi(latest_year)
  )
  
  df <-
    rbind(df, new_row)
  
  return(df)
}


#find latest year for data
latest_year <- 
  find_max_column(dec_ord, 
                  year)
#check if current month is March of the update year and if so update the dataset
if (year(Sys.Date()) == update_year & month(Sys.Date()) >= 3) {
  print("Updating Data. This may take a while..........")
  
  update_data(australianstockmarket, latest_year) %>% 
    drop_na() %>%
    saveRDS(paste0("data/australianstockmarket.RDS"))

  update_data(australianstockmarket, latest_year) %>%
    drop_na() %>%
    write.csv(paste0("data/australianstockmarket.csv"))
  
  
  print(paste("Stock market series has been updated", latest_year))

} else {
  print(paste0("Data not available until ", update_date, ". Try again on this date."))
}