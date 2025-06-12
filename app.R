library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(rvest)
library(DT)
library(thematic)
library(showtext)
library(bslib)

options(warn=-1)
options(scipen = 999)

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Open Sans"),
  primary = "#0d6efd",          # Customize primary color if desired
  "font-size-base" = "1.1rem"   # Slightly larger base font for readability
)

####################################
#convert data to tidy format (pivot longer)
####################################
convert_to_tidy <- 
  function(df, nam_to_name = 'market', val_to_name){
    
    columns <- 
      df %>% 
      select(-year) %>% 
      colnames()
    
    df <- df %>% 
      pivot_longer(cols = columns,
                   names_to = nam_to_name,
                   values_to = val_to_name) %>% 
      mutate_if(is.character,as.factor) %>%
      #mutate(year = ymd(year, truncated = 2L))
      
      return(df)
  }

####################################
#function for creating new index column
####################################
calc_index <- 
  function(col_1, col_2, lag){
    
    out <- 
      lag({{col_1}}[lag] * cumprod(lead({{col_2}} + lag)))
    
    return(out)
  }

####################################
#function for creating new real return column
####################################
calc_real_return <- 
  function(col_1, col_2){
    
    out <- 
      (((1+{{col_1}})/(1+{{col_2}}))-1)
    
    return(out)
  }

####################################
#function for creating new rolling returns column
####################################
create_rr_col <- 
  function(df, group_by_col, calc_col, roll_years) {
    
    name_to_pass <- 
      paste0(roll_years)
    
    out <- 
      df %>%
      group_by({{group_by_col}}) %>%
      mutate(!!name_to_pass := ({{calc_col}} / 
                                  lag({{calc_col}},roll_years)) ^ 
               (1/roll_years) -1 )
    
    return(out)
  }

####################################
#function for creating rolling returns dataframe
####################################
create_rr_df <- function(df, filter = F, no_years = NULL){
  
  out <- 
    df %>% 
    create_rr_col(.,market,dollars,no_years) %>%
    # create_rr_col(.,market,dollars,5) %>%
    # create_rr_col(.,market,dollars,8) %>%
    # create_rr_col(.,market,dollars,13) %>%
    # create_rr_col(.,market,dollars,21) %>% 
    pivot_longer(cols = !c(year,market,dollars), 
                 names_to = 'roll_years', 
                 values_to = 'percent') %>% 
    mutate_if(is.character,as.factor) %>% 
    select(-dollars)
  
  
  if(filter == T){
    out <- 
      out %>%  
      filter(roll_years == no_years)
  }
  return(out)
  
}

#######################################################
# function for line plot using ggplot and plotly
#######################################################
line_plotly <- 
  function(df, 
           x_col, 
           y_col,
           x_label = NULL,
           y_label = NULL,
           user_axis_x_lab = F,
           user_axis_y_lab = F,
           colour_col, 
           hline_intercept = 0, 
           hline_colour = 'darkgrey',
           log = "identity",
           plotly = TRUE, 
           x_limits = c(1882,2022), 
           x_breaks = seq(1882,2022,10),
           y_breaks = NULL,
           y_limits = NULL,
           x_an = NULL,
           y_an = NULL,
           lab_an = NULL,
           leg_txt_size = 10.5,
           scale_type = NULL,
           add_cycles = FALSE,
           cycles_data = NULL,
           ...){
    
    #set colours
    cols <- c("#46B8DA","#5CB85C","#D43F3A", "#9632B8","#EEA236")
    labs <- c("Bonds 10 yr", "Stock price + div", "Stock price")
    
    plot <- 
      df %>%
      ggplot(aes(x = {{x_col}}, 
                 y = {{y_col}})) +
      geom_line(aes(colour = {{colour_col}}),
                linetype = 1,
                lwd = 0.7) +
      geom_hline(yintercept = hline_intercept,
                 colour = hline_colour) +
      {if(add_cycles == T) geom_line(
        data = cycles_data, 
        aes(x = x, y = y),
        linetype = 1,
        lwd = 0.2
      )} +
      {if(user_axis_x_lab == T)xlab(x_label)} +
      {if(user_axis_y_lab == T)ylab(y_label)} +
      scale_x_continuous(limits = x_limits,
                         breaks = x_breaks) +
      scale_color_manual(labels = labs,
                         values = cols) +
      scale_y_continuous(trans = log,
                         labels = scale_type,
                         limits = y_limits,
                         breaks = y_breaks) +
      annotate('text', x = x_an, y = y_an, label = lab_an) +
      theme_minimal() +
      theme(
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11),
        legend.text = element_text(size = leg_txt_size),
        legend.position = "top",
        legend.title=element_blank(),
        axis.title.y = 
          element_text(size=12,
                       margin = margin(t = 0, r = 0.75, b = 0, l = 0, unit = 'cm')),
        axis.title.x = 
          element_text(size=12,
                       margin = margin(t = 0.75, r = 0, b = 0, l = 0, unit = 'cm')))
    
    
    #render as interactive plot
    if(plotly == TRUE) {
      plot <- 
        ggplotly(plot)
      plot}
    else{
      plot
      
    }
  }

find_min_max <- function(df, col, min = T){
  
  if(min == T){
    out <-
      df %>% 
      select({{col}}) %>% 
      pull() %>% 
      min()
  }
  else{
    out <-
      df %>% 
      select({{col}}) %>% 
      pull() %>% 
      max()  
  }
  
  return(out)
}

round_all_numeric <- function(df, dig){
  out <-
    df %>% 
    mutate_if(is.numeric,
              round,
              digits = dig)
  
  return(out)
    
}

#function to calculate cagr between two given years
calc_cagr <- function(df, year_1, year_2, which_market){
  
  n <- year_2 - year_1
  
  cagr_1 <- 
    df %>% 
    filter(year == c(year_1)) %>% 
    filter(market == which_market) %>% 
    select(dollars) %>% 
    pull()
  
  cagr_2 <- 
    df %>% 
    filter(year == c(year_2)) %>% 
    filter(market == which_market) %>% 
    select(dollars) %>% 
    pull()
  
  cagr <- 
    (cagr_2/cagr_1)^(1/n)-1
  
  return(signif(cagr,3) * 100)
  
}


calc_table <- function(df, year_1, year_2){
  
  n <- year_2 - year_1
  
  df <-
    df %>% 
    filter(year %in% c(year_1,year_2)) %>% 
    group_by(market) %>% 
    mutate(cagr = signif((dollars/lag(dollars,1))^(1/n)-1),3) %>%
    filter(year == year_2) %>% 
    select(market, cagr) %>% 
    pivot_wider(names_from = market, values_from = cagr)

  
  return(df)
  
}

#function to create a sinewave dataframe
make_sine_wave <- 
  function(var1,
           var2, 
           freq_in_months, 
           y_min = 0,
           y_max = 0.2, 
           up_sample = 100,
           offset = 0){
    
  # length of x axis 
  x_range <- var2 - var1
  
  # y axis range (i.e. difference)
  y_range <- c(y_min, y_max)
  
  #convert from months to years and take inverse 
  freq <- freq_in_months/12
  freq <- 1/freq
  
  offset <- offset/12
  
  #create dataframe of x and y
  x1 <- 1:(x_range*up_sample)/up_sample
  amp <- (y_range[2] - y_range[1])/2
  y1 <- amp*cos(2*pi*freq*x1) + amp
  
  df <- 
    data.frame(x = x1, y = y1) %>% 
    mutate(y = y - y_range[2]/2) %>% 
    mutate(x = x + var1 + offset)
  
  return(df)
  
}


######################################################################
#create final data frames
######################################################################
australianstockmarket <- 
  readRDS("data/australianstockmarket.RDS") %>% 
  rbind(data.frame(
    year = 1882,
    stock_accumulation = NA, 
    stock_price = NA, 
    bonds = NA, 
    inflation = NA  
  ), .)

#create new real return column and index columns for nominal and real returns
stock_temp <- 
  australianstockmarket %>% 
  mutate(across(-c(year,inflation), 
                ~calc_real_return(., inflation), .names = '{.col}_rr')) %>% 
  mutate(across(c(stock_accumulation,stock_price,bonds,inflation), 
                ~ifelse(year == 1882, 100, NA), .names = '{.col}_indx')) %>% 
  mutate(stock_accumulation_indx = 
           calc_index(stock_accumulation_indx, stock_accumulation , 1)) %>% 
  mutate(stock_price_indx = 
           calc_index(stock_price_indx, stock_price , 1))  %>% 
  mutate(bonds_indx = 
           calc_index(bonds_indx, bonds , 1)) %>% 
  mutate(inflation_indx = 
           calc_index(inflation_indx, inflation , 1)) %>%
  mutate(across(c(stock_accumulation_rr,stock_price_rr,bonds_rr), 
                ~ifelse(year == 1882, 100, NA), .names = '{.col}_indx')) %>% 
  mutate(stock_accumulation_rr_indx = 
           calc_index(stock_accumulation_rr_indx, stock_accumulation_rr , 1)) %>% 
  mutate(stock_price_rr_indx = 
           calc_index(stock_price_rr_indx, stock_price_rr , 1))  %>% 
  mutate(bonds_rr_indx = 
           calc_index(bonds_rr_indx, bonds_rr , 1))


min_yr <- 
  find_min_max(australianstockmarket, 
                       year, 
                       min = T)
max_yr <- 
  find_min_max(australianstockmarket, 
                       year, 
                       min = F)


# function to create index values based on minimum and maximum input years.
create_index_df <- function(real = F, 
                            min_year = min_yr, 
                            max_year = max_yr ){
  
  df <- 
    australianstockmarket %>%
    filter(between(year, min_year, max_year)) %>% 
    mutate(across(-c(year,inflation), 
                  ~calc_real_return(., inflation), .names = '{.col}_rr')) %>% 
    mutate(across(c(stock_accumulation,stock_price,bonds,inflation), 
                  ~ifelse(year == min_year, 100, NA), .names = '{.col}_indx')) %>% 
    mutate(stock_accumulation_indx = 
             calc_index(stock_accumulation_indx, stock_accumulation , 1)) %>% 
    mutate(stock_price_indx = 
             calc_index(stock_price_indx, stock_price , 1))  %>% 
    mutate(bonds_indx = 
             calc_index(bonds_indx, bonds , 1)) %>% 
    mutate(inflation_indx = 
             calc_index(inflation_indx, inflation , 1)) %>%
    mutate(across(c(stock_accumulation_rr,stock_price_rr,bonds_rr), 
                  ~ifelse(year == min_year, 100, NA), .names = '{.col}_indx')) %>% 
    mutate(stock_accumulation_rr_indx = 
             calc_index(stock_accumulation_rr_indx, stock_accumulation_rr , 1)) %>% 
    mutate(stock_price_rr_indx = 
             calc_index(stock_price_rr_indx, stock_price_rr , 1))  %>% 
    mutate(bonds_rr_indx = 
             calc_index(bonds_rr_indx, bonds_rr , 1))
  
  
  if(real == F){
    out <- 
      df %>% 
      select(-contains("rr")) %>% 
      select(year, ends_with('indx')) %>% 
      mutate(across(c(-year), 
                    ~ifelse(year == min_year, 100, .))) %>% 
      convert_to_tidy(val_to_name = 'dollars') %>% 
      mutate(market = recode(market,'bonds_indx' = 'Bonds 10yr',
                             'stock_price_indx' = 'Stock Price',
                             'stock_accumulation_indx' = 'Stock Price + Div')) %>% 
      filter(market != "inflation_indx") %>% 
      round_all_numeric(.,0)
  }
  else{
    out <-
      df %>% select(year, contains("rr")) %>% 
      select(year, ends_with('indx')) %>% 
      mutate(across(c(-year), 
                    ~ifelse(year == min_year, 100, .))) %>% 
      convert_to_tidy(val_to_name = 'dollars') %>% 
      mutate(market = recode(market,'bonds_rr_indx' = 'Bonds 10yr',
                             'stock_price_rr_indx' = 'Stock Price',
                             'stock_accumulation_rr_indx' = 'Stock Price + Div')) %>% 
      filter(market != "inflation_indx") %>% 
      round_all_numeric(.,0)  
      
    
  }
  
}

#create nominal and real dataframes for returns and index in long format    
stock_returns_nom <-
  stock_temp %>% select(-contains("rr")) %>%   
  select(year, !ends_with('indx')) %>% 
  convert_to_tidy(val_to_name = 'percent_yoy') %>% 
  mutate(market = recode(market,'bonds' = 'Bonds 10yr',
                         'stock_price' = 'Stock Price',
                         'stock_accumulation' = 'Stock Price + Div'))

stock_index_nom <- 
  stock_temp %>% select(-contains("rr")) %>% 
  select(year, ends_with('indx')) %>% 
  mutate(across(c(-year), 
                ~ifelse(year == 1882, 100, .))) %>% 
  convert_to_tidy(val_to_name = 'dollars') %>% 
  mutate(market = recode(market,'bonds_indx' = 'Bonds 10yr',
                         'stock_price_indx' = 'Stock Price',
                         'stock_accumulation_indx' = 'Stock Price + Div'))

stock_returns_real <-
  stock_temp %>% select(year, contains("rr")) %>% 
  select(year, !ends_with('indx'))  %>% 
  convert_to_tidy(val_to_name = 'percent_yoy') %>% 
  mutate(market = recode(market,'bonds_rr' = 'Bonds 10yr',
                         'stock_price_rr' = 'Stock Price',
                         'stock_accumulation_rr' = 'Stock Price + Div'))

stock_index_real <- 
  stock_temp %>% select(year, contains("rr")) %>% 
  select(year, ends_with('indx')) %>% 
  mutate(across(c(-year), 
                ~ifelse(year == 1882, 100, .))) %>% 
  convert_to_tidy(val_to_name = 'dollars') %>% 
  mutate(market = recode(market,'bonds_rr_indx' = 'Bonds 10yr',
                         'stock_price_rr_indx' = 'Stock Price',
                         'stock_accumulation_rr_indx' = 'Stock Price + Div'))






#set href links
ref_brail_2008 <- "https://research.bond.edu.au/en/publications/re-examination-of-the-historical-equity-risk-premium-in-australia"
ref_brail_2012 <- "https://research.bond.edu.au/en/publications/the-historical-equity-risk-premium-in-australia-post-gfc-and-128-"
ref_snp <- "https://www.spglobal.com/spdji/en/indices/equity/all-ordinaries/#overview"
ref_rba <- "https://www.rba.gov.au/statistics/tables"
ref_abs <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release"



# Define UI for application ############################# 
ui <- fluidPage(

  theme = app_theme,
  
  tags$head(
    tags$style(HTML("a{font-size: 95%;}"))
  ),
  
  
  titlePanel("Australian Stock Market App"),
  
  tags$br(),
  
  sidebarLayout(position = 'left',
                sidebarPanel(
                  conditionalPanel(condition="input.tabselected==1",
                                   
                                   tags$br(),
                                   materialSwitch(inputId = "index_plot_type", label = "Adjust for inflation", value = FALSE, status = 'info'),
                                   tags$br(),
                                   sliderInput("slider_index_plot", 
                                               label = h3("Select year range"), 
                                               min = find_min_max(stock_temp, year, min = T), 
                                               max = find_min_max(stock_temp, year, min = F), 
                                               value = c(1882, find_min_max(stock_temp, year, min = F)),
                                               sep = ""),
                                   
                                   
                                   tags$br(),
                                   tags$div(tags$p(HTML("<br><br>
                       Data sources: ")),
                       tags$ul(tags$li(tags$a(href=ref_snp, "S&P Dow Jones Indices")),
                               tags$br(),
                               tags$li(tags$a(href=ref_rba, "Reserve Bank of Australia")),
                               tags$br(),
                               tags$li(tags$a(href=ref_abs, "Australian Bureau of Statistics")),
                               tags$br(),
                               tags$li(tags$a(href=ref_brail_2012 , "Brailsford et al.(2012)"))),
                                   ),
                       
                  ),
                  
                  conditionalPanel(condition = "input.tabselected==2", 
                                   
                                   tags$br(),
                                   materialSwitch(inputId = "returns_plot_type", label = "Adjust for inflation", value = FALSE, status = 'info'),
                                   tags$br(),
                                   sliderInput("slider_returns_plot", 
                                               label = h3("Select year range"), 
                                               min = find_min_max(stock_temp, year, min = T), 
                                               max = find_min_max(stock_temp, year, min = F), 
                                               value = c(1882, find_min_max(stock_temp, year, min = F)),
                                               sep = ""),
                  
                
                                  tags$br(),
                                   tags$div(tags$p(HTML("<br><br>
                       Data sources: ")),
                       tags$ul(tags$li(tags$a(href=ref_snp, "S&P Dow Jones Indices")),
                               tags$br(),
                               tags$li(tags$a(href=ref_rba, "Reserve Bank of Australia")),
                               tags$br(),
                               tags$li(tags$a(href=ref_abs, "Australian Bureau of Statistics")),
                               tags$br(),
                               tags$li(tags$a(href=ref_brail_2012 , "Brailsford et al.(2012)"))),
                                   ),
                       
                  ),
                  
                  conditionalPanel(condition = "input.tabselected==3", 
                                   useShinyjs(),
                                   tags$br(),
                                   materialSwitch(inputId = "rolling_plot_type", label = "Adjust for inflation", value = FALSE, status = 'info'),
                                   tags$br(),
                                   sliderInput("slider_rolling_returns_plot", 
                                               label = h3("Select year range"), 
                                               min = find_min_max(stock_temp, year, min = T), 
                                               max = find_min_max(stock_temp, year, min = F), 
                                               value = c(1882, find_min_max(stock_temp, year, min = F)),
                                               sep = ""),
                                   
                                   
                                   tags$br(),
                                   tags$br(),
                                   selectizeInput('rolling_years_input', 'Select rolling years: ',
                                                  choices = seq(3,21,by = 1),
                                                  
                                                  selected = 3),
                                   tags$br(),
                                   tags$br(),
                                   materialSwitch(inputId = "add_cycles", label = "Add cycles", value = FALSE, status = 'info'),
                                   tags$br(),
                                   hidden(numericInput("cycle_months", "Months per cycle:", 120, min = 1, max = 480),
                                   tags$br(),
                                   numericInput("cycle_months_offset", "Months offset:", 0, min = 1, max = 480)),
                                   tags$div(tags$p(HTML("<br>
                       Data sources: ")),
                       tags$ul(tags$li(tags$a(href=ref_snp, "S&P Dow Jones Indices")),
                               tags$br(),
                               tags$li(tags$a(href=ref_rba, "Reserve Bank of Australia")),
                               tags$br(),
                               tags$li(tags$a(href=ref_abs, "Australian Bureau of Statistics")),
                               tags$br(),
                               tags$li(tags$a(href=ref_brail_2012 , "Brailsford et al.(2012)"))),
                                   ),
                       
                  ),
                  conditionalPanel(condition = "input.tabselected==4", 
                                   
                                   tags$div(tags$p(HTML("<br><br>
                       Data sources: ")),
                       tags$ul(tags$li(tags$a(href=ref_snp, "S&P Dow Jones Indices")),
                               tags$br(),
                               tags$li(tags$a(href=ref_rba, "Reserve Bank of Australia")),
                               tags$br(),
                               tags$li(tags$a(href=ref_abs, "Australian Bureau of Statistics")),
                               tags$br(),
                               tags$li(tags$a(href=ref_brail_2012 , "Brailsford et al.(2012)"))),
                                   )
                       
                  )
                  
                  ,width = 3),
                
                
                mainPanel(
                  tabsetPanel(type = 'tabs',
                              id = 'tabselected',
                              selected = 1,
                              tabPanel("Index",
                                       fluid = T, 
                                       value = 1, 
                                       plotlyOutput("index_plot", 
                                                    width = '100%',
                                                    height = 600),
                                       tags$br(),
                                       tags$br(),
                                       tags$h4("Annualised Returns"), 
                                       DT::dataTableOutput("index_cagr_table"),
                                       tags$br(),
                                       tags$br(),
                                       tags$h4("Data output"), 
                                       DT::dataTableOutput("index_table"),
                                       tags$br(),
                                       tags$figcaption("Values in each market column are in dollars (AUD)")),
                              tabPanel("Returns",
                                       fluid = T, 
                                       value = 2, 
                                       plotlyOutput("returns_plot", 
                                                    width = '100%',
                                                    height = 600),
                                       tags$br(),
                                       tags$br(),
                                       DT::dataTableOutput("returns_table"),
                                       tags$br(),
                                       tags$figcaption("Values in each market column are annualised returns in percent year on year")),
                              tabPanel("Rolling Returns",
                                       fluid = T, 
                                       value = 3, 
                                       plotlyOutput("rolling_returns_plot", 
                                                    width = '100%',
                                                    height = 600),
                                       tags$br(),
                                       tags$br(),
                                       DT::dataTableOutput("rolling_returns_table"),
                                       tags$br(),
                                       tags$figcaption("Values in each market column are annualised returns in percent")),
                              tabPanel("About",
                                       fluid = T, 
                                       value = 4, 
                                       includeHTML("about.Rhtml"),
                                       tags$head(
                                         tags$style("#about_text{
                                                 color:black; 
                                                 font-size:13px; 
                                                 overflow-y:scroll; 
                                                 max-height: 500px; 
                                                 width: 700px; 
                                                 max-width:100%;
                                                 padding-left:10px;
                                                 padding-right:10px;
                                                 background: ghostwhite;
                                                 white-space: pre-wrap;
                                                 }"))),
                              
                  ))
  )
  

)





# Define server logic required to draw a histogram ############################# 
server <- function(input, output) {
  
  observeEvent(input$add_cycles, {
    if(input$add_cycles == FALSE){
      shinyjs::hide("cycle_months")
      shinyjs::hide("cycle_months_offset")
    } else {
      shinyjs::show("cycle_months")
      shinyjs::show("cycle_months_offset")
    }
  })
  
  data_cycles <- reactive({
    
    cycles_y_max <- 
      data_rolling_returns_plot() %>% 
      pivot_wider(names_from = market, 
                  values_from = percent) %>% 
      select(-year, -roll_years) %>% 
      sapply(., max, na.rm = TRUE) %>% 
      max(na.rm = TRUE)
    
    cycles_y_min <- 
      data_rolling_returns_plot() %>% 
      pivot_wider(names_from = market, 
                  values_from = percent) %>% 
      select(-year, -roll_years) %>% 
      sapply(., min, na.rm = TRUE) %>% 
      min(na.rm = TRUE)
    
    cycles_y_mx <-
      (cycles_y_max - cycles_y_min) / 2
    
    cycles_y_mn <-
      (cycles_y_mx / 2) * -1

    out <- 
      make_sine_wave(input$slider_rolling_returns_plot[1],
                     input$slider_rolling_returns_plot[2], 
                     freq_in_months = input$cycle_months,
                     y_min = cycles_y_mn,
                     y_max = cycles_y_mx,
                     offset = input$cycle_months_offset)
    
  })
  
  
  data_index_plot_reactive <- reactive({
    
    if(input$index_plot_type == FALSE){
    out <-
      create_index_df(
        real = F,
        min_year = input$slider_index_plot[1],
        max_year = input$slider_index_plot[2])  
    }else{
    out <- 
      create_index_df(
        real = T,
        min_year = input$slider_index_plot[1],
        max_year = input$slider_index_plot[2])
    }
    
  })
  
  data_index_plot <- reactive({
    if(input$index_plot_type == FALSE){
      out <- stock_index_nom 
    }else{
      out <- stock_index_real
    }
  })
  
  data_returns_plot <- reactive({
    
    if(input$returns_plot_type == FALSE){
      out <- stock_returns_nom %>% 
        filter(between(year, 
                       input$slider_returns_plot[1], 
                       input$slider_returns_plot[2])) %>% 
        filter(market != "inflation") 
    }else{
      out <- stock_returns_real %>% 
        filter(between(year, 
                       input$slider_returns_plot[1], 
                       input$slider_returns_plot[2])) %>% 
        filter(market != "inflation") 
    }
  })
  
  data_rolling_returns_plot <- reactive({
    
    if(input$rolling_plot_type == FALSE){
      out <- 
        create_rr_df(stock_index_nom, 
                     filter = T, 
                     no_years = as.numeric(input$rolling_years_input)) %>% 
        filter(market != "inflation_indx") %>% 
        filter(between(year, 
                       input$slider_rolling_returns_plot[1], 
                       input$slider_rolling_returns_plot[2])) %>% 
        round_all_numeric(.,2)
    }else{
      out <- 
        create_rr_df(stock_index_real, 
                     filter = T, 
                     no_years = as.numeric(input$rolling_years_input)) %>% 
        filter(market != "inflation_indx") %>% 
        filter(between(year, 
                       input$slider_rolling_returns_plot[1], 
                       input$slider_rolling_returns_plot[2])) %>% 
        round_all_numeric(.,2)
    }
  })
  
  
  output$index_plot <- renderPlotly({
    

    if(input$slider_returns_plot[2]-input$slider_returns_plot[1] > 10){
      break_number <- 10
      
    } else{
      
      break_number <- (input$slider_returns_plot[2]-input$slider_returns_plot[1])
    }
    
    max_dollars <- 
      data_index_plot_reactive() %>% 
      find_min_max(dollars, min = F)
    
    min_dollars <-
      data_index_plot_reactive() %>% 
      find_min_max(dollars, min = T)
    
    if(min_dollars > 100){
      min_dollars <- 100
    }
      
    
    data_index_plot_reactive() %>% 
    line_plotly(x_col = year, 
                  y_col = dollars,
                  colour_col = market,
                  log = "log10",
                  scale_type = scales::comma,
                  x_limits = c(input$slider_index_plot[1],input$slider_index_plot[2]),
                  y_limits = c(min_dollars,max_dollars),
                  x_breaks = scales::breaks_pretty(break_number),
                  y_breaks = scales::trans_breaks('log10', function(x) 10^x),
                  x_an = 1892,
                  y_an = 50,
                  # breaks = c(100,1000,10000,100000,1000000,10000000,100000000),
                  lab_an = "$100 invested",
                  user_axis_x_lab = T,
                  user_axis_y_lab = T,
                  x_label = "Year",
                  y_label = "Dollars (AUD)") %>% 
      layout(legend = list(orientation = "h",
                           side = 'top',
                           traceorder = 'normal',
                           xanchor = "center",
                           x = 0.3, 
                           y = 1.15,
                           title = list(text = '')))
    
  })
  
  output$index_table <- DT::renderDT({

    
    DT::datatable(
      data = data_index_plot_reactive() %>% 
        pivot_wider(names_from = market, 
                    values_from = dollars),
      selection = list(target = 'column'),
      # filter = list(position = 'top', clear = TRUE),
      rownames = FALSE,
      style = "bootstrap5",
      class = "table-striped table-hover table-bordered compact",
      extensions = 'Buttons',
      options = list(orderClasses = TRUE,
                     scrollY=TRUE,
                     searching = TRUE,
                     fixedColumns = TRUE,
                     dom = 'lBfrtip',
                     buttons = list(list(extend = 'colvisRestore',
                                         text = "Restore all columns"),
                                    list(extend = 'colvis',
                                         text = "Add/Remove columns"),
                                    list(extend = 'csv',
                                         text = "Download csv",
                                         title = "data",
                                         exportOptions = list(columns = ':visible')
                                    )),
                     lengthMenu = list(c(10, 50, 100, -1),
                                       c('10', '50', "100", 'All')),
                     pageLength = 10,
                     autoWidth = TRUE,
                     paging = TRUE,
                     info = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
                     
      ))
  })
  
  
  output$index_cagr_table <- renderDataTable({

    req(data = data_index_plot_reactive())

    data <- 
      calc_table(data_index_plot_reactive(),
                       input$slider_index_plot[1],
                       input$slider_index_plot[2]) 

    caption <- 
      paste0("Between ",  input$slider_index_plot[1] , " and " ,input$slider_index_plot[2])
   
    
     DT::datatable(
      data = data,
      caption = caption,
      rownames = FALSE,
      style = "bootstrap5",
      class = "table-striped table-hover table-bordered compact",
      options = list(
        paging = FALSE,
        info = FALSE,
        searching = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all")))
      )  %>% 
      formatPercentage(1:3,2)
  })
  
  output$returns_plot <- renderPlotly({
    
    if(input$slider_returns_plot[2]-input$slider_returns_plot[1] > 10){
      break_number <- 10
      
    } else{
      
      break_number <- (input$slider_returns_plot[2]-input$slider_returns_plot[1])
    }
    
    data_returns_plot() %>% 
      line_plotly(x_col = year, 
                  y_col = percent_yoy,
                  colour_col = market,
                  y_breaks = scales::breaks_pretty(6),
                  scale_type = scales::percent_format(accuracy=1),
                  x_limits = c(input$slider_returns_plot[1],input$slider_returns_plot[2]),
                  x_breaks = scales::breaks_pretty(break_number),
                  plotly = T,
                  user_axis_x_lab = T,
                  user_axis_y_lab = T,
                  x_label = "Year",
                  y_label = "Percent (YoY)") %>% 
      layout(legend = list(orientation = "h",
                           side = 'top',
                           traceorder = 'normal',
                           xanchor = "center",
                           x = 0.3, 
                           y = 1.15,
                           title = list(text = '')))
    
  })
  
  output$returns_table <- DT::renderDT({
    DT::datatable(
      data = data_returns_plot() %>% 
        pivot_wider(names_from = market, 
                    values_from = percent_yoy),
      selection = list(target = 'column'),
      filter = list(position = 'top', clear = FALSE),
      rownames = FALSE,
      style = "bootstrap5",
      class = "table-striped table-hover table-bordered compact",
      extensions = 'Buttons',
      options = list(orderClasses = TRUE,
                     scrollY=TRUE,
                     searching = TRUE,
                     fixedColumns = TRUE,
                     dom = 'lBfrtip',
                     buttons = list(list(extend = 'colvisRestore',
                                         text = "Restore all columns"),
                                    list(extend = 'colvis',
                                         text = "Add/Remove columns"),
                                    list(extend = 'csv',
                                         text = "Download csv",
                                         title = "data",
                                         exportOptions = list(columns = ':visible')
                                    )),
                     lengthMenu = list(c(10, 50, 100, -1),
                                       c('10', '50', "100", 'All')),
                     pageLength = 10,
                     autoWidth = TRUE,
                     paging = TRUE,
                     info = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
                     
      ))
  })
  
  output$rolling_returns_plot <- renderPlotly({
    
    if(input$slider_returns_plot[2]-input$slider_returns_plot[1] > 10){
      break_number <- 10
      
    } else{
      
      break_number <- (input$slider_returns_plot[2]-input$slider_returns_plot[1])
    }
    
    if(input$add_cycles == FALSE){
      cycles <- FALSE
      cycles_input <- NULL
      
    }
    else{
      cycles <- TRUE
      cycles_input <- data_cycles()
        
      
    }
    
    
    data_rolling_returns_plot() %>% 
      line_plotly(x_col = year, 
                  y_col = percent,
                  colour_col = market,
                  y_breaks = scales::breaks_pretty(6),
                  scale_type = scales::percent_format(accuracy=1),
                  x_limits = c(input$slider_rolling_returns_plot[1],input$slider_rolling_returns_plot[2]),
                  x_breaks = scales::breaks_pretty(break_number),
                  plotly = T,
                  user_axis_x_lab = T,
                  user_axis_y_lab = T,
                  x_label = "Year",
                  y_label = "Percent",
                  add_cycles = cycles,
                  cycles_data = cycles_input) %>% 
      layout(legend = list(orientation = "h",
                           side = 'top',
                           traceorder = 'normal',
                           xanchor = "center",
                           x = 0.3, 
                           y = 1.15,
                           title = list(text = '')))
    
  })
  
  output$rolling_returns_table <- DT::renderDT({
    DT::datatable(
      data = data_rolling_returns_plot() %>% 
        filter(market != "inflation") %>%
        pivot_wider(names_from = market, 
                    values_from = percent) %>% 
        filter(if_all(c(`Stock Price`), ~ !is.na(.))),
      selection = list(target = 'column'),
      filter = list(position = 'top', clear = FALSE),
      rownames = FALSE,
      style = "bootstrap5",
      class = "table-striped table-hover table-bordered compact",
      extensions = 'Buttons',
      options = list(orderClasses = TRUE,
                     scrollY=TRUE,
                     searching = TRUE,
                     fixedColumns = TRUE,
                     dom = 'lBfrtip',
                     buttons = list(list(extend = 'colvisRestore',
                                         text = "Restore all columns"),
                                    list(extend = 'colvis',
                                         text = "Add/Remove columns"),
                                    list(extend = 'csv',
                                         text = "Download csv",
                                         title = "data",
                                         exportOptions = list(columns = ':visible')
                                    )),
                     lengthMenu = list(c(10, 50, 100, -1),
                                       c('10', '50', "100", 'All')),
                     pageLength = 10,
                     autoWidth = TRUE,
                     paging = TRUE,
                     info = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
                     
      ))
  })
  
  output$about_text <- renderText(about_text)
  
  

}








# Run the application ##########################################################  
shinyApp(ui = ui, server = server)