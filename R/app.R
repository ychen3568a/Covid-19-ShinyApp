
library(shiny)
library(tidyverse)
library(maps)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(shinythemes)
library(fresh)



total_data <- read_csv("../data/Total_Data_New.csv")
HealthExpendPerCapita_01 <- read_csv("../data/0_1_HealthExpendPerCapita.csv")
povertyHeadcount_04 <- read_csv("../data/0_4_povertyHeadcount.csv")
public_totalHeath_02 <- read_csv("../data/0_2_public_totalHeath.csv")
totalHealth_GDP_03 <- read_csv("../data/0_3_totalHealth_GDP.csv")


total_data %>% 
  mutate(country = str_replace(country, "^Congo$", "Republic of Congo"),
         country = str_replace(country, "^Democratic Republic of Congo$", "Democratic Republic of the Congo"),
         country = str_replace(country, "^Antigua and Barbuda$", "Antigua"),
         country = str_replace(country, "^Cote d'Ivoire$", "Ivory Coast"),
         country = str_replace(country, "^Czechia$", "Czech Republic"),
         country = str_replace(country, "^Eswatini$", "Swaziland"),
         country = str_replace(country, "^Micronesia (country)$", "Micronesia"),
         country = str_replace(country, "^North Madedonia$", "Madedonia"),
         country = str_replace(country, "^Saint Kitts and Nevis$", "Saint Kitts"),
         country = str_replace(country, "^Saint Vincent and the Grenadines$", "Saint Vincent"),
         country = str_replace(country, "^Timor$", "Timor-Leste"),
         country = str_replace(country, "^Trinidad and Tobago$", "Trinidad"),
         country = str_replace(country, "^United Kingdom$", "UK"),
         country = str_replace(country, "^United States$", "USA")) ->
  total_data_map

world_map <- map_data("world")

world_map %>% 
  left_join(total_data_map, by = c("region" = "country")) ->
  world_map_new

total_data %>% 
  mutate(cases_real = cases*population/1000000,
         deaths_real = deaths*population/1000000) %>% 
  group_by(date, continent) %>% 
  mutate(continent_cases = sum(cases_real, na.rm = TRUE),
         continent_deaths = sum(deaths_real, na.rm = TRUE)) %>% 
  ungroup()->
  total_data
  
country <- total_data %>% 
  select(country) %>% 
  unique()

country2 <- HealthExpendPerCapita_01 %>% 
  select(Entity) %>% 
  unique()

country3 <- povertyHeadcount_04 %>% 
  select(CountryName) %>% 
  unique()




ui <- fluidPage(theme = shinytheme("sandstone"),
  
  titlePanel("COVID-19 Analyzer"),
  tabsetPanel(type = "pills",
    tabPanel("Global",
             tabsetPanel(
               tabPanel("Map",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("Tab1_var", "Choose a type", choices = c("Cases", "Deaths", "Vaccinations")),
                            sliderInput("Tab1_date", "Choose a date", 
                                        min = as.Date("2020-01-22","%Y-%m-%d"),
                                        max = as.Date("2021-11-04","%Y-%m-%d"),
                                        value=as.Date("2021-01-01"),timeFormat="%Y-%m-%d")),
                          mainPanel(
                            plotOutput("Tab1_map")
                          ))
               ),

               
               tabPanel("World Plot",
                        sidebarPanel(
                          radioButtons("Tab1_var1", "Level", choices = c("Global", "Continent")),
                          radioButtons("Tab1_var2", "Choose a type", choices = c("Cases", "Deaths"))
                        ),
                        mainPanel(
                          plotOutput("Tab2_line")
                        )
                        
               )
             )),
    
    
    tabPanel("Univariate",
             tabsetPanel(
               tabPanel("Cumulative",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("Tab2_country", "Country", choices = country),
                            selectInput("Tab2_type", "Choose a type", choices = c("Cases", "Deaths", "Vaccinations")),
                            selectInput("Tab2_log", "Log Transform?", choices = c("Yes", "No"), selected = "No")
                          ),
                          mainPanel(
                            plotOutput("Tab2_Cumulative")
                          )
                        )),
               tabPanel("Daily",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("Tab2_country2", "Country", choices = country),
                            selectInput("Tab2_type2", "Choose a type", choices = c("Cases", "Deaths")),
                            sliderInput("Tab2_width", "Number of Width?", min = 1, max = 50, value = 5)
                          ),
                          mainPanel(
                            plotOutput("Tab2_hist")
                          )
                        )
                        )
             )
             ),
    
    
  
    
    tabPanel("Multivariate",
             tabsetPanel(
               tabPanel("Cumulative",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("Tab3_country", "Country", choices = country, multiple = TRUE),
                            selectInput("Tab3_type", "Choose a type", choices = c("Cases", "Deaths", "Vaccinations")),
                            selectInput("Tab3_log", "Log Transform?", choices = c("Yes", "No"), selected = "No")
                          ),
                          mainPanel(
                            plotOutput("Tab3_Cumulative")
                          )
                        )),
               tabPanel("Daily",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("Tab3_country2", "Country", choices = country, multiple = TRUE),
                            selectInput("Tab3_type2", "Choose a type", choices = c("Cases", "Deaths")),
                            sliderInput("Tab3_width", "Number of Width?", min = 1, max = 50, value = 5)
                          ),
                          mainPanel(
                            plotOutput("Tab3_hist")
                          )
                        )
               )
             )
             
    ),
    tabPanel("Reason Analysis",
             tabsetPanel(
               tabPanel("Heathcare",
                        tabsetPanel(
                          tabPanel("Visualization",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("Tab4_country", "Country", choices = country2, multiple = TRUE)
                                     ), 
                                     mainPanel(
                                       fluidRow(
                                         plotOutput("Tab4_plot1")
                                       ),
                                       fluidRow(
                                         plotOutput("Tab4_plot2")
                                       ),
                                       fluidRow(
                                         plotOutput("Tab4_plot3")
                                       )
                                     )
                                   )),
                          tabPanel("Table",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("Tab4_Year", "Year", choices = seq(2004, 2014)),
                                       selectInput("Tab4_type", "Choose a type", choices = c("Health Expenditure (% of GDP)", "Health Expenditure in Public", "Health Expenditure per Capita"))
                                     ),
                                     mainPanel(
                                       dataTableOutput("Tab4_table1")
                                     )
                                   ))
                        )),
               tabPanel("Poverty",
                        tabsetPanel(
                          tabPanel("Visualization",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("Tab4_country2", "Country", choices = country3, multiple = TRUE)
                                     ),
                                     mainPanel(
                                       plotOutput("Tab4_povertyPlot")
                                     )
                                   )),
                          tabPanel("Table",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("Tab4_Year2", "Year", choices = seq(2004, 2019))
                                     ),
                                     mainPanel(
                                       dataTableOutput("Tab4_table2")
                                     )
                                   )
                                   )
                        ))
             )
             
      
    )
  )
)


server <- function(input, output, session) {
  
  # Tab 1 Map
  
  output$Tab1_map <- renderPlot({
    if(input$Tab1_var == "Cases") {
      world_map_new %>%   
        filter(date == input$Tab1_date) %>% 
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill= cases*population/1000000), colour = "gray50") +
        scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +
        scale_y_continuous(breaks = seq(-60, 100, 30), labels = function(x){paste0(x, "°")}) +
        scale_fill_gradient(low = "lightblue", high="steel blue") +
        geom_polygon(data = world_map,  aes(x = long, y = lat, group = group), fill = NA,  colour = "gray50") +
        labs(title="Total Cases by Country",
             y="", x="",
             subtitle = paste0("This plot shows the total number of COVID-19 
cases in each country on ", input$Tab1_date),
             fill = "Cases") +
        theme_classic()
    } else if(input$Tab1_var == "Deaths") {
      world_map_new %>%   
        filter(date == input$Tab1_date) %>% 
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill= deaths*population/1000000), colour = "gray50") +
        scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +
        scale_y_continuous(breaks = seq(-60, 100, 30), labels = function(x){paste0(x, "°")}) +
        scale_fill_gradient(low = "lightblue", high="steel blue") +
        geom_polygon(data = world_map,  aes(x = long, y = lat, group = group), fill = NA,  colour = "gray50") +
        labs(title="Total Deaths by Country",
             y="", x="",
             subtitle = paste0("This plot shows the total number of COVID-19 
deaths in each country on ", input$Tab1_date),
             fill = "Deaths") +
        theme_classic()
    } else {
      world_map_new %>%   
        filter(date == input$Tab1_date) %>% 
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill= total_vaccinations_per_hundred*population/100), colour = "gray50") +
        scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +
        scale_y_continuous(breaks = seq(-60, 100, 30), labels = function(x){paste0(x, "°")}) +
        scale_fill_gradient(low = "lightblue", high="steel blue") +
        geom_polygon(data = world_map,  aes(x = long, y = lat, group = group), fill = NA,  colour = "gray50") +
        labs(title="Total Vaccinations by Country",
             y="", x="",
             subtitle = paste0("This plot shows the total number of COVID-19 
vaccinations in each country on ", input$Tab1_date),
             fill = "Vaccinations") +
        theme_classic()
    }
  })
  
 # Tab 1 Line Plot
  
  output$Tab2_line <- renderPlot({
    if(input$Tab1_var1 == "Global" & input$Tab1_var2 == "Cases") {
      total_data %>% 
        ggplot(aes(x = date, y = world_cases)) +
        geom_line() +
        geom_point(size = 0.05, color = "#FF8C00") +
        labs(x = "Date", y = "Cumulative", 
             title = "Total Number of Confirmed Cases Worldwide per Million") +
        theme_bw() 
    } else if(input$Tab1_var1 == "Global" & input$Tab1_var2 == "Deaths") {
      total_data %>% 
        ggplot(aes(x = date, y = world_deaths)) +
        geom_line() +
        geom_point(size = 0.05, color = "#FF8C00") +
        labs(x = "Date", y = "Cumulative", 
             title = "Total Number of Deaths Worldwide per Million") +
        theme_bw() 
    } else if(input$Tab1_var1 == "Continent" & input$Tab1_var2 == "Cases") {
      total_data %>% 
        ggplot(aes(x = date, y = continent_cases, color = continent)) +
        geom_line() +
        geom_point(size = 0.05) +
        labs(x = "Date", y = "Cumulative", 
             title = "Total Number of Confirmed Cases by Continent") +
        scale_color_economist() +
        theme_bw()
    } else if(input$Tab1_var1 == "Continent" & input$Tab1_var2 == "Deaths") {
      total_data %>% 
        ggplot(aes(x = date, y = continent_deaths, color = continent)) +
        geom_line() +
        geom_point(size = 0.05) +
        labs(x = "Date", y = "Cumulative", 
             title = "Total Number of Deaths by Continent") +
        scale_color_economist() +
        theme_bw()
    }
  })
  
  
  
  
  # Tab2 Cumulative
  output$Tab2_Cumulative <- renderPlot({
    if (input$Tab2_log == "Yes") {
      if (input$Tab2_type == "Cases") {
        total_data %>% 
          filter(country == input$Tab2_country) %>% 
          ggplot(aes(x = date, y = cases)) +
          geom_line(color = "#FF8C00", lwd = 1.1) +
          scale_y_log10() +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = paste0("Number of Cases in ", input$Tab2_country, " (per million)"))
          
      } else if (input$Tab2_type == "Deaths") {
        total_data %>% 
          filter(country == input$Tab2_country) %>% 
          ggplot(aes(x = date, y = deaths)) +
          geom_line(color = "#458B00", lwd = 1.1) +
          scale_y_log10() +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = paste0("Number of Deaths in ", input$Tab2_country, " (per million)"))
      } else if (input$Tab2_type == "Vaccinations") {
        total_data %>% 
          filter(country == input$Tab2_country) %>% 
          ggplot(aes(x = date, y = total_vaccinations_per_hundred)) +
          geom_line(color = "darkcyan", lwd = 1.1) +
          scale_y_log10() +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = paste0("Number of Vaccinations in ", input$Tab2_country, " (per hundred)"))
      }
    } else if (input$Tab2_log == "No") {
      if (input$Tab2_type == "Cases") {
        total_data %>% 
          filter(country == input$Tab2_country) %>% 
          ggplot(aes(x = date, y = cases)) +
          geom_line(color = "#FF8C00", lwd = 1.1) +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = paste0("Number of Cases in ", input$Tab2_country, " (per million)"))
        
      } else if (input$Tab2_type == "Deaths") {
        total_data %>% 
          filter(country == input$Tab2_country) %>% 
          ggplot(aes(x = date, y = deaths)) +
          geom_line(color = "#458B00", lwd = 1.1) +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = paste0("Number of Deaths in ", input$Tab2_country, " (per million)"))
      } else if (input$Tab2_type == "Vaccinations") {
        total_data %>% 
          filter(country == input$Tab2_country) %>% 
          ggplot(aes(x = date, y = total_vaccinations_per_hundred)) +
          geom_line(color = "darkcyan", lwd = 1.1) +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = paste0("Number of Vaccinations in ", input$Tab2_country, " (per hundred)"))
      }
    }
  })
  
  
  
  # Tab2 Histogram
  output$Tab2_hist <- renderPlot({
      if (input$Tab2_type2 == "Cases") {
        total_data %>% 
          filter(country == input$Tab2_country2) %>% 
          ggplot(aes(x = date, y = newcases)) +
          geom_col(color = "#FF8C00", width = input$Tab2_width, fill = "#FF8C00") +
          theme_bw() +
          labs(x = "Date",
               y = "Cases per Day",
               title = paste0("Number of Daily Cases in ", input$Tab2_country2, " (per million)"))
        
      } else if (input$Tab2_type2 == "Deaths") {
        total_data %>% 
          filter(country == input$Tab2_country2) %>% 
          ggplot(aes(x = date, y = newdeaths)) +
          geom_col(color = "#458B00", width = input$Tab2_width, fill = "#458B00") +
          theme_bw() +
          labs(x = "Date",
               y = "Deaths per Day",
               title = paste0("Number of Daily Deaths in ", input$Tab2_country2, " (per million)"))
      } 
  })
  
  
 # Tab3 Cumulative
  
  output$Tab3_Cumulative <- renderPlot({
    if (input$Tab3_log == "Yes") {
      if (input$Tab3_type == "Cases") {
        total_data %>% 
          filter(country %in% input$Tab3_country) %>% 
          ggplot(aes(x = date, y = cases, color = country)) +
          geom_line(lwd = 1.1) +
          scale_y_log10() +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = "Number of Cases (per million)")
        
      } else if (input$Tab3_type == "Deaths") {
        total_data %>% 
          filter(country %in% input$Tab3_country) %>% 
          ggplot(aes(x = date, y = deaths, color = country)) +
          geom_line(lwd = 1.1) +
          scale_y_log10() +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = "Number of Deaths (per million)")
      } else if (input$Tab3_type == "Vaccinations") {
        total_data %>% 
          filter(country %in% input$Tab3_country) %>% 
          ggplot(aes(x = date, y = total_vaccinations_per_hundred, color = country)) +
          geom_line(lwd = 1.1) +
          scale_y_log10() +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = "Number of Vaccinations (per hundred)")
      }
    } else if (input$Tab3_log == "No") {
      if (input$Tab3_type == "Cases") {
        total_data %>% 
          filter(country %in% input$Tab3_country) %>% 
          ggplot(aes(x = date, y = cases, color = country)) +
          geom_line(lwd = 1.1) +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = "Number of Cases (per million)")
        
      } else if (input$Tab3_type == "Deaths") {
        total_data %>% 
          filter(country %in% input$Tab3_country) %>% 
          ggplot(aes(x = date, y = deaths, color = country)) +
          geom_line(lwd = 1.1) +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = "Number of Deaths (per million)")
      } else if (input$Tab3_type == "Vaccinations") {
        total_data %>% 
          filter(country %in% input$Tab3_country) %>% 
          ggplot(aes(x = date, y = total_vaccinations_per_hundred, color = country)) +
          geom_line(lwd = 1.1) +
          theme_bw() +
          labs(x = "Date",
               y = "Cumulative",
               title = "Number of Vaccinations (per hundred)")
      }
    }
    
    
  })
  
  # Tab3 Histogram
  output$Tab3_hist <- renderPlot({
    if (input$Tab3_type2 == "Cases") {
        total_data %>% 
          filter(country %in% input$Tab3_country2) %>% 
          ggplot(aes(x = date, y = newcases, color = country, fill = country)) +
          geom_col(width = input$Tab3_width,
                   position = "stack", alpha = 0.5) +
          theme_bw() +
          labs(x = "Date",
               y = "Cases per Day",
               title = "Number of Daily Cases (per million)")
        
      } else if (input$Tab3_type2 == "Deaths") {
        total_data %>% 
          filter(country %in% input$Tab3_country2) %>% 
          ggplot(aes(x = date, y = newdeaths, color = country, fill = country)) +
          geom_col(width = input$Tab3_width,
                   position = "stack", alpha = 0.5) +
          theme_bw() +
          labs(x = "Date",
               y = "Deaths per Day",
               title = "Number of Daily Deaths (per million)")
      } 
  })
  
  
  
  
  # Tab4 Healthcare Plot1
  output$Tab4_plot1 <- renderPlot({
    totalHealth_GDP_03 %>% 
      filter(Entity %in% input$Tab4_country) %>% 
      filter(Year > 2003) %>% 
      ggplot(aes(x = Year, y = `Health expenditure, total (% of GDP)`, color = Entity)) +
      geom_line(lwd = 0.8) +
      geom_point() +
      theme_bw() +
      labs(x = "Year",
           y = "Health Expenditure",
           title = "Total Health Expenditure (% of GDP)")
  })
  
  
  # Tab4 Healthcare Plot2
  output$Tab4_plot2 <- renderPlot({
    public_totalHeath_02 %>% 
      filter(Entity %in% input$Tab4_country) %>% 
      filter(Year > 2003) %>% 
      ggplot(aes(x = Year, y = `Health expenditure, public (% of total health expenditure)`, color = Entity)) +
      geom_line(lwd = 0.8) +
      geom_point() +
      theme_bw() +
      labs(x = "Year",
           y = "Health Expenditure in Public",
           title = "Health Expenditure in Public (% of total health expenditure)")
  })
  
  
  
  # Tab4 Healthcare Plot3
  output$Tab4_plot3 <- renderPlot({
    HealthExpendPerCapita_01 %>% 
      filter(Entity %in% input$Tab4_country) %>% 
      filter(Year > 2003) %>% 
      ggplot(aes(x = Year, y = `Health expenditure per capita, PPP (constant 2011 international $)`, color = Entity)) +
      geom_line(lwd = 0.8) +
      geom_point() +
      theme_bw() +
      labs(x = "Year",
           y = "Health Expenditure per Capita",
           title = "Health Expenditure per Capita ($)")
  })
  
  
  
  # Tab4 Healthcare Table
  output$Tab4_table1 <- renderDataTable({
    if (input$Tab4_type == "Health Expenditure (% of GDP)") {
      totalHealth_GDP_03 %>% 
        filter(Year == input$Tab4_Year) %>% 
        select(-Year) %>% 
        rename(Country = Entity) %>% 
        filter(!is.na(Code)) %>% 
        arrange(desc(`Health expenditure, total (% of GDP)`)) %>% 
        mutate(Rank = row_number()) %>% 
        select(Rank, everything())
    } else if (input$Tab4_type == "Health Expenditure in Public") {
      public_totalHeath_02 %>% 
        filter(Year == input$Tab4_Year) %>% 
        select(-Year) %>% 
        rename(Country = Entity) %>% 
        filter(!is.na(Code)) %>% 
        arrange(desc(`Health expenditure, public (% of total health expenditure)`)) %>% 
        mutate(Rank = row_number()) %>% 
        select(Rank, everything())
    } else if (input$Tab4_type == "Health Expenditure per Capita") {
      HealthExpendPerCapita_01 %>% 
        filter(Year == input$Tab4_Year) %>% 
        select(-Year) %>% 
        rename(Country = Entity) %>% 
        filter(!is.na(Code)) %>% 
        arrange(desc(`Health expenditure per capita, PPP (constant 2011 international $)`)) %>% 
        rename(`Health expenditure per capita ($)` = `Health expenditure per capita, PPP (constant 2011 international $)`) %>% 
        mutate(Rank = row_number()) %>% 
        select(Rank, everything())
    }
})
  
  
  
  
  # Tab4 Poverty Plot
  
  output$Tab4_povertyPlot <- renderPlot({
    povertyHeadcount_04 %>% 
      filter(CountryName %in% input$Tab4_country2) %>% 
      filter(RequestYear > 2003) %>% 
      filter(CoverageType %in% c("N", "U")) %>% 
      ggplot(aes(x = RequestYear, y = HeadCount, color = CountryName)) +
      geom_line(lwd = 0.8) +
      scale_color_colorblind() +
      geom_point() +
      theme_bw() +
      labs(x = "Year",
           y = "Headcount",
           title = "Headcount from 2004 to 2019",
           subtitle = "Headcount: % of population living in households with consumption or income per person below the poverty line.")
  })
  
  
  
  # Tab4 Poverty Table
  
  output$Tab4_table2 <- renderDataTable({
    povertyHeadcount_04 %>% 
      filter(RequestYear == input$Tab4_Year2) %>% 
      filter(CoverageType %in% c("N", "U")) %>% 
      select(CountryName, CountryCode, HeadCount) %>% 
      rename(Country = CountryName) %>% 
      arrange(desc(HeadCount)) %>% 
      mutate(Rank = row_number()) %>% 
      select(Rank, everything())
    
  })
  
  
  
}

shinyApp(ui, server)
