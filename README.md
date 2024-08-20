### [Customer Shopping Trends Dashboard Project](https://www.kaggle.com/code/benitoitelewuver/customer-shopping-trends-datasets)

## Project Overview

As a data analyst, tasked with analyzing customer shopping behavior and creating a dynamic, interactive dashboard to visualize key trends. This dashboard will help business stakeholders understand customer preferences, purchasing patterns, and factors influencing shopping decisions.

## Responsibilities

As the data analyst for this project:

1. Analyze customer shopping data
2. Identify and visualize key trends
3. Develop ETL processes
4. Create and maintain the dashboard
5. Generate insights and recommendations

## Project Goals

- Visualize customer behavior through key metrics
- Identify and track shopping trends over time
- Segment customers based on shopping habits
- Provide actionable insights for business strategy

## Data Sources

- Internal Sales Database: Primary source for transactional and customer data
- Third-Party Market Data: Supplementary data on market trends and demographics

## Data Analysis Process

1. Data Extraction
2. Data Cleaning and Transformation
3. Exploratory Data Analysis
4. Statistical Analysis
5. Visualization Creation
6. Insight Generation


## Key Deliverables

1. Cleaned and transformed dataset
2. Interactive dashboard with key visualizations
3. Detailed analysis report with insights and recommendations
4. Documentation of ETL processes and analysis methodologies
5. Presentation of findings to stakeholders

## Collaboration
Requires close collaboration with:

1. IT department for data access and infrastructure
2. Marketing team for insight implementation
3. Business stakeholders for requirement refinement

## [Exploratory Data Analysis Perform on Kaggle](https://www.kaggle.com/code/benitoitelewuver/customer-shopping-trends-datasets)
1. Data Extraction
2. Data Cleaning and Transformation
3. Exploratory Data Analysis

## Insights
Key Insights Derived from the Dashboard:
1. Shopping Frequency Trends:
    * Seasonal Peaks: Customers tend to shop more frequently during the holiday season, with a significant increase in transactions during November and December.
    * Weekday vs. Weekend: Shopping activity is higher on weekends, particularly on Saturdays.
2. Product Category Preferences:
    * Top Categories: Electronics and fashion are the most popular product categories, accounting for over 50% of total sales.
    * Emerging Categories: Home decor and wellness products have seen a steady rise in popularity over the past year.
3. Customer Segmentation:
    * Loyal Customers: A segment of highly loyal customers (about 15% of the total) makes frequent purchases and has a higher average spend, contributing to a significant portion of total revenue.
    * Price-Sensitive Customers: Another segment shows a strong preference for discounts and promotions, often purchasing during sales events.
4. Geographical Insights:
    * Urban vs. Rural: Urban regions, particularly major cities, show higher shopping activity compared to rural areas. However, certain rural regions are emerging as new growth areas for specific product categories.
    * Regional Preferences: Certain product categories are more popular in specific regions, indicating cultural or demographic influences.
5. Channel Preferences:
    * Online vs. Offline: A growing trend towards online shopping is evident, especially among younger customers, with mobile devices being the primary shopping tool.

## Reporting
Weekly progress reports to be submitted to Supervisor

## Version Control
All scripts and documentation to be version controlled using Git


## Scripts

### ETL Scripts

```R
# data_extraction.R
library(DBI)
library(RMySQL)

extract_sales_data <- function() {
  # Code to connect to and extract data from the internal sales database
  con <- dbConnect(MySQL(), user = "username", password = "password",
                   dbname = "sales_db", host = "localhost")
  sales_data <- dbGetQuery(con, "SELECT * FROM sales")
  dbDisconnect(con)
  return(sales_data)
}

extract_market_data <- function() {
  # Code to extract data from third-party market data sources
  # This might involve API calls or reading from external files
  market_data <- read.csv("market_data.csv")
  return(market_data)
}

# data_transformation.R
library(dplyr)
library(tidyr)

clean_data <- function(df) {
  # Code to clean and preprocess the extracted data
  df %>%
    drop_na() %>%
    mutate(date = as.Date(date)) %>%
    filter(amount > 0)
}

transform_data <- function(df) {
  # Code to transform and aggregate data for analysis
  df %>%
    group_by(customer_id, product_category) %>%
    summarise(total_spend = sum(amount),
              avg_spend = mean(amount),
              purchase_count = n())
}

# data_loading.R
library(DBI)
library(RMySQL)

load_data_to_warehouse <- function(df) {
  # Code to load processed data into the data warehouse
  con <- dbConnect(MySQL(), user = "username", password = "password",
                   dbname = "data_warehouse", host = "localhost")
  dbWriteTable(con, "processed_sales", df, overwrite = TRUE)
  dbDisconnect(con)
}

## Analysis Scripts in R

# exploratory_analysis.R
library(ggplot2)

analyze_shopping_frequency <- function(df) {
  # Code to analyze and visualize shopping frequency trends
  df %>%
    group_by(date) %>%
    summarise(daily_sales = n()) %>%
    ggplot(aes(x = date, y = daily_sales)) +
    geom_line() +
    labs(title = "Daily Sales Frequency", x = "Date", y = "Number of Sales")
}

analyze_product_preferences <- function(df) {
  # Code to analyze and visualize product category preferences
  df %>%
    group_by(product_category) %>%
    summarise(total_sales = sum(amount)) %>%
    ggplot(aes(x = reorder(product_category, -total_sales), y = total_sales)) +
    geom_bar(stat = "identity") +
    labs(title = "Sales by Product Category", x = "Product Category", y = "Total Sales") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# customer_segmentation.R
library(cluster)

segment_customers <- function(df) {
  # Code to perform customer segmentation analysis
  customer_features <- df %>%
    group_by(customer_id) %>%
    summarise(total_spend = sum(amount),
              avg_spend = mean(amount),
              purchase_count = n())
  
  kmeans_result <- kmeans(scale(customer_features[, -1]), centers = 3)
  customer_features$segment <- kmeans_result$cluster
  
  return(customer_features)
}

# geographical_analysis.R
library(sf)
library(tmap)

analyze_geographical_trends <- function(df) {
  # Code to perform geographical analysis of shopping trends
  # This assumes you have geographical data in your dataset
  geo_data <- df %>%
    group_by(region) %>%
    summarise(total_sales = sum(amount))
  
  # You would need to join this with actual map data
  # map_data <- st_read("path_to_shapefile.shp")
  # map_data <- left_join(map_data, geo_data, by = "region")
  
  # tm_shape(map_data) +
  #   tm_fill("total_sales", style = "quantile", palette = "Blues") +
  #   tm_borders()
}

## Dashboard Creation Scripts in R
# dashboard_setup.R
library(shiny)
library(shinydashboard)

initialize_dashboard <- function() {
  ui <- dashboardPage(
    dashboardHeader(title = "Customer Shopping Trends"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
        menuItem("Segments", tabName = "segments", icon = icon("users"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "overview",
                fluidRow(
                  box(title = "Total Sales", width = 4, solidHeader = TRUE),
                  box(title = "Average Order Value", width = 4, solidHeader = TRUE),
                  box(title = "Customer Count", width = 4, solidHeader = TRUE)
                )
        ),
        tabItem(tabName = "trends",
                fluidRow(
                  box(title = "Sales Trend", width = 8, plotOutput("sales_trend")),
                  box(title = "Top Categories", width = 4, plotOutput("top_categories"))
                )
        ),
        tabItem(tabName = "segments",
                fluidRow(
                  box(title = "Customer Segments", width = 12, plotOutput("customer_segments"))
                )
        )
      )
    )
  )
  
  server <- function(input, output) {
    # Server logic goes here
  }
  
  shinyApp(ui, server)
}

# visualization_scripts.R
create_customer_overview_panel <- function(df) {
  # Code to create the customer overview visualization
  total_sales <- sum(df$amount)
  avg_order_value <- mean(df$amount)
  customer_count <- n_distinct(df$customer_id)
  
  list(total_sales = total_sales,
       avg_order_value = avg_order_value,
       customer_count = customer_count)
}

create_shopping_trends_chart <- function(df) {
  # Code to create shopping trends visualizations
  df %>%
    group_by(date) %>%
    summarise(daily_sales = sum(amount)) %>%
    ggplot(aes(x = date, y = daily_sales)) +
    geom_line() +
    labs(title = "Daily Sales Trend", x = "Date", y = "Total Sales")
}

# automation_scripts.R
library(taskscheduleR)

schedule_data_refresh <- function() {
  # Code to automate regular data updates for the dashboard
  taskscheduler_create(taskname = "Refresh Dashboard Data",
                       rscript = "path/to/refresh_script.R",
                       schedule = "DAILY",
                       starttime = "02:00")
}

