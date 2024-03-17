# Load libraries
library(shiny)
library(tidyverse)

# Read in data
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Application Layout
shinyUI(
  fluidPage(
    br(),
    # Application title
    titlePanel("Exploring Demographic Information by Income Level"),
    
    # Description
    p("Explore the difference between people who earn less than $50K and more than $50K. 
      You can filter the data by country, then explore various demographic information."),
    
    # Select input for country
    fluidRow(
      column(12, 
             wellPanel(
               selectInput("country", "Select Country:", choices = unique(adult$native_country))
             )
      )
    ),
    
    # Select input for continuous variables and chart type
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a continuous variable and graph type to view on the right."),
               radioButtons("continuous_var", "Continuous Variable:", choices = c("age", "hours_per_week")),
               radioButtons("chart_type", "Chart Type:", choices = c("Histogram", "Boxplot"))
             )
      ),
      column(9, plotOutput("p1"))  # plot output
    ),
    
    # Select input for categorical variables
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a categorical variable to view a bar chart. Use the check box to view a stacked bar chart."),
               radioButtons("categorical_var", "Categorical Variable:", choices = c("education", "workclass", "sex")),
               checkboxInput("is_stacked", "Stacked Bar Chart", value = FALSE)
             )
      ),
      column(9, plotOutput("p2"))  # plot output
    )
  )
)
