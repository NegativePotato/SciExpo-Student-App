# Load packages

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
# library(here)
require(tidyr)

# Get the data

# file <- "https://github.com/rstudio-education/shiny-course/raw/main/movies.RData"
# destfile <- "movies.RData"
# 
# download.file(file, destfile)

extract_after_last_underscore <- function(x) {
  gsub(".*_(.+)$", "\\1", x)
}

# Load data
# data_dir <- file.path(here(), "East_High_Psych_Study", "Data")
# qualtrics_pre.df <- read.csv(file.path(data_dir, "Qualtrics_Pre_Clean.csv"))
# qualtrics_post.df <- read.csv(file.path(data_dir, "Qualtrics_Post_Clean.csv"))
qualtrics_pre.df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToj6FFtDV_hhK_gA8dB92UrYJs-vXa9PuEKr1oyzs4vjVUTCGmfZIXjvqAJTOVPQ/pub?gid=299623981&single=true&output=csv")
qualtrics_post.df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTvgpWiBSagN0Af1o5JhbJT58dCaRia-5LFWTeC6EhvISr0RHnjPiBEQWfjPzoTyg/pub?gid=550024776&single=true&output=csv")
sciexpo_study.df <- merge(qualtrics_pre.df, qualtrics_post.df) %>% 
  tidyr::drop_na() %>% 
  filter(SSI_Performance_Pre != 0) %>% 
  mutate(SSI_Performance_Pre = SSI_Performance_Pre/3,
         SSI_Competence_Pre = SSI_Competence_Pre/4,
         SSI_Recognition_Pre = SSI_Recognition_Pre/4,
         SSI_Interest_Pre = SSI_Interest_Pre/5,
         SSI_Performance_Post = SSI_Performance_Post/3,
         SSI_Competence_Post = SSI_Competence_Post/4,
         SSI_Recognition_Post = SSI_Recognition_Post/4,
         SSI_Interest_Post = SSI_Interest_Post/5) %>% 
  mutate(AP = ifelse(AP == "Yes", "In AP Class", "No AP Class"),
         Minority = ifelse(Minority == "Yes", "Minority", "Not Minority"),
         Child_Age_Under_18 = ifelse(Child_Age_Under_18 == "Yes", 
                                     "Under 18", "Older Than 18")) %>% 
  mutate(AP = 
           factor(AP, 
                  levels = c("In AP Class", "No AP Class")),
         Minority = 
           factor(Minority, 
                  levels = c("Minority", "Not Minority")),
         Child_Age_Under_18 = 
           factor(Child_Age_Under_18, 
                  levels = c("Under 18", "Older Than 18"))) %>% 
  tidyr::pivot_longer(cols = c(matches("_(Pre|Post)$")), 
                      names_to = "dv", 
                      values_to = "value") %>%
  rename(id = ResponseId, 
         age_category = Child_Age_Under_18, 
         minority_status = Minority, 
         ap_enrollment = AP, 
         current_grade = Grade, 
         student_id = Student_ID) %>% 
  mutate(prepost = extract_after_last_underscore(dv), 
         dv = gsub("_(Pre|Post)", "", dv)) %>% 
  mutate(prepost = factor(prepost, levels = c("Pre", "Post"))) %>% 
  arrange(student_id, prepost)

dv_list <- unique(sciexpo_study.df$dv)
vars_excluded <- c("id", "dv", "value", "student_id")
covariate_list <- sciexpo_study.df %>%
  select(-any_of(vars_excluded)) %>% 
  names()


# Define UI
## Input todo 
## - Font size for title
## - Font size for axis title
## - Font size for axis ticks
## - Name for x axis, y axis, and color legend
## - Custom colors for color legend
## - Add summary data table 
## - Add a tab with a graph for the difference between pre and post
## - Remove alpha slider 
## - Add "N" to the title (or as annotation)
## - Add download button for each plot and each data frame

ui <- page_sidebar(
  sidebar = sidebar(
    # Select variable for y-axis
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = dv_list
      # selected = "value"
    ),
    # Select variable for x-axis
    selectInput(
      inputId = "x",
      label = "X-axis:",
      choices = covariate_list
      # selected = "critics_score"
    ),
    # Select variable for color
    selectInput(
      inputId = "color",
      label = "Color by:",
      choices = covariate_list
      # selected = "mpaa_rating"
    ),
    # Select alpha level for dots
    sliderInput(
      inputId = "alpha",
      label = "Alpha:", 
      min = 0, 
      max = 1, 
      value = 0.66
    ),
    # Select size for dots
    sliderInput(
      inputId = "size",
      label = "Size:", 
      min = 0, 
      max = 5,
      value = 2
    ),
    checkboxInput(
      label = "Show data table", 
      inputId = "show_df",
      value = TRUE
    ),
    textInput(
      inputId = "plot_title",
      label = "Plot title:",
      value = "This is a title",
      placeholder = "Enter title here"
    ),
    # checkboxGroupInput(
    #   inputId = "movie_types",
    #   label = "Select movie type(s):",
    #   choiceNames = as.character(unique(movies$title_type)),
    #   choiceValues = as.character(unique(movies$title_type)),
    #   selected = as.character(unique(movies$title_type))[1]
    # ),
    numericInput(
      inputId = "sample_size",
      label = "Sample Size:", 
      value = 50,
      min = 1, 
      max = length(unique(sciexpo_study.df$student_id))
    )
  ),
  # Output: Show scatterplot
  card(plotOutput(outputId = "scatterplot"), 
       htmlOutput(outputId = "text_1"),
       min_height = "400px"), 
  card(dataTableOutput(outputId = "sciexpo_study.df"))
)

# Define server

server <- function(input, output, session) {
  sciexpo_study.react <- reactive({
      sciexpo_study.df %>%
        filter(dv == input$y) %>% 
        slice_head(n = input$sample_size)
    }
  ) 
  
  sciexpo_study_summary.react <- reactive({
    cols2group <- c(input$x, input$color)
    coucou <- sciexpo_study.react() 
    coucou <- coucou %>%
      group_by(across(all_of(cols2group))) %>% 
      dplyr::summarise(dv_mean = mean(value), 
                       dv_sd = ifelse(is.na(sd(value)), 0, sd(value)),
                       eb_min = dv_mean-dv_sd,
                       eb_max = dv_mean+dv_sd)
    return(coucou)
    }
  ) 
  
  
  output$scatterplot <- renderPlot({
    print(sciexpo_study.react())
    print(sciexpo_study_summary.react())
    
    col.width <- 0.5
    dodge.width <- 0.55
    
    ggplot() +
      geom_col(data = sciexpo_study_summary.react(),
               aes_string(x = input$x, y = "dv_mean",
                          fill = input$color),
               width = col.width,
               position = position_dodge(width = dodge.width)) +
      geom_errorbar(data = sciexpo_study_summary.react(),
                    aes_string(ymin = "eb_min", ymax = "eb_max",
                               x = input$x, group = input$color),
                    width = 0.2,
                    position = position_dodge(width = dodge.width)) +
      geom_point(data = sciexpo_study.react(), 
                 aes_string(x = input$x, 
                            y = "value",
                            group = input$color),
                 size = input$size, alpha = input$alpha, 
                 color = "black",
                 position = position_dodge(width = dodge.width)) + 
      ggtitle(input$plot_title)
  })
  
  output$sciexpo_study.df <- renderDataTable({
    sciexpo_study.react() %>% 
      select(id, input$x, input$color, value) %>%
      rename_with(
        ~ sprintf("%s", input$y),
        value
      )
      
  })

  output$text_1 <- renderUI({
    HTML(paste("There are coucou"))
  })
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)