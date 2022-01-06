#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(png)
library(shinydashboard)
library(shinythemes)
library(tidymodels)
library(ggtext)

ui <-
  navbarPage(
    h4(style = "font-size:18px", "Punt Return Decision Model"),
    tags$style(type = "text/css", "body {padding-top: 0px;}"),
    tags$head(tags$style(".shiny-plot-output{width:90vw !important;}")),
    tags$head(tags$style(".shiny-plot-output{height:65vh !important;}")),
    
    theme = shinytheme("cosmo"),
    # #shinythemes::themeSelector(),
    #
    #
    #   # Application title
    #   titlePanel("Punt Return Decision Model"),
    
    tabPanel(
      title = h3(style = "font-size:12px", "Decision Plot"),
      
      # Show a plot of the generated distribution
      mainPanel(
        width = 12,
        fluidRow(
          #hr(style = "border-top: 1px solid #000000;"),
          column(
            offset = 0,
            width = 1,
            actionButton("go", "Generate Plot", style='padding:4px; font-size:100%'), 
            tags$head(tags$style(".shiny-plot-output{width:90vw !important;}")),
            tags$head(tags$style(".shiny-plot-output{height:65vh !important;}"))
          ),
          column(
            width = 3,
            offset = 0,
            sliderInput(
              "score_difference",
              "Score Differential (How much are you up or down by?)",
              min = -30,
              max = 30,
              value = 0,
            ),
            tags$head(tags$style(".shiny-plot-output{width:90vw !important;}")),
            tags$head(tags$style(".shiny-plot-output{height:65vh !important;}"))
          ),
          column(
            offset = 0,
            width = 1,
            selectInput(
              inputId = "quarter",
              label = "Quarter",
              choices = c(1, 2, 3, 4),
              selected = 3
            ), 
            tags$head(tags$style(".shiny-plot-output{width:90vw !important;}")),
            tags$head(tags$style(".shiny-plot-output{height:65vh !important;}"))
          ),
          column(
            width = 3,
            offset = 0,
            sliderInput(
              "minutes_remaining",
              "Minutes Remaining in Quarter",
              min = 1,
              max = 15,
              value = 10
            ), 
            tags$head(tags$style(".shiny-plot-output{width:90vw !important;}")),
            tags$head(tags$style(".shiny-plot-output{height:65vh !important;}"))
          ),
          column(
            offset = 0,
            width = 3,
            sliderInput(
              "scrimmage_line",
              "Line of Scrimmage
                        (Where are they punting from?)",
              min = 10,
              max = 50,
              value = 40
            ), 
            tags$head(tags$style(".shiny-plot-output{width:90vw !important;}")),
            tags$head(tags$style(".shiny-plot-output{height:65vh !important;}"))
          ),
          
          
        ),
        #hr(style = "border-top: 1px solid #000000;"),
        
        plotOutput("distPlot", width = 800, height = 400) %>% withSpinner(color =
                                                                             "#0dc5c1"),
        #dataTableOutput("table")
        #img(src='returner_choices.png', width = 800)
      )
    ),
    tabPanel(title = h3(style = "font-size:12px", "Observed Returns"), 
             img(src='plot3.png', width = 1000)),
    tabPanel(title = h3(style = "font-size:12px", "Punt Returner Performance"), 
             img(src = "playerbar_plot.png", width = 900)),
    tabPanel(title = h3(style = "font-size:12px", "Team Performance"), 
             # column(width = 3,
             #   offset = 0,
             #   img(src = "teambar_plot.png", width = 700)
             # ), 
             # column(width = 3,
             #        offset = 1,
                    img(src = "quadrant_plot.png", width = 1100)
             # )
                    
    )
             
  )




server <- function(input, output) {
  # maybe change defender distance values
  # Read in Model
  model1 = readRDS("epa_model.rds")
  #model1 = readRDS("C:\\Users\\nateh\\Documents\\NFL Special Teams\\appdata\\lm_model.rds")
  #model1 <- readRDS("~\\Ultimate\\Big Data Bowl 2022\\NFL-Big-Data-Bowl-2022\\lm_model.rds")
  yardline_grid <- seq(1,20, by=1) ## make a yardline vector
  def_dist_grid <- seq(5,35, by=1) ## make a distance vector
  event_grid <- c("punt_land", "fair_catch", "punt_received")
  #score_diff_grid = (input$score_difference)
  #seconds_grid = 180
  #kicklength_grid = 60
  #quarter_grid = 2
  pred_grid <- reactive({
    expand.grid(yardLine = yardline_grid, mean_defender_distance = def_dist_grid, event.y = event_grid,
                score_difference = input$score_difference,
                # kickLength.x = input$scrimmage_line,
                quarter.x = input$quarter)
  })
  pred_grid1 <- reactive( {
    pred_grid() %>%
      mutate(kickLength.x = 100 - (yardLine + input$scrimmage_line)) %>%
      mutate(half_seconds_remaining =
               case_when(
                 (quarter.x == 4 | quarter.x == 2) ~ 60*input$minutes_remaining,
                 (quarter.x == 3 | quarter.x == 1) ~ 60*input$minutes_remaining + 60*15
               )
      )
  })
  # Make Predictions
  #matrix.preds = reactive(predict(model, pred_grid()))
  # Bind them together
  preds_data <- reactive({
    augment(model1,
            new_data = pred_grid1(),
            interval = "prediction")
  })
  # make yardline bins
  preds_data_bin <- reactive({
    preds_data() %>% mutate(yardlinebin =
                              case_when(
                                yardLine < 2 ~ 1,
                                yardLine < 4 ~ 2,
                                yardLine < 6 ~ 3,
                                yardLine < 8 ~ 4,
                                yardLine < 10 ~ 5,
                                yardLine < 12 ~ 6,
                                yardLine < 14 ~ 7,
                                yardLine < 16 ~ 8,
                                yardLine < 18 ~ 9,
                                yardLine < 21 ~ 10
                              ))
  })
  # Make defender distances bins
  preds_data_bin1 <- reactive({
    preds_data_bin() %>% mutate(def_distance_bin =
                                  case_when(
                                    mean_defender_distance < 15 ~ 0,
                                    mean_defender_distance < 20 ~ 1,
                                    mean_defender_distance < 25 ~ 2,
                                    mean_defender_distance < 30 ~ 3,
                                    mean_defender_distance < 64 ~ 4
                                  ))
  })
  epa_tbl <- reactive({
    preds_data_bin1() %>% group_by(yardlinebin, def_distance_bin, event.y) %>%
      mutate(median_epa = mean(-.pred)) %>%
      arrange(median_epa, .by_group = TRUE)
  })
  # Find which one is best in each case
  epa_tbl_only = reactive({
    epa_tbl() %>% dplyr::select(median_epa, yardlinebin, def_distance_bin, event.y)
  })
  epa_tbl_only1 <- reactive({
    unique(epa_tbl_only())
  })
  result_table_epa <- reactive({
    epa_tbl_only1()  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, median_epa)
  })
  # Get second highest in each group
  second = reactive({
    epa_tbl_only1()  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(2, median_epa) %>% arrange(-median_epa, .by_group = TRUE) %>%
      dplyr::slice(2) %>%
      dplyr::select(yardlinebin, def_distance_bin, median_epa)
  })
  second1 = reactive({
    second() %>% rename(median_2 = median_epa)
  })
  result_table_epa1 = reactive({
    result_table_epa() %>% left_join(second1())
  })
  result_table_epa2 <- eventReactive(input$go, {
    result_table_epa1() %>%
      mutate(difference = median_epa - median_2) %>%
      mutate(event.y = case_when(
        event.y == "fair_catch" ~ "Fair Catch",
        event.y == "punt_land" ~ "Let Bounce",
        event.y == "punt_received" ~ "Return Punt"
      ))
  })
  output$table <- DT::renderDataTable({
    result_table_epa2()
  })
  
  
  output$distPlot <- renderPlot({
    group.colors <- c(`Return Punt` = "#333BFF", `Let Bounce` = "#11D879", `Fair Catch` = "891322")
    
    ggplot(
      data = result_table_epa2(),
      mapping = aes(
        x = yardlinebin,
        y = def_distance_bin,
        fill = event.y,
        alpha = difference
      )
    ) +
      geom_tile(color = "white") +
      labs(
        title = "What returners *should* be doing in this situation",
        subtitle = "Gradient represents how much better the optimal choice is than the next best choice",
        x = "Yardline \n (binned every 2 yards)",
        y = "Closest Defender Distance\n(2 seconds until ball lands)",
        fill = NULL,
        caption = "Plot: Nate Hawkins & Jacob Miller \n Data: NFL Big Data Bowl & nflfastr"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = c(0.5, 3, 5.5, 8, 10.5),
                         labels = c(0, 5, 10, 15, 20)) +
      scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                         labels = c("< 15", "15 - 20", "20 - 25", "25 - 30", "> 30")) +
      theme(panel.grid = element_blank(),
            legend.position = "top",
            legend.text = element_text(size = 20),
            axis.text = element_text(size = 14,
                                     face = "bold"),
            axis.title = element_text(size = 16,
                                      face = "bold"),
            plot.subtitle = element_text(size = 18),
            plot.title = element_markdown(size = 25,
                                          face = "bold")) +
      scale_fill_manual(values = group.colors) +
      guides(alpha = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
