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
# Define UI for application that draws a histogram
ui <- fluidPage(


    # Application title
    titlePanel("Punt Return Decision Model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("score_difference",
                        "Score Differential:",
                        min = -30,
                        max = 30,
                        value = 0),
            selectInput(inputId = "quarter",
                        label = "Quarter", 
                        choices = c(1,2,3,4),
                        selected = 1), 
            sliderInput("kicklength",
                        "Punt Distance",
                        min = 30,
                        max = 70,
                        value = 40)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           dataTableOutput("table")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Read in Model
  model = readRDS("C:\\Users\\nateh\\Documents\\NFL Special Teams\\appdata\\epa_model.rds")
  yardline_grid <- seq(1,20, by=1) ## make a yardline vector
  def_dist_grid <- seq(1,60, by=1) ## make a distance vector
  event_grid <- c("punt_land", "fair_catch", "punt_received")
  #score_diff_grid = (input$score_difference)
  seconds_grid = 180
  #kicklength_grid = 60
  #quarter_grid = 2
  
  pred_grid <- reactive({
    expand.grid(yardLine = yardline_grid, mean_defender_distance = def_dist_grid, event.y = event_grid, 
                           score_difference = input$score_difference, half_seconds_remaining = seconds_grid,
                           kickLength.x = input$kicklength, quarter.x = input$quarter)
  })

  # Make Predictions
  #matrix.preds = reactive(predict(model, pred_grid()))
  
  # Bind them together
  preds_data = reactive({
    as.data.frame(cbind(pred_grid(), "predictions" = predict(model, pred_grid())))
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
                                              yardLine < 14~ 7,
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
    mutate(median_epa = mean(-predictions)) %>% 
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
    slice(2) %>% 
    dplyr::select(yardlinebin, def_distance_bin, median_epa)
  })
  
  second1 = reactive({
    second() %>% rename(median_2 = median_epa)
  })

   result_table_epa1 = reactive({
    result_table_epa() %>% left_join(second1())
  })
 
   result_table_epa2 = reactive({
    result_table_epa1() %>% mutate(difference = median_epa - median_2)
  })
   
   output$table <- DT::renderDataTable({
     epa_tbl_only1()
   })
  
  
    output$distPlot <- renderPlot({
        
      ggplot(data = result_table_epa2(), mapping = aes(x = yardlinebin, y = def_distance_bin, 
                                                                                   fill = event.y, 
                                                                                   alpha = difference)) + 
        geom_raster() + labs(title = "What should returners be doing based on EPA",
                       subtitle = "Difference between first and second best choice",
                       x = "Yardline binned every 2 yards",
                       y = "Closest Defender Distance binned at quantiles")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
