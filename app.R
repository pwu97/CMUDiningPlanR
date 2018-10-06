#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(lattice)
library(ggplot2)
library(zoo)

ui <- shiny::shinyUI(shiny::fluidPage(
  
  tags$head(includeHTML(("google-analytics.html"))),
  
  shiny::titlePanel('CMU Dining Plan Progress'),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::dateInput(inputId = "start_date", label = "Current Date:", value = Sys.Date(), 
                             min = NULL, max = NULL, format = "yyyy-mm-dd",
                             startview = "month", weekstart = 0, language = "en"),

      shiny::dateInput(inputId = "end_date", label = "End Date:", value = "2018-12-16",
                            min = NULL, max = NULL, format = "yyyy-mm-dd",
                            startview = "month", weekstart = 0, language = "en"),

      shiny::selectInput('plan', "Meal Plan:", choices = c('Red', 'Blue', 'Green',
                                                                 'Yellow', 'Tartan Flex',
                                                                 'Scotty\'s Choice',
                                                                 'Whitfield\'s Favor',
                                                                 'Piper Select')),

      shiny::numericInput(inputId = "dollars", label = "Flex Dollars Remaining:"
                          ,value = 600, min = 0, max = 800, step =1),

      shiny::numericInput(inputId = "blocks", label = "Meal Blocks Remaining:",
                                value = 150, min = 0, max = 292, step =1)

    ),
    
    # Show a plot of the generated distribution
    shiny::mainPanel(
      shiny::p(
        'This Shiny app is designed to track your progress on your current
        CMU dining plan. To access information about your dining expenditures, go to 
        CMU SIO and click on Campus Life > Meal Plan Assignment. To the left, enter your 
        dining plan, flex dollars remaining, and meal blocks remaining to see if
        you\'re on track! Notice also that you can change the end date for when
        you want to use up your flex dollars and meal blocks by.'
      ),
      
      shiny::tabsetPanel(
        shiny::tabPanel('Flex Dollars Progress',
                        shiny::plotOutput("flexPlot", height = 350),
                        shiny::textOutput("selected_var")
        ),
        shiny::tabPanel('Meal Blocks Progress',
                        shiny::plotOutput("blocksPlot", height = 350),
                        shiny::textOutput("blockstext")
        )
        )
      
      
      
      # shiny::tabsetPanel(
      #   shiny::tabPanel('Empty Bracket',
      #                   shiny::plotOutput("flex.dollars.plot", height = 600)
      #   ),
      #   shiny::tabPanel('Filled Bracket',
      #                   shiny::plotOutput("meal.blocks.plot", height = 600)
      #   ),
      #   shiny::tabPanel('Test Results',
      #                   shiny::plotOutput("bracket.empty.plot", height = 600)
      #   )
      # )
      
    )
  )
))

# Define UI for application that draws a histogram
# ui <- fluidPage(
#   
#   tags$script('
#           $(document).ready(function(){
#               var d = new Date();
#               var target = $("#clientTime");
#               target.val(d.toLocaleString());
#               target.trigger("change");
#               });
#               '),
#    
#    # Application title
#    titlePanel("CMU Dining Plan Progress"),
#   
#    dateInput(inputId = "start_date", label = "Current Date:", value = NULL, 
#              min = NULL, max = NULL, format = "yyyy-mm-dd", 
#              startview = "month", weekstart = 0, language = "en"),
#    
#    dateInput(inputId = "end_date", label = "End Date:", value = "2018-12-16", 
#             min = NULL, max = NULL, format = "yyyy-mm-dd", 
#             startview = "month", weekstart = 0, language = "en"),
#   
#    selectInput('plan', "Meal Plan:", choices = c('Green', 'Blue', 'Red',
#                                                  'Yellow', 'Tartan Flex',
#                                                  'Scotty\'s Choice', 
#                                                  'Whitfield\'s Favor', 
#                                                  'Piper Select')),
#    
#    textInput(inputId = "dollars", label = "Flex Dollars Remaining:"),
#    
#    numericInput(inputId = "blocks", label = "Meal Blocks Remaining:",
#                 value = 100, min = 0, max = 292, step =),
#    
#    actionButton('btn.optimal', 'Calculate Progress',
#                class = "btn btn-primary"),
#   
#   mainPanel(
#     p("Hi")
#   )
# 
# )

server <- function(input, output) {
  
  #'Green', 'Blue', 'Red',
  #                                                  'Yellow', 'Tartan Flex',
  #                                                  'Scotty\'s Choice', 
  #                                                  'Whitfield\'s Favor', 
  #                                                  'Piper Select'
  
  
  
  # input$plan <- renderPrint({
  # if (x == 'Green')
  #   startingmoney <- 250
  # else if(x == 'Blue')
  #   startingmoney <- 490
  # else if(input$plan == 'Red')
  #   startingmoney <- 800
  # else if(input$plan == 'Yellow')
  #   startingmoney <- 180
  # else if(input$plan == 'Tartan Flex')
  #   startingmoney <- 830
  # else if(input$plan == 'Scotty\'s Choice')
  #   startingmoney <- 595
  # else if(input$plan == 'Whitfield\'s Favor')
  #   startingmoney <- 450
  # else if(input$plan == 'Piper Select')
  #   startingmoney <- 320
  # 
  # if (input$plan == 'Green')
  #   startingblocks <- 292
  # else if(input$plan == 'Blue')
  #   startingblocks <- 252
  # else if(input$plan == 'Red')
  #   startingblocks <- 205
  # else if(input$plan == 'Yellow')
  #   startingblocks <- 125
  # else if(input$plan == 'Tartan Flex')
  #   startingblocks <- 170
  # else if(input$plan == 'Scotty\'s Choice')
  #   startingblocks <- 85
  # else if(input$plan == 'Whitfield\'s Favor')
  #   startingblocks <- 54
  # else if(input$plan == 'Piper Select')
  #   startingblocks <- 32
  # 
  # })
  
  output$flexPlot = shiny::renderPlot({
    
    if (input$plan == 'Green')
      startingmoney <- 250
    else if(input$plan == 'Blue')
      startingmoney <- 490
    else if(input$plan == 'Red')
      startingmoney <- 800
    else if(input$plan == 'Yellow')
      startingmoney <- 180
    else if(input$plan == 'Tartan Flex')
      startingmoney <- 830
    else if(input$plan == 'Scotty\'s Choice')
      startingmoney <- 595
    else if(input$plan == 'Whitfield\'s Favor')
      startingmoney <- 450
    else if(input$plan == 'Piper Select')
      startingmoney <- 320

    if (input$plan == 'Green')
      startingblocks <- 292
    else if(input$plan == 'Blue')
      startingblocks <- 252
    else if(input$plan == 'Red')
      startingblocks <- 205
    else if(input$plan == 'Yellow')
      startingblocks <- 125
    else if(input$plan == 'Tartan Flex')
      startingblocks <- 170
    else if(input$plan == 'Scotty\'s Choice')
      startingblocks <- 85
    else if(input$plan == 'Whitfield\'s Favor')
      startingblocks <- 54
    else if(input$plan == 'Piper Select')
      startingblocks <- 32
    
    dates <- c(as.Date("2018-08-27"), input$start_date, input$end_date);

    print(as.numeric(input$start_date))
    print(dates)
    dollars <- c(startingmoney, input$dollars, 0);
    blocks <- c(292, input$blocks, 0);
    df <- data.frame(dates, dollars)
    
    print(df)
    dfplot <- ggplot(data=df, aes(x=dates, y=dollars)) +  
      geom_point(size = 4, alpha = 0.6) + 
      theme_bw() +
      labs(x = "Time",
           y = "Flex Dollars Remaining",
          color = "Line Info") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 18),
          legend.position = "right") +
      coord_cartesian(xlim=c(as.Date("2018-08-27"), input$end_date))
    
    
    c <- data.frame(x1 = as.Date("2018-08-27"), x2 = input$start_date,
                     y1 = startingmoney, y2 = input$dollars)
    
    # print(as.numeric(as.Date("2018-08-27")))
    # print(as.numeric(input$start_date))
    # 
    y2 <- input$dollars
    y1 <- startingmoney
    x1 <- as.Date("2018-08-27")
    x2 <- input$start_date
    
    
    
    print(as.numeric(x1))
    x1 <- as.numeric(x1)
    print(as.numeric(x2))
    x2 <- as.numeric(x2)
    print(y1)
    print(y2)
    
    slope <- ((y2-y1)/(x2-x1))
    print(slope)
    print((0-y1)/slope+x1)
    
    
    y <- dfplot + 
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/slope+x1), yend = 0, color = "Projected"), data = c,linetype=2) +
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-1+x1), yend = 0, color = "$1/day"), data = c,linetype=2) +
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-2+x1), yend = 0, color = "$2/day"), data = c,linetype=2) +
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-3+x1), yend = 0, color = "$3/day"), data = c,linetype=2) +
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-4+x1), yend = 0, color = "$4/day"), data = c,linetype=2) +
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-5+x1), yend = 0, color = "$5/day"), data = c,linetype=2) +
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-6+x1), yend = 0, color = "$6/day"), data = c,linetype=2) +
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-7+x1), yend = 0, color = "$7/day"), data = c,linetype=2) +
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-8+x1), yend = 0, color = "$8/day"), data = c,linetype=2) +
      geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-9+x1), yend = 0, color = "$9/day"), data = c,linetype=2) +

      
      geom_segment(aes(x = x2, y = y2, xend = input$end_date, yend = 0, color = "Ideal"), data = c,linetype=2) +      
      ggtitle("Flex Dollars Progress")
    
    print(y)



    
  })
  
  output$selected_var <- renderText({
    if (input$plan == 'Green')
      startingmoney <- 250
    else if(input$plan == 'Blue')
      startingmoney <- 490
    else if(input$plan == 'Red')
      startingmoney <- 800
    else if(input$plan == 'Yellow')
      startingmoney <- 180
    else if(input$plan == 'Tartan Flex')
      startingmoney <- 830
    else if(input$plan == 'Scotty\'s Choice')
      startingmoney <- 595
    else if(input$plan == 'Whitfield\'s Favor')
      startingmoney <- 450
    else if(input$plan == 'Piper Select')
      startingmoney <- 320
    
    if (input$plan == 'Green')
      startingblocks <- 292
    else if(input$plan == 'Blue')
      startingblocks <- 252
    else if(input$plan == 'Red')
      startingblocks <- 205
    else if(input$plan == 'Yellow')
      startingblocks <- 125
    else if(input$plan == 'Tartan Flex')
      startingblocks <- 170
    else if(input$plan == 'Scotty\'s Choice')
      startingblocks <- 85
    else if(input$plan == 'Whitfield\'s Favor')
      startingblocks <- 54
    else if(input$plan == 'Piper Select')
      startingblocks <- 32
    
    c <- data.frame(x1 = as.Date("2018-08-27"), x2 = input$start_date,
                    y1 = startingmoney, y2 = input$dollars)
    
    # print(as.numeric(as.Date("2018-08-27")))
    # print(as.numeric(input$start_date))
    # 
    y2 <- input$dollars
    y1 <- startingmoney
    x1 <- as.Date("2018-08-27")
    x2 <- input$start_date
    x3 <- input$end_date
    
    print(as.numeric(x1))
    x1 <- as.numeric(x1)
    print(as.numeric(x2))
    x2 <- as.numeric(x2)
    print(y1)
    print(y2)
    
    slope <- ((y2-y1)/(x2-x1))
    print(slope)
    print((0-y1)/slope+x1)
    
    print(as.Date((0-y1)/slope+x1))
  
    
    if((0-y1)/slope+x1 < as.numeric(x3))
    {
      text <- "hi"
      paste("Good work! You are currently spending your flex dollars at the pace of $",
            round(-slope, digits = 2),"/day. If you continue
            at your current pace, you are projected to use up all your flex
            dollars by ", as.character(as.Date((0-y1)/slope+x1)), ".", 
            sep="", collapse=NULL)
    }
    else
    {
      text <- "hi"
      paste("Spend more! You are currently spending your flex dollars at the pace of $",
            round(-slope, digits = 2),"/day. If you continue
            at your current pace, you are projected to have about ", 
            round(slope*(as.numeric(input$end_date)-x1)+y1, digits=0),
            " flex dollars at the end of the semester.", sep="")
    }
      
  })
  
  output$blockstext <- renderText({
    if (input$plan == 'Green')
      startingmoney <- 250
    else if(input$plan == 'Blue')
      startingmoney <- 490
    else if(input$plan == 'Red')
      startingmoney <- 800
    else if(input$plan == 'Yellow')
      startingmoney <- 180
    else if(input$plan == 'Tartan Flex')
      startingmoney <- 830
    else if(input$plan == 'Scotty\'s Choice')
      startingmoney <- 595
    else if(input$plan == 'Whitfield\'s Favor')
      startingmoney <- 450
    else if(input$plan == 'Piper Select')
      startingmoney <- 320
    
    if (input$plan == 'Green')
      startingblocks <- 292
    else if(input$plan == 'Blue')
      startingblocks <- 252
    else if(input$plan == 'Red')
      startingblocks <- 205
    else if(input$plan == 'Yellow')
      startingblocks <- 125
    else if(input$plan == 'Tartan Flex')
      startingblocks <- 170
    else if(input$plan == 'Scotty\'s Choice')
      startingblocks <- 85
    else if(input$plan == 'Whitfield\'s Favor')
      startingblocks <- 54
    else if(input$plan == 'Piper Select')
      startingblocks <- 32
    
    c <- data.frame(x1 = as.Date("2018-08-27"), x2 = input$start_date,
                    y1 = startingblocks, y2 = input$blocks)
    
    # print(as.numeric(as.Date("2018-08-27")))
    # print(as.numeric(input$start_date))
    # 
    y2 <- input$blocks
    y1 <- startingblocks
    x1 <- as.Date("2018-08-27")
    x2 <- input$start_date
    x3 <- input$end_date
    
    print(as.numeric(x1))
    x1 <- as.numeric(x1)
    print(as.numeric(x2))
    x2 <- as.numeric(x2)
    print(y1)
    print(y2)
    
    slope <- ((y2-y1)/(x2-x1))
    print(slope)
    print((0-y1)/slope+x1)
    
    print(as.Date((0-y1)/slope+x1))
    
    
    if((0-y1)/slope+x1 < as.numeric(x3))
    {
      text <- "hi"
      paste("Good work! You are currently spending your meal blocks at the pace of ",
            round(-slope, digits = 2),"/day. If you continue
            at your current pace, you are projected to use up all your meal
            blocks by ", as.character(as.Date((0-y1)/slope+x1)), ".", 
            sep="", collapse=NULL)
    }
    else
    {
      text <- "hi"
      paste("Spend more! You are currently spending your meal blocks at the pace of ",
            round(-slope, digits = 2),"/day. If you continue
            at your current pace, you are projected to have about ", 
            round(slope*(as.numeric(input$end_date)-x1)+y1, digits=0),
            " meal blocks at the end of the semester.", sep="")
    }
    
  })
  
  
  ###blocks
  
  output$blocksPlot = shiny::renderPlot({
    
    if (input$plan == 'Green')
      startingmoney <- 250
    else if(input$plan == 'Blue')
      startingmoney <- 490
    else if(input$plan == 'Red')
      startingmoney <- 800
    else if(input$plan == 'Yellow')
      startingmoney <- 180
    else if(input$plan == 'Tartan Flex')
      startingmoney <- 830
    else if(input$plan == 'Scotty\'s Choice')
      startingmoney <- 595
    else if(input$plan == 'Whitfield\'s Favor')
      startingmoney <- 450
    else if(input$plan == 'Piper Select')
      startingmoney <- 320
    
    if (input$plan == 'Green')
      startingblocks <- 292
    else if(input$plan == 'Blue')
      startingblocks <- 252
    else if(input$plan == 'Red')
      startingblocks <- 205
    else if(input$plan == 'Yellow')
      startingblocks <- 125
    else if(input$plan == 'Tartan Flex')
      startingblocks <- 170
    else if(input$plan == 'Scotty\'s Choice')
      startingblocks <- 85
    else if(input$plan == 'Whitfield\'s Favor')
      startingblocks <- 54
    else if(input$plan == 'Piper Select')
      startingblocks <- 32
    
    dates <- c(as.Date("2018-08-27"), input$start_date, input$end_date);
    
    print(as.numeric(input$start_date))
    print(dates)
    dollars <- c(startingmoney, input$dollars, 0);
    blocks <- c(startingblocks, input$blocks, 0);
    df <- data.frame(dates, blocks)
    
    print(df)
    
  dfplot <- ggplot(data=df, aes(x=dates, y=blocks)) +  
    geom_point(size = 4, alpha = 0.6) + 
    theme_bw() +
    labs(x = "Time",
         y = "Block Meals Remaining",
         color = "Line Info") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 18),
          legend.position = "right") +
    coord_cartesian(xlim=c(as.Date("2018-08-27"), input$end_date))
  
  c <- data.frame(x1 = as.Date("2018-08-27"), x2 = input$start_date,
                  y1 = startingblocks, y2 = input$blocks)
  
  y2 <- input$blocks
  y1 <- startingblocks
  x1 <- as.Date("2018-08-27")
  x2 <- input$start_date
  
  
  
  print(as.numeric(x1))
  x1 <- as.numeric(x1)
  print(as.numeric(x2))
  x2 <- as.numeric(x2)
  print(y1)
  print(y2)
  
  slope <- ((y2-y1)/(x2-x1))
  print(slope)
  print((0-y1)/slope+x1)
  
  
  y <- dfplot + 
    geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/slope+x1), yend = 0, color = "Projected"), data = c,linetype=2) +
    geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-1+x1), yend = 0, color = "1 block/day"), data = c,linetype=2) +
    geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-2+x1), yend = 0, color = "2 blocks/day"), data = c,linetype=2) +
    geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-3+x1), yend = 0, color = "3 blocks/day"), data = c,linetype=2) +
    geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-4+x1), yend = 0, color = "4 blocks/day"), data = c,linetype=2) +
    geom_segment(aes(x = x2, y = y2, xend = as.Date((0-y1)/-5+x1), yend = 0, color = "5 blocks/day"), data = c,linetype=2) +
    geom_segment(aes(x = x2, y = y2, xend = input$end_date, yend = 0, color = "Ideal"), data = c,linetype=2) +   
    ggtitle("Meal Blocks Progress")
  
  print(y)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)


