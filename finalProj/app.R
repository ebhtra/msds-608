#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(forcats)
library(ggplot2)
library(dplyr)
library(ggalluvial)

# Load all data at the start

baseloc = 'https://raw.githubusercontent.com/ebhtra/msds-608/main/finalProj/csvs/'
longnats = read.csv(paste0(baseloc, 'longnats.csv'))
longlprs = read.csv(paste0(baseloc, 'longlprs.csv'))

# grid-type layout, where columns exist within rows

ui <- fluidPage(

    fluidRow( style = 'padding-bottom:0px',
      
      column(1, offset = 3, style = 'padding-bottom:0px',
             selectInput('YR1', NULL, as.character(2005:2021), selected='2005')),
      
      column(2, style = 'padding-bottom:0px',
             selectInput('CAT1', NULL, c('Green Cards', 'New Citizens'), selected='Green Cards')),
      
      column(2, radioButtons("radio", NULL, inline = T,
                             choices = list("States Inside" = 1, "States Outside" = 2), 
                             selected = 1)),
      
      column(1, style = 'padding-bottom:0px',
             selectInput('YR2', NULL, as.character(2005:2021), selected='2011')),
      
      column(2, style = 'padding-bottom:0px',
             selectInput('CAT2', NULL, c('Green Cards', 'New Citizens'), selected='New Citizens'))
      ),
      
    fluidRow(
      
      column(2, offset = 0, div(style = 'margin-top:3em'), 
             h2('2005-2021 New U.S. Green Cards and Citizenship'), 
             br(),
             p('When immigrants to the U.S. receive their Green Cards and become 
             Legal Permanent Residents, the next step is naturalization 
             (citizenship), which could take any number of years, 
             if it happens at all.'),
             br(),
             p('The charts to the right help you explore where immigrants were born 
             and where they lived when they became LPRs and citizens, from 2005-2021.'),
             br(),
             p('By selecting different years, events, and chart orientations, 
              you can compare how proportions are changing over time, in terms of
               where people come from and which states they go to.'),
             br(),
             p('Data come from the Department of Homeland Security,
               at dhs.gov/immigration-statistics'),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             p('by Ethan Haley')
             ),
            
            
      
      column(5, offset = 0, div(style = 'margin-top:-2em', plotOutput('leftPlot'))),
      
      column(5, offset = 0, div(style = 'margin-top:-2em', plotOutput('rightPlot')))
      
    )
)
# factor out as much code as possible, since there are 16 possible param combos
myplot = function(DF, ycol, ax1, ax2){
  ggplot(DF, 
         aes(y = ycol, axis1 = ax1, axis2 = ax2)) +
        geom_alluvium(aes(fill = X), width = .1) +
        geom_stratum(width = .1, aes(fill = X)) +
        geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
        theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank())
}

server <- function(input, output) {

    output$leftPlot <- renderPlot({
        yr = input$YR1
        cat = input$CAT1
        flip = input$radio
        
        frame = longlprs %>% filter(year == yr)
        ycol = frame$LPRs
        ax1 = frame$X
        ax2 = frame$LPRin
        
        if (cat == 'New Citizens'){
          frame = longnats %>% filter(year == yr)
          ycol = frame$NATZs
          ax2 = frame$NATZin
        }
        
        if (flip == 2){
          temp = ax1
          ax1 = ax2
          ax2 = temp
        }
        
    myplot(frame, ycol, ax1, ax2)  # call plot func
      
    }, height = 900)
    
    output$rightPlot <- renderPlot({
      
      yr = input$YR2
      cat = input$CAT2
      flip = input$radio
     
      frame = longnats %>% filter(year == yr)
      ycol = frame$NATZs
      ax1 = frame$NATZin
      ax2 = frame$X
      
      if (cat == 'Green Cards'){
        frame = longlprs %>% filter(year == yr)
        ycol = frame$LPRs
        ax1 = frame$LPRin
      }
      
      if (flip == 2){
        temp = ax1
        ax1 = ax2
        ax2 = temp
      }
      
      myplot(frame, ycol, ax1, ax2)
      
    }, height = 900)
}

# Run the application 
shinyApp(ui = ui, server = server)
