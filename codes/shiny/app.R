library(shiny)
library("shinyWidgets")
library(ggplot2)

load("data/manual_real_fore_ras_corr_crop.RData")
source("R/setup.R")

ui <- fluidPage(
  titlePanel("Analiza prognoz klimatycznych o wysokiej rozdzielczości w kontekście możliwości wystąpienia suszy w wybranych regionach Polski"
             ),
  tabsetPanel(
              tabPanel("spei",
                      sidebarLayout(position = "left",
                                    sidebarPanel("",
                                                 sliderInput("year", "Year:", min=2007, max=2021, value=2007, step=1, sep=""),
                                                 radioButtons("timescale", "SPEI timescale:", choices = c("SPEI-1" = 1, "SPEI-3" = 3, "SPEI-6" = 6, "SPEI-12" = 12))
                                                ),
                                    mainPanel()
                      ),
                      fluidRow(),
                      fluidRow(
                              splitLayout(cellWidths = c("30%", "30%", "20%"), plotOutput("plot_real"), plotOutput("plot_forecast"), plotOutput("plot_legend"))
                              )
                              
                      ),
              tabPanel("correlation",
                       sidebarLayout(position = "left",
                                     sidebarPanel("",
                                                  radioButtons("timescale_c", "SPEI timescale:", choices = c("SPEI-1" = 1, "SPEI-3" = 3, "SPEI-6" = 6, "SPEI-12" = 12)),
                                                  radioButtons("corr_method_spei", "Correlation method:", choices = c("Pearson" = 1, "Spearman" = 2))
                                     ),
                                     mainPanel(
                                       plotOutput("plot_corr_spei")
                                     )
                       ),
                       sidebarLayout(position = "left",
                                     sidebarPanel(
                                                  sliderTextInput(
                                                    inputId = "month",
                                                    label = "Month:",
                                                    choices = month.name,
                                                    selected = month.name[1],
                                                    grid=TRUE,
                                                    hide_min_max = TRUE
                                                  ),
                                                  radioButtons("corr_method_other", "Correlation method:", choices = c("Pearson" = 1, "Spearman" = 2))
                                                  
                                     ),
                                     mainPanel()
                       ),
                       fluidRow(),
                       fluidRow(plotOutput("plot_tmax")),
                       fluidRow(plotOutput("plot_tmin")),
                       fluidRow(plotOutput("plot_ppt"))
                       )
              )
)
      
  
  
  
  
  
#  tabsetPanel(
#    tabPanel("spei",
#             fluidRow(
#               column(6,
#                      sliderInput("year", "Year:", min=2007, max=2021, value=2007, step=1, sep=""),
#               ),
#               column(6, 
#                      radioButtons("timescale", "SPEI timescale:", choices = c("SPEI-1" = 1, "SPEI-3" = 3, "SPEI-6" = 6, "SPEI-12" = 12)),
#               )
#             ),
#             fluidRow(
#               column(6, 
#                      plotOutput("plot_real"), 
#               ),
#               column(6, 
#                      plotOutput("plot_forecast")
#               )
#             )
#    ),
#    tabPanel("correlation", "contents")))
  

server <- function(input, output, session) {
  y <- reactive(input$year - 2006)
  tscale <- reactive(input$timescale)
  tscale_c <- reactive(input$timescale_c)
  c_method_spei <- reactive(as.numeric(input$corr_method_spei))
  c_method_other <- reactive(as.numeric(input$corr_method_other))
  m <- reactive(match(input$month, month.name))

  output$plot_real <- renderPlot({
    
    #par(mfrow=1:2, mar=c(0,0,3,0.5)+0.1, oma=c(0.5,0.5,0,2))
    plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Real")
    plot(spei_rasters_cropped[tscale(),1][[1]][[y()]], add=TRUE, legend=FALSE, col=pal(12), breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Real")
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55))
    
  }, res = 96)
  output$plot_forecast <- renderPlot({
    
    plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Forecast")
    plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], add=TRUE, legend=FALSE, col=pal(12), breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Forecast")
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55))
  
  }, res = 96)
  output$plot_legend <- renderPlot({
    
    plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], col=pal(12), breaks=cuts, legend.only=TRUE)
    
  }, res = 96)

  output$plot_corr_spei <- renderPlot({
    plot(correlation_spei[tscale_c(), c_method_spei()][[1]])
  }, res = 96)
  
  output$plot_tmax <- renderPlot({
    plot(correlation_tmax[m(), c_method_other()][[1]])
  }, res = 96)
  
  output$plot_tmin <- renderPlot({
    plot(correlation_tmin[m(), c_method_other()][[1]])
  }, res = 96)
  
  output$plot_ppt <- renderPlot({
    plot(correlation_ppt[m(), c_method_other()][[1]])
  }, res = 96)
}

shinyApp(ui, server)