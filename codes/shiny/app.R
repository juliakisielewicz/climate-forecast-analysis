library(shiny)
library("shinyWidgets")
#library(ggplot2)
library('RColorBrewer')
library(thematic)


load("data/manual_real_fore_ras_corr_crop.RData")
source("R/setup.R")


pal <- brewer.pal(6,'RdYlBu')
cuts<-seq(-3,3,1)

#cuts_c <- seq(-1, 1, by=0.2)
cuts_c <- c(-1, -0.5, -0.3, 0, 0.3, 0.5, 1)

#cuts_cp <- seq(0, 1, by=0.1)
#pal_c <- colorRampPalette(c("blue","white", "red"))

pal_c <- colorRampPalette(c("#d73027", "#ffffff", "#4575b4"))
#pal_c = brewer.pal(6, "RdBu")
pal_cp <- colorRampPalette(c("#d73027", "white"))






ui <- fluidPage(id = 'test',
                tags$style('#test {
                             background-color: #E9EDDE;
                             color: #662020;
              }'),
                
  titlePanel(HTML("<h1>Analiza prognoz klimatycznych o wysokiej rozdzielczości <br> w kontekście możliwości wystąpienia suszy <br>w wybranych regionach Polski</h1>")
             ),
  tabsetPanel(
              tabPanel("spei",
                      br(),
                      fluidRow(
                        
                        column(3, div(style = "height:150px; background-color: #d73027", radioButtons("timescale", "SPEI timescale:", choices = c("SPEI-1" = 1, "SPEI-3" = 3, "SPEI-6" = 6, "SPEI-12" = 12)))),
                        column(5, div(style = "height:150px; background-color: #d73027", sliderInput("year", "Year:", min=2007, max=2021, value=2007, step=1, sep="")))
                        
                        ),
                      
                      #sidebarLayout(position = "left",
                       #             sidebarPanel("",
 #                                                sliderInput("year", "Year:", min=2007, max=2021, value=2007, step=1, sep=""),
 #                                                radioButtons("timescale", "SPEI timescale:", choices = c("SPEI-1" = 1, "SPEI-3" = 3, "SPEI-6" = 6, "SPEI-12" = 12))
                        #                        ),
                         #           mainPanel()
                      #),
                      fluidRow(
                              column(5,div(style = "height:700px;", plotOutput("plot_real"))),
                              column(5,div(style = "height:700px;", plotOutput("plot_forecast"))),
                              column(2,div(style = "height:700px;", plotOutput("plot_legend")))
                              )
                      ),
                              
                      #),
              tabPanel("correlation",
                        br(),
                       sidebarLayout(position = "left",
                                     sidebarPanel("",
                                                  radioButtons("timescale_c", "SPEI timescale:", choices = c("SPEI-1" = 1, "SPEI-3" = 3, "SPEI-6" = 6, "SPEI-12" = 12)),
                                                  radioButtons("corr_method_spei", "Correlation method:", choices = c("Pearson" = 1, "Spearman" = 2))
                                     ),
                                     mainPanel()
                       ),
                       fluidRow(
                                column(6,div(style = "height:700px;", plotOutput("plot_corr_spei"))),
                                column(6,div(style = "height:700px;", plotOutput("plot_corr_p_spei"))),
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
                        fluidRow(
                                column(1,div(style = "height:1000px;")),
                                column(3,div(style = "height:1000px;", plotOutput("plot_corr_tmax"))),
                                column(3,div(style = "height:1000px;", plotOutput("plot_corr_tmin"))),
                                column(3,div(style = "height:1000px;", plotOutput("plot_corr_ppt"))),
                                column(2,div(style = "height:1000px;", 
                                    fluidRow(div(style = "height:500px;", plotOutput("plot_corr_legend"))),
                                    fluidRow(div(style = "height:500px;", plotOutput("plot_corr_p_legend")))
                                ))
                                )
                       )
              )
)
      
  
  
  

server <- function(input, output, session) {
  #thematic::thematic_shiny()
  
  y <- reactive(input$year - 2006)
  tscale <- reactive(input$timescale)
  tscale_c <- reactive(input$timescale_c)
  c_method_spei <- reactive(as.numeric(input$corr_method_spei))
  c_method_other <- reactive(as.numeric(input$corr_method_other))
  m <- reactive(match(input$month, month.name))

  
#---------SPEI-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

    #real
  output$plot_real <- renderPlot({
    plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main="Real")
    plot(spei_rasters_cropped[tscale(),1][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Real")
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
  }, res = 96, width = 800, height = 700)

    #forecast
  output$plot_forecast <- renderPlot({
    plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main="Forecast")
    plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Forecast")
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
  }, res = 96, width = 800, height = 700)

    #legend
  output$plot_legend <- renderPlot({  
    plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], col=pal, breaks=cuts, legend.only=TRUE)
  }, res = 96, width = 200, height = 700)

#---------CORRELATION-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

    #correlation SPEI
  output$plot_corr_spei <- renderPlot({
    #par(mfrow=1:2)
    plot(correlation_spei[tscale_c(), c_method_spei()][[1]][[1]], breaks = cuts_c, col=pal_c(6), main = paste0("SPEI-", tscale_c()))
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
  }, res = 96, width = 700, height = 700)
  

  output$plot_corr_p_spei <- renderPlot({
    plot(correlation_spei[tscale_c(), c_method_spei()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10))
    plot(mask(correlation_spei[tscale_c(), c_method_spei()][[1]][[2]], correlation_spei[tscale_c(), c_method_spei()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
  }, res = 96, width = 700, height = 700)

    #correlation others

    #tmax
  output$plot_corr_tmax <- renderPlot({
    par(mfrow=c(2,1))

    plot(correlation_tmax[m(), c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), main = "Maximum temperature", legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")

    plot(correlation_tmax[m(), c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend=FALSE)
    plot(mask(correlation_tmax[m(), c_method_other()][[1]][[2]], correlation_tmax[m(), c_method_other()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
  }, res = 0.96, width = 500, height = 1000)

    #tmin
  output$plot_corr_tmin <- renderPlot({
    par(mfrow=c(2,1))

    plot(correlation_tmin[m(), c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), main = "Minimum temperature", legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")

    plot(correlation_tmin[m(), c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend=FALSE)
    plot(mask(correlation_tmin[m(), c_method_other()][[1]][[2]], correlation_tmin[m(), c_method_other()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
  }, res = 0.96, width = 500, height = 1000)

    #ppt
  output$plot_corr_ppt <- renderPlot({
    par(mfrow=c(2,1))

    plot(correlation_ppt[m(), c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), main = "Precipitation", legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")

    plot(correlation_ppt[m(), c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend=FALSE)
    plot(mask(correlation_ppt[m(), c_method_other()][[1]][[2]], correlation_ppt[m(), c_method_other()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
  }, res = 0.96, width = 500, height = 1000)

    #legend
  output$plot_corr_legend <- renderPlot({
    plot(correlation_ppt[m(), c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6),legend.only=TRUE)
  }, res = 0.96, width = 200, height = 500)

    #legend p-value
  output$plot_corr_p_legend <- renderPlot({
    plot(correlation_ppt[m(), c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend.only=TRUE)
  }, res = 0.96, width = 200, height = 500)
}


#"Correlation of real and forecasted maximum temperature for years 2006 - 2021"

shinyApp(ui, server)