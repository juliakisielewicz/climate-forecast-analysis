library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)

#---------DATA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/manual_real_fore_ras_corr_crop.RData")
source("R/setup_final.R")


pal <- brewer.pal(6,'RdYlBu')
cuts<-seq(-3,3,1)

#cuts_c <- seq(-1, 1, by=0.2)
cuts_c <- c(-1, -0.5, -0.3, 0, 0.3, 0.5, 1)

#cuts_cp <- seq(0, 1, by=0.1)
#pal_c <- colorRampPalette(c("blue","white", "red"))

pal_c <- colorRampPalette(c("#d73027", "#ffffff", "#4575b4"))
#pal_c = brewer.pal(6, "RdBu")
pal_cp <- colorRampPalette(c("#d73027", "white"))


#---------FRONTEND-----------------------------------------------------------------------------------------------------------------------------------------------------------------------


header <- dashboardHeader(
  
  tags$li(
    class = "dropdown",
   # tags$style(".main-header {font-size: 100px}"),
    tags$style(".main-header .logo {height: auto}"),
  ),
  title = div(
    span(HTML("Analiza prognoz klimatycznych o wysokiej rozdzielczości
                                  <br> w kontekście możliwości wystąpienia suszy w wybranych regionach Polski")),
    align = "left",
    width = "100%",
    style = "padding-right:0px; font-size: 26px; line-height: 36px; margin: 1%;"
  ),
  titleWidth = "100%"
)


body <- dashboardBody(id = "id_body",
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(tags$style(HTML('
      #id_main {
        margin: 20px;
      }
      #info {
        font-size: 18px;
        margin-top: calc(230px - 10vw);
      }
      .class_row {
        margin-top: 20px;
      }
      
      #settings {
      padding: 30px;
      }
      .skin-blue .main-header .logo {
          background-color: #d73027;
        }
      .skin-blue .main-header .logo:hover {
        background-color: #d73027;
      }
        
      .skin-blue .main-header .navbar {
        visibility: hidden;      }
      
      .skin-blue .main-header {
      /*margin-bottom: 0.6vh;*/
      }
                            '
  ))),
  fluidRow(id = "id_main",
    tabsetPanel(id = "info",
                selected = "Indeks SPEI",
                tabPanel(id = "spei_tab", title = "Indeks SPEI", 
                         
                         fluidRow(class = "class_row", 
                           box(id = "settings",
                             title = NULL, 
                             width = 3,
                             solidHeader=TRUE,
                             radioButtons("timescale",
                                          "Skala czasowa indeksu SPEI:",
                                          choices = c("1 miesiąc" = 1, "3 miesiące" = 3, "6 miesięcy" = 6, "12 miesięcy" = 12)
                                          ),
                             sliderInput("year",
                                         "Rok:",
                                         min=2007, max=2021, value=2007, step=1, sep="",
                                         width = '100%')
                           ),
                           box(title = NULL,
                               width = 9,
                               solidHeader=TRUE,
                               height = 700,
                               fluidRow(id = "spei_maps",
                                        height = 2000,
                                        column(12,div(plotOutput("plot_spei")))
                                        )

                           )
                         ),

                         ),
                tabPanel(id = "corr_tab", title = "Korelacja danych prognostycznych i rzeczywistych", 
                         
                         fluidRow(class = "class_row",
                                  box( id = "settings",
                                    title = NULL, 
                                    width = 2,
                                    height = 470,
                                    solidHeader=TRUE,
                                    radioButtons("timescale_c",
                                                 "Skala czasowa indeksu SPEI:",
                                                 choices = c("1 miesiąc" = 1, "3 miesiące" = 3, "6 miesięcy" = 6, "12 miesięcy" = 12)),
                                    radioButtons("corr_method_spei",
                                                 "Metoda korelacji:",
                                                 choices = c("Pearsona" = 1, "Spearmana" = 2))
                                  ),   
                                  box(
                                      width = 10,
                                      height = 470,
                                      solidHeader=TRUE,
                                      fluidRow(id = "spei_corr_maps",
                                               height = 1500,
                                               column(12,div(plotOutput("plot_spei_corr")))
                                      )
                                  )
                                  ),
                         fluidRow(
                           column(width = 3,
                                  box(id = "settings",
                                    title = NULL, 
                                    width = NULL, height = 820,
                                    solidHeader=TRUE,
                                    {
                                    customTicks <- seq(1,12, by=1)
                                    customSlider <- sliderInput(
                                      inputId = "month",
                                      label = "Miesiąc:",
                                      min = 1,
                                      max = 12,
                                      value = 1,
                                      step = 1,
                                      ticks = TRUE
                                    )
                                    #tagQuery(customSlider)$find("input")$addAttrs("data-values" = paste0(customTicks, collapse = ", "))$allTags()
                                    },
                                    radioButtons("corr_method_other", 
                                                 "Metoda korelacji:", 
                                                 choices = c("Pearsona" = 1, "Spearmana" = 2)),
                                    textOutput("test")
                                  )
                           ),
                           
                           column(width = 9,
                                  box(
                                    title = NULL, 
                                    width = NULL, height = 400,
                                    solidHeader=TRUE,
                                    div(plotOutput("plot_corr"))                                  ),
                                  box(
                                    title = NULL, 
                                    width = NULL, height = 400,
                                    solidHeader=TRUE,
                                    div(plotOutput("plot_corr_p"))                                  )
                           )
                           
                          
                         )
                         
 
                         )
                )
  )
)




ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  #sidebar,
  body
)



#---------BACKEND-----------------------------------------------------------------------------------------------------------------------------------------------------------------------


server <- function(input, output) { 
  
  
  y <- reactive(input$year - 2006)
  tscale <- reactive(input$timescale)
  tscale_c <- reactive(input$timescale_c)
  c_method_spei <- reactive(as.numeric(input$corr_method_spei))
  c_method_other <- reactive(as.numeric(input$corr_method_other))

  output$test <- renderText(input$month)
  #---------SPEI-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

    output$plot_spei <- renderPlot({  
      
      par(oma = c(4,1,1,1), mfrow = c(1, 2), mar = c(2, 2, 1, 1))
        plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main="Dane rzeczywiste")
            plot(spei_rasters_cropped[tscale(),1][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Dane rzeczywiste")
            plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
        plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main="Dane prognostyczne")
            plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Dane prognostyczne")
            plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
        
          
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
        plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], col=pal, axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, breaks=cuts)
       
    }, res = 96, height = 600)
 
  
  
  #---------CORRELATION-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

  output$plot_spei_corr <- renderPlot({  
    
    par(oma = c(5,1,1,0), mfrow = c(1, 2), mar = c(0, 1, 3, 2))
      plot(correlation_spei[tscale_c(), c_method_spei()][[1]][[1]], breaks = cuts_c, col=pal_c(6), main = paste0("SPEI-", tscale_c()), horizontal = TRUE, legend =FALSE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
      
      plot(correlation_spei[tscale_c(), c_method_spei()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), horizontal = TRUE, legend = FALSE, main = paste0("p-value (SPEI-", tscale_c(), ")"))
      plot(mask(correlation_spei[tscale_c(), c_method_spei()][[1]][[2]], correlation_spei[tscale_c(), c_method_spei()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    
      
      par(fig = c(0, 0.5, 0.1, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(correlation_spei[tscale_c(), c_method_spei()][[1]][[1]], breaks = cuts_c, col=pal_c(6), horizontal = TRUE, legend.only = TRUE)

      par(fig = c(0.5, 1, 0.1, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(correlation_spei[tscale_c(), c_method_spei()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), horizontal = TRUE, legend.only = TRUE)
      
      
      
      
  }, res = 96, height = 450)
  
  
          #---------CORRELATION OTHERS-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #correlation
  
  output$plot_corr <- renderPlot({  
    
    par(oma = c(10,1,1,0), mfrow = c(1, 3), mar = c(0, 1, 3, 0))
    
    plot(correlation_tmax[input$month, c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), main = "Temperatura maksymalna", legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    plot(correlation_tmin[input$month, c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), main = "Temperatura minimalna", legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    plot(correlation_ppt[input$month, c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), main = "Opady", legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
            plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
            plot(correlation_tmax[input$month, c_method_other()][[1]][[1]], col=pal_c(6), axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, breaks=cuts_c, legend.shrink = 0.3, legend.width = 0.4)
           
    
  }, res = 96, height = 380)
  
  
  #p-value
  
  output$plot_corr_p <- renderPlot({  
    
    par(oma = c(10,1,1,0), mfrow = c(1, 3), mar = c(0, 1, 3, 0))
    
    plot(correlation_tmax[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend=FALSE, main = "p-value (temperatura maksymalna)")
    plot(mask(correlation_tmax[input$month, c_method_other()][[1]][[2]], correlation_tmax[input$month, c_method_other()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    
    
    plot(correlation_tmin[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend=FALSE, main = "p-value (temperatura minimalna)")
    plot(mask(correlation_tmin[input$month, c_method_other()][[1]][[2]], correlation_tmin[input$month, c_method_other()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    
    plot(correlation_ppt[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend=FALSE, main = "p-value (opady)")
    plot(mask(correlation_ppt[input$month, c_method_other()][[1]][[2]], correlation_ppt[input$month, c_method_other()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
    plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
    plot(correlation_tmax[input$month, c_method_other()][[1]][[2]], col=pal_cp(10), axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, breaks=cuts_cp, legend.shrink = 0.3, legend.width = 0.4)
    
    
  }, res = 96, height = 380)
  
  
  
}

shinyApp(ui, server)

