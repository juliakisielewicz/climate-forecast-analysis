library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
library(sf)
library(RColorBrewer)
library(raster)
#---------DATA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#load("data/manual_real_fore_ras_corr_crop.RData")
#source("R/setup_final.R")
load("data/manual_data.RData")
#save(list=c("spei_rasters_cropped", "correlation_spei", "correlation_tmax", "correlation_tmin", "correlation_ppt", "pol", "both"), file = "data/manual_data.RData")
pal <- brewer.pal(6,'RdYlBu')
cuts<-seq(-3,3,1)

#cuts_c <- seq(-1, 1, by=0.2)
cuts_c <- c(-1, -0.5, -0.3, 0, 0.3, 0.5, 1)

cuts_cp <- c(0.0, 0.05, seq(0.1, 1, by=0.1))
#pal_c <- colorRampPalette(c("blue","white", "red"))
cuts_cp_l <- c(0.0, 0.049, 0.09999, 0.19999, 0.29999, 0.399999, 0.499999, 0.59999999999999, 0.6999999, 0.7999999, 0.9, 1.0)


pal_c <- colorRampPalette(c("#4575b4", "#ffffff", "#d73027"))
#pal_c = brewer.pal(6, "RdBu")
pal_cp <- colorRampPalette(c("#d73027", "white"))
mixed_cp <- c("#f2b103", "#fee090", pal_cp(9))

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
    style = "padding-right:0px; font-size: 22px; line-height: 28px; margin: 1%;"
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
        font-size: 16px;
        margin-top: calc(300px - 10vw);
      }
      .class_row {
        margin-top: 20px;
      }
      
      #settings {
        padding: 30px;
      }
      .skin-blue .main-header .logo {
          background-color: #d99f03;
        }
      .skin-blue .main-header .logo:hover {
        background-color: #3e69a2;
      }
        
      .skin-blue .main-header .navbar {
        visibility: hidden;      }
  '))),
  fluidRow(id = "id_main",
    tabsetPanel(id = "info",
                selected = "Indeks SPEI",
                tabPanel(id = "spei_tab", title = "Indeks SPEI",
                         fluidRow(class = "class_row",
                                  box(id = "settings",
                                      title=NULL,
                                      width=12,
                                      solidHeader=TRUE, 
                                      h5(
                                                 HTML("Indeks SPEI determinuje wystąpienie suszy. Obliczany jest na podstawie miesięcznej sumy opadów atmosferycznych, 
                                                 średniej miesięcznej temperatury minimalnej i średniej miesięcznej temperatury maksymalnej. 
                                                 Dane klimatyczne pochodzić mogą z 1, 3, 6 lub 12 miesięcy poprzedzających. 
                                                 Wartości ujemne oznaczają warunki suche, zaś wartości dodatnie - wilgotne. <br><br>
                                                 Poniższe wykresy pozwalają porównać wartości indeksu SPEI dla lipca w poszczególnych latach od 2007 do 2021 roku, uzyskane na podstawie dwóch różnych zestawów danych. 
                                                  Zaprezentowana jest również różnica pod względem podatności na suszę pomiędzy dwoma wybranymi obszarami Polski."))
                                      )
                                  ),
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
                               height = 600,
                               fluidRow(id = "spei_maps",
                                        height = 2000,
                                        column(12,div(plotOutput("plot_spei")))
                                        )
                           )
                         ),

                         ),
                tabPanel(id = "corr_tab", title = "Porównanie danych prognostycznych i rzeczywistych", 
                         fluidRow(class = "class_row",
                                  box(id = "settings",
                                      title=NULL,
                                      width=12,
                                      solidHeader=TRUE, 
                                      h5(
                                        HTML("W celu prześledzenia zgodności prognozy z rzeczywistymi warunkami klimatycznymi okresu 2006 - 2021, 
                                             skorelowano ze sobą oba zestawy danych. 
                                             Korelację wykonano zarówno dla samego indeksu SPEI, jak i dla poszczególnych zmiennych wykorzystanych przy jego obliczaniu - 
                                             miesięcznej sumy opadów atmosferycznych, średniej miesięcznej temperatury minimalnej i średniej miesięcznej temperatury maksymalnej. 
                                             Uwzględniono dwie metody korelacji - Pearsona i Spearmana. <br><br>
                                             Poniższe wykresy prezentują wartość współczynnika korelacji między dwoma zestawami danych oraz wartość p-value, wyznaczającą istotność statystyczną korelacji w danej lokalizacji.
                                             Ze względu na dominujący brak istotności statystycznej w obrębie analizowanych obszarów, poziom korelacji zbadano również dla otoczenia tych obszarów, mieszczącego się w granicach całej Polski."))
                                  )
                         ),
                         fluidRow(class = "class_row",
                                  box( id = "settings",
                                    title = NULL, 
                                    width = 2,
                                    height = 570,
                                    solidHeader=TRUE,
                                    radioButtons("timescale_c",
                                                 "Skala czasowa indeksu SPEI:",
                                                 choices = c("1 miesiąc" = 1, "3 miesiące" = 3, "6 miesięcy" = 6, "12 miesięcy" = 12)),
                                    radioButtons("corr_method_spei",
                                                 "Metoda korelacji:",
                                                 choices = c("Pearsona" = 1, "Spearmana" = 2)),
                                    radioButtons("is_ss_spei",
                                                 "Zamaskuj obszary bez istotności statystycznej:",
                                                 choices = c("Brak maski" = 1, "przy założeniu p-value < 0.05" = 2, "przy założeniu p-value < 0.1" = 3))
                                  ),   
                                  box(
                                      width = 10,
                                      height = 570,
                                      solidHeader=TRUE,
                                      fluidRow(id = "spei_corr_maps",
                                               height = 1500,
                                               column(12,div(plotOutput("plot_spei_corr")))
                                      )
                                  )
                                  ),
                         fluidRow(
                         #  height = 750,
                           column(width = 3,
                                  box(id = "settings",
                                    title = NULL, 
                                    width = NULL, height = 750,
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
                                    radioButtons("is_ss_other",
                                                 "Zamaskuj obszary bez istotności statystycznej:",
                                                 choices = c("Brak maski" = 1, "przy założeniu p-value < 0.05" = 2, "przy założeniu p-value < 0.1" = 3))
                                  )
                           ),
                           
                           column(width = 9,
                                  box(
                                    title = NULL, 
                                    width = NULL, height = 365,
                                    solidHeader=TRUE,
                                    div(plotOutput("plot_corr"))                                  ),
                                  box(
                                    title = NULL, 
                                    width = NULL, height = 365,
                                    solidHeader=TRUE,
                                    div(plotOutput("plot_corr_p"))                                  )
                           )
                           
                          
                         )
                         
 
                         )
                )
  )
)




ui <- dashboardPage(
  title="Analiza prognoz klimatycznych",
  header,
  dashboardSidebar(disable = TRUE),
  body
)



#---------BACKEND-----------------------------------------------------------------------------------------------------------------------------------------------------------------------


server <- function(input, output) { 
  
  y <- reactive(input$year - 2006)
  tscale <- reactive(input$timescale)
  tscale_c <- reactive(input$timescale_c)
  c_method_spei <- reactive(as.numeric(input$corr_method_spei))
  c_method_other <- reactive(as.numeric(input$corr_method_other))

  #---------SPEI-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

    output$plot_spei <- renderPlot({  
      
      par(oma = c(2,1,1,1), mfrow = c(1, 2), mar = c(1, 2, 2, 1))
        
        plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main="Dane rzeczywiste")
        plot(spei_rasters_cropped[tscale(),1][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Dane rzeczywiste")
        plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    
        plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main="Dane prognostyczne")
        plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Dane prognostyczne")
        plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    
          
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
        plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], col=pal, axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, breaks=cuts,legend.width = 0.4)
       
    }, res = 96, height = 550)
 
  
  
  #---------CORRELATION-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

  output$plot_spei_corr <- renderPlot({  
    
    par(oma = c(5,1,1,0), mfrow = c(1, 2), mar = c(0, 1, 3, 2))
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main = paste0("SPEI-", tscale_c()))
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], breaks = cuts_c, col=pal_c(6), horizontal = TRUE, legend =FALSE, add=TRUE)
      
      if(input$is_ss_spei == 2) {
        plot(mask(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]] >= 0.05, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      else if (input$is_ss_spei == 3) {
        plot(mask(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]] >= 0.1, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
      
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main = paste0("p-value (SPEI-", tscale_c(), ")"))
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]], breaks = cuts_cp, col=mixed_cp, horizontal = TRUE, legend = FALSE, add=TRUE)
      #plot(mask(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]], correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
    
  
      par(fig = c(0, 0.5, 0.1, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], breaks = cuts_c, col=pal_c(6), horizontal = TRUE, legend.only = TRUE, legend.shrink = 0.7, legend.width=0.6)

      par(fig = c(0.5, 1, 0.1, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]], breaks = cuts_cp_l, axis.args = list(at = cuts_cp_l[1:(length(cuts_cp_l))], labels=c('0.0', '0.05','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9', '1.0')), col=mixed_cp, horizontal = TRUE, legend.only = TRUE, legend.shrink = 0.7, legend.width=0.6)
      
      
  }, res = 96, height = 550)
  
  
          #---------CORRELATION OTHERS-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #correlation
  
  output$plot_corr <- renderPlot({  
    
    par(oma = c(4,1,1,0), mfrow = c(1, 3), mar = c(0, 1, 2, 0))
    
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main = "Temperatura maksymalna")
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), legend=FALSE, add=TRUE)
      if(input$is_ss_other == 2) {
        plot(mask(correlation_tmax_cropped[input$month, c_method_other()][[1]][[1]], correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]] >= 0.05, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      else if (input$is_ss_other ==3){
        plot(mask(correlation_tmax_cropped[input$month, c_method_other()][[1]][[1]], correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]] >= 0.1, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main = "Temperatura minimalna")
      plot(correlation_tmin_cropped[input$month, c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), legend=FALSE, add=TRUE)
      if(input$is_ss_other == 2) {
        plot(mask(correlation_tmin_cropped[input$month, c_method_other()][[1]][[1]], correlation_tmin_cropped[input$month, c_method_other()][[1]][[2]] >= 0.05, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      else if (input$is_ss_other ==3){
        plot(mask(correlation_tmin_cropped[input$month, c_method_other()][[1]][[1]], correlation_tmin_cropped[input$month, c_method_other()][[1]][[2]] >= 0.1, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main = "Opady")
      plot(correlation_ppt_cropped[input$month, c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), legend=FALSE, add=TRUE)
      if(input$is_ss_other == 2) {
        plot(mask(correlation_ppt_cropped[input$month, c_method_other()][[1]][[1]], correlation_ppt_cropped[input$month, c_method_other()][[1]][[2]] >= 0.05, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      else if (input$is_ss_other ==3){
        plot(mask(correlation_ppt_cropped[input$month, c_method_other()][[1]][[1]], correlation_ppt_cropped[input$month, c_method_other()][[1]][[2]] >= 0.1, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
      
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 5, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[1]], col=pal_c(6), axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, breaks=cuts_c, legend.shrink = 0.3, legend.width = 0.3)
           
    
  }, res = 96, height = 350)
  
  
  #p-value
  
  output$plot_corr_p <- renderPlot({  
    
    par(oma = c(4,1,1,0), mfrow = c(1, 3), mar = c(0, 1, 2, 0))
    
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main = "p-value (temperatura maksymalna)")
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend=FALSE, add=TRUE)
      #plot(mask(correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]], correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main = "p-value (temperatura minimalna)")
      plot(correlation_tmin_cropped[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend=FALSE, add=TRUE)
      #plot(mask(correlation_tmin_cropped[input$month, c_method_other()][[1]][[2]], correlation_tmin_cropped[input$month, c_method_other()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", main = "p-value (opady)")
      plot(correlation_ppt_cropped[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=pal_cp(10), legend=FALSE, add=TRUE)
      #plot(mask(correlation_ppt_cropped[input$month, c_method_other()][[1]][[2]], correlation_ppt_cropped[input$month, c_method_other()][[1]][[2]] < 0.05, maskvalue=FALSE), col = "yellow", add=TRUE, legend=FALSE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "black")
      
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 5, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]], breaks=cuts_cp, axis.args = list(at = cuts_cp_l[1:(length(cuts_cp_l))], labels=c('0.0', '0.05','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9', '1.0')), col=mixed_cp, axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, legend.shrink = 0.3, legend.width = 0.3)
    
    
  }, res = 96, height = 350)
  
  
  
}

shinyApp(ui, server)

