library(shiny)
library(shinydashboard)
library(sf)
library(RColorBrewer)
library(raster)
library(SPEI)
library(patchwork)
library(lubridate)
library(zoo)

#---------DATA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/dashboard_data.RData")

pal <- brewer.pal(7,'RdYlBu')
cuts<-c(-3, -2, -1.5, -1, 1, 1.5, 2, 3)

cuts_c <- c(-1, -0.5, -0.3, 0, 0.3, 0.5, 1)
pal_c <- colorRampPalette(c("#4575b4", "#ffffff", "#d73027"))

cuts_cp <- c(0.0, 0.05, seq(0.1, 1, by=0.1))
cuts_cp_l <- c(0.0, 0.049, 0.09999, 0.19999, 0.29999, 0.399999, 0.499999, 0.59999999999999, 0.6999999, 0.7999999, 0.9, 1.0)
pal_cp <- colorRampPalette(c("#d73027", "white"))
mixed_cp <- c("#ffffbf", "#fee090", pal_cp(9))

#---------FRONTEND-----------------------------------------------------------------------------------------------------------------------------------------------------------------------


header <- dashboardHeader(
  
  tags$li(
    class = "dropdown",
    tags$style(".main-header .logo {height: auto}"),
  ),
  title = div(
    span(HTML("Analiza prognoz klimatycznych o wysokiej rozdzielczości
                                  <br> w kontekście możliwości wystąpienia suszy w wybranych regionach Polski")),
    align = "left",
    width = "100%",
    style = "padding-right:0px; font-size: 26px; line-height: 28px; margin: 1%; color: #3c8dbc;"
  ),
  titleWidth = "100%"
)


body <- dashboardBody(id = "id_body",
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(tags$style(HTML('
      #id_main {
        margin: 20px;
        font-family: Helvetica, sans-serif;
      }
      #info {
        font-size: 16px;
        margin-top: calc(300px - 10vw);
      }
      .class_row {
        margin-top: 20px;
      }
      
      #settings {
        padding: 25px;
        font-family: Helvetica, sans-serif;
        font-size: 13px;
      }
      .skin-blue .main-header .logo {
          background-color: #ecf0f5;
        }
      .skin-blue .main-header .logo:hover {
        background-color: #ecf0f5;
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
                                                 HTML("<h5 style='font-family: Helvetica, sans-serif;'>
                                                 
                                      Rzeczywiste i prognozowane wartości indeksu SPEI (1-, 3-, 6- oraz 12-miesięcznego) wyliczone dla lipca, 
                                      w dwóch analizowanych obszarach, w kolejnych latach okresu 2007-2021.<br>  
                                      Obszar o wysokim narażeniu względem wystąpienia suszy (OWN) obejmuje województwa wielkopolskie i kujawsko-pomorskie. 
                                      Obszar o niskim narażeniu względem wystąpienia suszy (ONN) obejmuje województwa małopolskie i podkarpackie.</h5>")
                                       )
                                  ),
                         fluidRow(class = "class_row", 
                                  box(id = "settings",
                                       title = NULL, 
                                       width = 3,
                                       height = 620,
                                       solidHeader=TRUE,
                                       radioButtons("timescale",
                                                    "Skala czasowa indeksu SPEI (typ suszy):",
                                                    choices = c("1 miesiąc (susza meteorologiczna)" = 1, "3 miesiące (susza meteorologiczna i rolnicza)" = 3, "6 miesięcy (susza rolnicza)" = 6, "12 miesięcy (susza hydrologiczna)" = 12)
                                                    ),
                                       sliderInput("year",
                                                   "Rok:",
                                                   min=2007, max=2021, value=2007, step=1, sep="",
                                                   width = '100%')
                                     ),
                                   box(title = NULL,
                                       width = 9,
                                       solidHeader=TRUE,
                                       height = 620,
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
                                      HTML("<h5 style='font-family: Helvetica, sans-serif;'>
                                      Wyniki korelacji rzeczywistych i prognozowanych wartości indeksu SPEI (1-, 3-, 6-, i 12-miesięcznego) dla całego obszaru Polski (okres analizy: 2007-2021).<br><br>
                                      Poniżej: Wyniki korelacji rzeczywistych i prognozowanych wartości zmiennych klimatycznych wykorzystanych przy obliczaniu SPEI 
                                      (tj. średniej miesięcznej temperatury maksymalnej i minimalnej, a także miesięcznej sumy opadów) dla całego obszaru Polski, dla poszczególnych miesięcy (okres analizy: 2006-2021).<br><br>
                                      Poniżej: Porównanie rzeczywistych i prognozowanych wartości zmiennych klimatycznych wykorzystanych do obliczenia SPEI w poszczególnych miesiącach w okresie 2006 -2021, 
                                      w dwóch wybranych lokalizacjach, znajdujących się w centrum analizowanych obszarów. </h5>")
                                  )
                                ),
                         fluidRow(class = "class_row",
                                  box(id = "settings",
                                      title = NULL, 
                                      width = 3,
                                      height = 620,
                                      solidHeader=TRUE,
                                      radioButtons("timescale_c",
                                                   "Skala czasowa indeksu SPEI (typ suszy):",
                                                   choices = c("1 miesiąc (susza meteorologiczna)" = 1, "3 miesiące (susza meteorologiczna i rolnicza)" = 3, "6 miesięcy (susza rolnicza)" = 6, "12 miesięcy (susza hydrologiczna)" = 12)),
                                                   radioButtons("corr_method_spei",
                                                   "Metoda korelacji:",
                                                   choices = c("korelacja liniowa Pearsona" = 1, "korelacja rang Spearmana" = 2)),
                                      radioButtons("is_ss_spei",
                                                   "Zamaskuj obszary nieistotne statystycznie:",
                                                   choices = c("nie maskuj" = 1, "przy założeniu wartość p < 0.05" = 2, "przy założeniu wartość p < 0.1" = 3))
                                    ),   
                                  box(
                                      width = 9,
                                      height = 620,
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
                                      width = NULL, height = 750,
                                      solidHeader=TRUE,
                                      sliderInput(
                                      inputId = "month",
                                      label = "Miesiąc:",
                                      min = 1,
                                      max = 12,
                                      value = 1,
                                      step = 1,
                                      ticks = TRUE
                                      ),
                                      radioButtons("corr_method_other", 
                                                   "Metoda korelacji:", 
                                                   choices = c("korelacja liniowa Pearsona" = 1, "korelacja rang Spearmana" = 2)),
                                      radioButtons("is_ss_other",
                                                   "Zamaskuj obszary nieistotne statystycznie:",
                                                   choices = c("nie maskuj" = 1, "przy założeniu wartość p < 0.05" = 2, "przy założeniu wartość p < 0.1" = 3))
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
                         ),
                         fluidRow(class="class_row",
                                  box(id = "settings",
                                      title = NULL, 
                                      width = 3,
                                      height = 360,
                                      solidHeader=TRUE,
                                      div(plotOutput("localization_map1", height='200', width='100%')),
                                      radioButtons("localization1",
                                                   "Lokalizacja:",
                                                   choices = c("1: 52.5°N, 17.5°E" = 1, "2: 50.0°N, 21.0°E" = 2)
                                      )
                                  ),
                                  box(id = "settings",
                                      title = NULL, 
                                      width = 9,
                                      height = 360,
                                      solidHeader=TRUE,
                                      div(plotOutput("local_others", height=310), height=320, width='100%'),
                                  )
                         ) 
                         ), 
                tabPanel(id = "local_tab", title = "Indeks SPEI w wybranych lokalizacjach",
                         fluidRow(class = "class_row",
                                  box(id = "settings",
                                      title=NULL,
                                      width=12,
                                      solidHeader=TRUE, 
                                      HTML("<h5 style='font-family: Helvetica, sans-serif;'>
                                                 
                                      Rzeczywiste i prognozowane wartości indeksu SPEI (1-, 3-, 6- oraz 12-miesięcznego) wyliczone dla wszystkich miesięcy roku,
                                      w okresie od stycznia 2007 do grudnia 2021,
                                      w dwóch wybranych lokalizacjach, znajdujących się w centrum analizowanych obszarów.</h5>")
                                      )
                         ),
                         fluidRow(class="class_row",
                                  box(id = "settings",
                                      title = NULL, 
                                      width = 2,
                                      height = 900,
                                      solidHeader=TRUE,
                                      div(plotOutput("localization_map", height='200', width='100%')),
                                      radioButtons("localization",
                                                   "Lokalizacja:",
                                                   choices = c("1: 52.5°N, 17.5°E" = 1, "2: 50.0°N, 21.0°E" = 2)
                                      ),
                                      textOutput("test")
                                  ),
                                  box(id = "settings",
                                      title = NULL, 
                                      width = 5,
                                      height = 900,
                                      solidHeader=TRUE,
                                      div(plotOutput("local_spei_real"), height='100%', width='100%')
                                  ),
                                  box(id = "settings",
                                      title = NULL, 
                                      width = 5,
                                      height = 900,
                                      solidHeader=TRUE,
                                      div(plotOutput("local_spei_fore"), height='100%', width='100%')
                                  )
                                  
                         )
                         
                )
                ) #tabsetPanel
  ) #fluidRow
) #dashboardBody




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
  loc <- reactive(as.numeric(input$localization))
  loc1 <- reactive(as.numeric(input$localization1))
  
  
  #---------SPEI-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

    output$plot_spei <- renderPlot({  
      
      par(oma = c(2,1,3,1), mfrow = c(1, 2), mar = c(1, 2, 3, 1))
      
        plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main="Rzeczywiste", col.main="#333333", cex.main=1.3)
        plot(spei_rasters_cropped[tscale(),1][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Dane rzeczywiste", col.main="#333333")
        plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
    
        plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main="Prognozowane", col.main="#333333", cex.main=1.3)
        plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Dane prognostyczne", col.main="#333333")
        plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
    
        mtext(paste0("SPEI-", tscale()),
              side = 3,
              line = 1,
              outer = TRUE, 
              cex=1.4, col="#333333")
        
        par(fig = c(0, 1, 0, 0.7), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        
        plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
        plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], col=pal, axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, breaks=cuts,legend.width = 0.7)#, axis.args=list(cex.axis=1.5))
       

    }, res = 96, height = 570)
 
  
  #---------CORRELATION-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

  output$plot_spei_corr <- renderPlot({  
    
    par(oma = c(2,1,3,1), mfrow = c(1, 2), mar = c(1, 2, 3, 1))
    
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "Współczynnik korelacji", col.main="#333333")
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], breaks = cuts_c, col=pal_c(6), horizontal = TRUE, legend =FALSE, add=TRUE)
      
      if(input$is_ss_spei == 2) {
        plot(mask(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]] >= 0.05, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      else if (input$is_ss_spei == 3) {
        plot(mask(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]] >= 0.1, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "Wartość p", col.main="#333333")
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]], breaks = cuts_cp, col=mixed_cp, horizontal = TRUE, legend = FALSE, add=TRUE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
    
      
      mtext(paste0("SPEI-", tscale_c()),
            side = 3,
            line = 1,
            outer = TRUE, 
            cex=1.4, col="#333333")
  
      par(fig = c(0, 0.5, 0, 0.7), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], breaks = cuts_c, col=pal_c(6), horizontal = TRUE, legend.only = TRUE, legend.shrink = 0.7, legend.width=0.7)

      par(fig = c(0.5, 1, 0, 0.7), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]], breaks = cuts_cp_l, axis.args = list(at = cuts_cp_l[1:(length(cuts_cp_l))], labels=c('0.0', '0.05','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9', '1.0')), col=mixed_cp, horizontal = TRUE, legend.only = TRUE, legend.shrink = 0.7, legend.width=0.7)
      
      
  }, res = 96, height = 550)
  
  
          #---------CORRELATION OTHERS-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #correlation
  
  output$plot_corr <- renderPlot({  
    
    par(oma = c(4,1,4,0), mfrow = c(1, 3), mar = c(0, 1, 2, 0))
    
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "Temperatura maksymalna", col.main="#333333")
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), legend=FALSE, add=TRUE)
      if(input$is_ss_other == 2) {
        plot(mask(correlation_tmax_cropped[input$month, c_method_other()][[1]][[1]], correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]] >= 0.05, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      else if (input$is_ss_other ==3){
        plot(mask(correlation_tmax_cropped[input$month, c_method_other()][[1]][[1]], correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]] >= 0.1, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "Temperatura minimalna", col.main="#333333")
      plot(correlation_tmin_cropped[input$month, c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), legend=FALSE, add=TRUE)
      if(input$is_ss_other == 2) {
        plot(mask(correlation_tmin_cropped[input$month, c_method_other()][[1]][[1]], correlation_tmin_cropped[input$month, c_method_other()][[1]][[2]] >= 0.05, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      else if (input$is_ss_other ==3){
        plot(mask(correlation_tmin_cropped[input$month, c_method_other()][[1]][[1]], correlation_tmin_cropped[input$month, c_method_other()][[1]][[2]] >= 0.1, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "Opady", col.main="#333333")
      plot(correlation_ppt_cropped[input$month, c_method_other()][[1]][[1]], breaks = cuts_c, col=pal_c(6), legend=FALSE, add=TRUE)
      if(input$is_ss_other == 2) {
        plot(mask(correlation_ppt_cropped[input$month, c_method_other()][[1]][[1]], correlation_ppt_cropped[input$month, c_method_other()][[1]][[2]] >= 0.05, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      else if (input$is_ss_other ==3){
        plot(mask(correlation_ppt_cropped[input$month, c_method_other()][[1]][[1]], correlation_ppt_cropped[input$month, c_method_other()][[1]][[2]] >= 0.1, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      
      mtext("Współczynnik korelacji",
            side = 3,
            line = 2,
            outer = TRUE, 
            cex=1.1, col="#333333")
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 5, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[1]], col=pal_c(6), axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, breaks=cuts_c, legend.shrink = 0.3, legend.width = 0.3)
           
    
  }, res = 96, height = 350)
  
  
  #p-value
  
  output$plot_corr_p <- renderPlot({  
    
    par(oma = c(4,1,4,0), mfrow = c(1, 3), mar = c(0, 1, 2, 0))
    
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "Temperatura maksymalna", col.main="#333333")
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=mixed_cp, legend=FALSE, add=TRUE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "Temperatura minimalna", col.main="#333333")
      plot(correlation_tmin_cropped[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=mixed_cp, legend=FALSE, add=TRUE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "Opady", col.main="#333333")
      plot(correlation_ppt_cropped[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=mixed_cp, legend=FALSE, add=TRUE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      mtext("Wartość p",
            side = 3,
            line = 2,
            outer = TRUE, 
            cex=1.1, col="#333333")
      
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 5, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]], breaks=cuts_cp_l, axis.args = list(at = cuts_cp_l[1:(length(cuts_cp_l))], labels=c('0.0', '0.05','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9', '1.0')), col=mixed_cp, axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, legend.shrink = 0.3, legend.width = 0.3)
    
  }, res = 96, height = 350)
  
  #linear
  
  output$localization_map1 <- renderPlot({
    
    par(mar= c(0,0,0,0))
      plot(pol,  lwd=3, border = "#333333")
      plot(both, add=TRUE, lwd=3, border = "#333333")
      points(real_coord[1, 1], real_coord[2,1], col="#4575b4", pch=16) #real
      text((real_coord[1, 1]-0.3), (real_coord[2,1]+0.5), labels = "1", col="#4575b4") #real
      points(real_coord[1, 2], real_coord[2,2], col="#4575b4", pch=16) #real
      text((real_coord[1, 2]-0.6), (real_coord[2,2]-0.1), labels = "2", col="#4575b4") #real
      
  }, res = 96, height =200) 
  
  #others linear
  
  m <- reactive(as.numeric(input$month))
  output$local_others <- renderPlot({
    
    p1<- ggplot(local_data[month(local_data$time) == m(),])+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == m(),][paste0("tmax_real", loc1())])), color="rzeczywista"), size=1)+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == m(),][paste0("tmax_fore", loc1())])), color="prognozowana"), size=1, alpha=0.4)+
      scale_x_continuous(breaks=seq(2006, 2021, by=2))+
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, size=10), legend.text=element_text(size=10)) + 
      labs(x="", y=NULL, title="Średnia miesięczna temperatura maksymalna [°C]" , color = '') +
      scale_color_manual(values = c(
        'rzeczywista' = '#4575b4',
        'prognozowana' = '#f2b103'))
    
    
    p2 <- ggplot(local_data[month(local_data$time) == m(),])+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == m(),][paste0("tmin_real", loc1())])), color="rzeczywista"), size=1)+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == m(),][paste0("tmin_fore", loc1())])), color="prognozowana"), size=1, alpha=0.4)+
      scale_x_continuous(breaks=seq(2006, 2021, by=2))+
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, size=10), legend.text=element_text(size=10)) + 
      labs(x="", y=NULL, title="Średnia miesięczna temperatura minimalna [°C]", color = '') +
      scale_color_manual(values = c(
        'rzeczywista' = '#4575b4',
        'prognozowana' = '#f2b103'))
    
    p3 <- ggplot(local_data[month(local_data$time) == m(),])+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == m(),][paste0("ppt_real", loc1())])), color="rzeczywista"), size=1)+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == m(),][paste0("ppt_fore", loc1())])), color="prognozowana"), size=1, alpha=0.4)+
      scale_x_continuous(breaks=seq(2006, 2021, by=2))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5, size=10), legend.text=element_text(size=10)) + 
      labs(x="", y=NULL, title="Miesięczna suma opadów [mm]", color = '') +
      scale_color_manual(values = c(
        'rzeczywista' = '#4575b4',
        'prognozowana' = '#f2b103'))
    
    combined <- p1 + p2 + p3 & theme(legend.position = "bottom")
    combined + plot_layout(guides = "collect")
    
  }, res=96, height=300)
  
  #---------LINEAR-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$localization_map <- renderPlot({
    
    par(mar= c(0,0,0,0))
    plot(pol,  lwd=3, border = "#333333")
    plot(both, add=TRUE, lwd=3, border = "#333333")
    points(real_coord[1, 1], real_coord[2,1], col="#4575b4", pch=16) #real
    text((real_coord[1, 1]-0.3), (real_coord[2,1]+0.5), labels = "1", col="#4575b4") #real
    points(real_coord[1, 2], real_coord[2,2], col="#4575b4", pch=16) #real
    text((real_coord[1, 2]-0.6), (real_coord[2,2]-0.1), labels = "2", col="#4575b4") #real
    
  }, res = 96, height =200) 
  
  #real
  
  output$local_spei_real <- renderPlot({
    
    dd<- data.frame(spei=c(), sgn=c())
    for (i in c(1, 3, 6, 12))
    {
      a<-spei(local_data[paste0("ppt_real", loc())] - local_data[paste0("PET_real",loc())], i, na.rm = TRUE) #1 month [x] indicates region
      sn = ifelse(a$fitted >= 0, "pos", "neg")
      scale = rep(paste0("SPEI-", i), length(sign))
      ti = local_data$time
      
      d <- data.frame(t=ti, a=a$fitted, sign=sn, sc=factor(scale, levels=c("SPEI-1", "SPEI-3", "SPEI-6", "SPEI-12")))
      d<-d[13:length(local_data$time),]
      
      dd<- rbind(dd, d)
    }
    colnames(dd) <- c("time", "spei", "sign", "scale")
    
    ggplot(dd) +
      geom_bar(aes(x = as.Date(time), y = spei, col = sign, fill = sign),
               show.legend = F, stat = "identity") +
      scale_color_manual(values = c("pos" = "#4575b4", "neg" = "#d73027")) +
      scale_fill_manual(values = c("pos"  = "#4575b4", "neg" = "#d73027")) +
      scale_y_continuous(limits = c(-3, 3), breaks = -3:3) +
      scale_x_date(date_breaks="2 years", date_labels = "%Y")+
      labs(x=NULL, y="SPEI") + 
      ggtitle("Rzeczywiste") +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5, margin=margin(0,0,20,0), size=16), 
            strip.background = element_rect(colour=NA, fill=NA),
            strip.text.x = element_text(size = 11, colour="black")) +
      facet_wrap(~ scale, ncol = 1, scales='free')
    
  }, res=96, height = 850)
  
  #forecast
  
  output$local_spei_fore <- renderPlot({
    
    dd<- data.frame(spei=c(), sgn=c())
    for (i in c(1, 3, 6, 12))
    {
      a<-spei(local_data[paste0("ppt_fore", loc())] - local_data[paste0("PET_fore",loc())], i, na.rm = TRUE) #1 month [x] indicates region
      sn = ifelse(a$fitted >= 0, "pos", "neg")
      scale = rep(paste0("SPEI-", i), length(sign))
      ti = local_data$time
      
      d <- data.frame(t=ti, a=a$fitted, sign=sn, sc=factor(scale, levels=c("SPEI-1", "SPEI-3", "SPEI-6", "SPEI-12")))
      d<-d[13:length(local_data$time),]
      
      dd<- rbind(dd, d)
    }
    colnames(dd) <- c("time", "spei", "sign", "scale")
    
    ggplot(dd) +
      geom_bar(aes(x = as.Date(time), y = spei, col = sign, fill = sign),
               show.legend = F, stat = "identity") +
      scale_color_manual(values = c("pos" = "#4575b4", "neg" = "#d73027")) +
      scale_fill_manual(values = c("pos"  = "#4575b4", "neg" = "#d73027")) +
      scale_y_continuous(limits = c(-3, 3), breaks = -3:3) +
      scale_x_date(date_breaks="2 years", date_labels = "%Y")+
      labs(x=NULL, y="SPEI") + 
      ggtitle("Prognozowane") +
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5, margin=margin(0,0,20,0), size=16), 
            strip.background = element_rect(colour=NA, fill=NA),
            strip.text.x = element_text(size = 11, colour="black")) +
      
      facet_wrap(~ scale, ncol = 1, scales='free')
    
  }, res=96, height = 850)
  
  
}

shinyApp(ui, server)

