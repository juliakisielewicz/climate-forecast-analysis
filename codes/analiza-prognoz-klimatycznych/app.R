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

load("data/manual_data.RData")
load("data/manual_local1.RData")

pal <- brewer.pal(7,'RdYlBu')
cuts<-c(-3, -2, -1.5, -1, 1, 1.5, 2, 3)

cuts_c <- c(-1, -0.5, -0.3, 0, 0.3, 0.5, 1)
pal_c <- colorRampPalette(c("#4575b4", "#ffffff", "#d73027"))

cuts_cp <- c(0.0, 0.05, seq(0.1, 1, by=0.1))
cuts_cp_l <- c(0.0, 0.049, 0.09999, 0.19999, 0.29999, 0.399999, 0.499999, 0.59999999999999, 0.6999999, 0.7999999, 0.9, 1.0)
pal_cp <- colorRampPalette(c("#d73027", "white"))
mixed_cp <- c("#ffffbf", "#fee090", pal_cp(9))
#f2b103
#d99f03
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
    style = "padding-right:0px; font-size: 26px; line-height: 28px; margin: 2%; color: #4575b4;"
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
                                                 HTML("<h5 style='font-family: Helvetica, sans-serif;'>Indeks SPEI determinuje wystąpienie suszy. Obliczany jest na podstawie miesięcznej sumy opadów atmosferycznych, 
                                                 średniej miesięcznej temperatury minimalnej i średniej miesięcznej temperatury maksymalnej. 
                                                 Dane klimatyczne pochodzić mogą z 1, 3, 6 lub 12 miesięcy poprzedzających. 
                                                 Wartości ujemne oznaczają warunki suche, zaś wartości dodatnie - wilgotne. <br><br>
                                                 Poniższe wykresy pozwalają porównać wartości indeksu SPEI dla lipca w poszczególnych latach od 2007 do 2021 roku, uzyskane na podstawie dwóch różnych zestawów danych. 
                                                  Zaprezentowana jest również różnica pod względem podatności na suszę pomiędzy dwoma wybranymi obszarami Polski.</h5>")
                                      )
                                  ),
                         fluidRow(class = "class_row", 
                                  box(id = "settings",
                                       title = NULL, 
                                       width = 3,
                                       height = 600,
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
                tabPanel(id = "local_tab", title = "Indeks SPEI w wybranych lokalizacjach",
                         fluidRow(class="class_row",
                                  box(id = "settings",
                                      title = NULL, 
                                      width = 2,
                                      height = 900,
                                      solidHeader=TRUE,
                                      div(plotOutput("localization_map", height='200', width='100%')),
                                      radioButtons("localization",
                                                   "Wybierz punkt:",
                                                   choices = c("1" = 1, "2" = 2)
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

                         ),
                tabPanel(id = "corr_tab", title = "Porównanie danych prognostycznych i rzeczywistych", 
                         fluidRow(class = "class_row",
                                  box(id = "settings",
                                      title=NULL,
                                      width=12,
                                      solidHeader=TRUE, 
                                      HTML("<h5 style='font-family: Helvetica, sans-serif;'>W celu prześledzenia zgodności prognozy z rzeczywistymi warunkami klimatycznymi okresu 2006 - 2021, 
                                           skorelowano ze sobą oba zestawy danych. 
                                           Korelację wykonano zarówno dla samego indeksu SPEI, jak i dla poszczególnych zmiennych wykorzystanych przy jego obliczaniu - 
                                           miesięcznej sumy opadów atmosferycznych, średniej miesięcznej temperatury minimalnej i średniej miesięcznej temperatury maksymalnej. 
                                           Uwzględniono dwie metody korelacji - Pearsona i Spearmana. <br><br>
                                           Poniższe wykresy prezentują wartość współczynnika korelacji między dwoma zestawami danych oraz wartość p-value, wyznaczającą istotność statystyczną korelacji w danej lokalizacji.
                                           Ze względu na dominujący brak istotności statystycznej w obrębie analizowanych obszarów, poziom korelacji zbadano również dla otoczenia tych obszarów, mieszczącego się w granicach całej Polski.</h5>")
                                  )
                                ),
                         fluidRow(class = "class_row",
                                  box(id = "settings",
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
                                                   choices = c("brak maski" = 1, "przy założeniu p-value < 0.05" = 2, "przy założeniu p-value < 0.1" = 3))
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
                                                   choices = c("Pearsona" = 1, "Spearmana" = 2)),
                                      radioButtons("is_ss_other",
                                                   "Zamaskuj obszary bez istotności statystycznej:",
                                                   choices = c("brak maski" = 1, "przy założeniu p-value < 0.05" = 2, "przy założeniu p-value < 0.1" = 3))
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
                                                   "Wybierz punkt:",
                                                   choices = c("1" = 1, "2" = 2)
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
                         ) #tabpanel
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
      
      par(oma = c(2,1,3,1), mfrow = c(1, 2), mar = c(1, 2, 2, 1))
        
        plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main="Dane rzeczywiste", col.main="#333333")
        plot(spei_rasters_cropped[tscale(),1][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Dane rzeczywiste", col.main="#333333")
        plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
    
        plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main="Dane prognostyczne", col.main="#333333")
        plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], add=TRUE, legend=FALSE, col=pal, breaks=cuts, xlim=c(14, 24.3), ylim=c(48.2, 55), main="Dane prognostyczne", col.main="#333333")
        plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
    
        mtext(paste0("SPEI-", tscale()),
              side = 3,
              line = 1,
              outer = TRUE, 
              cex=1.4, col="#333333")
        
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
        plot(spei_rasters_cropped[tscale(),2][[1]][[y()]], col=pal, axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, breaks=cuts,legend.width = 0.4)
       
    }, res = 96, height = 550)
 
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
      ggtitle("Dane rzeczywiste") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5), strip.background = element_rect(colour=NA, fill=NA)) +
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
      ggtitle("Dane prognostyczne") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5), strip.background = element_rect(colour=NA, fill=NA)) +
      facet_wrap(~ scale, ncol = 1, scales='free')
    
  }, res=96, height = 850)
  
  
  
  #---------CORRELATION-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

  output$plot_spei_corr <- renderPlot({  
    
    par(oma = c(5,1,1,0), mfrow = c(1, 2), mar = c(0, 1, 3, 2))
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = paste0("SPEI-", tscale_c()), col.main="#333333")
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], breaks = cuts_c, col=pal_c(6), horizontal = TRUE, legend =FALSE, add=TRUE)
      
      if(input$is_ss_spei == 2) {
        plot(mask(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]] >= 0.05, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      else if (input$is_ss_spei == 3) {
        plot(mask(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]] >= 0.1, maskvalue=FALSE), col = "white", add=TRUE, legend=FALSE)
      }
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = paste0("p-value (SPEI-", tscale_c(), ")"), col.main="#333333")
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]], breaks = cuts_cp, col=mixed_cp, horizontal = TRUE, legend = FALSE, add=TRUE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
    
  
      par(fig = c(0, 0.5, 0.1, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[1]], breaks = cuts_c, col=pal_c(6), horizontal = TRUE, legend.only = TRUE, legend.shrink = 0.7, legend.width=0.6)

      par(fig = c(0.5, 1, 0.1, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(correlation_spei_cropped[tscale_c(), c_method_spei()][[1]][[2]], breaks = cuts_cp_l, axis.args = list(at = cuts_cp_l[1:(length(cuts_cp_l))], labels=c('0.0', '0.05','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9', '1.0')), col=mixed_cp, horizontal = TRUE, legend.only = TRUE, legend.shrink = 0.7, legend.width=0.6)
      
      
  }, res = 96, height = 550)
  
  
          #---------CORRELATION OTHERS-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #correlation
  
  output$plot_corr <- renderPlot({  
    
    par(oma = c(4,1,1,0), mfrow = c(1, 3), mar = c(0, 1, 2, 0))
    
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
      
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 5, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[1]], col=pal_c(6), axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, breaks=cuts_c, legend.shrink = 0.3, legend.width = 0.3)
           
    
  }, res = 96, height = 350)
  
  
  #p-value
  
  output$plot_corr_p <- renderPlot({  
    
    par(oma = c(4,1,1,0), mfrow = c(1, 3), mar = c(0, 1, 2, 0))
    
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "p-value (temperatura maksymalna)", col.main="#333333")
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=mixed_cp, legend=FALSE, add=TRUE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "p-value (temperatura minimalna)", col.main="#333333")
      plot(correlation_tmin_cropped[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=mixed_cp, legend=FALSE, add=TRUE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", main = "p-value (opady)", col.main="#333333")
      plot(correlation_ppt_cropped[input$month, c_method_other()][[1]][[2]], breaks = cuts_cp, col=mixed_cp, legend=FALSE, add=TRUE)
      plot(pol, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333", add=TRUE)
      plot(both, add=TRUE, lwd=3, xlim=c(14, 24.3), ylim=c(48.2, 55), border = "#333333")
      
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 5, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
      plot(correlation_tmax_cropped[input$month, c_method_other()][[1]][[2]], breaks=cuts_cp, axis.args = list(at = cuts_cp_l[1:(length(cuts_cp_l))], labels=c('0.0', '0.05','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9', '1.0')), col=mixed_cp, axes = FALSE, horizontal = TRUE, box=FALSE, legend.only=TRUE, legend.shrink = 0.3, legend.width = 0.3)
    
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
  
  output$local_others <- renderPlot({
    
    p1<- ggplot(local_data[month(local_data$time) == 7,])+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == 7,][paste0("tmax_real", loc1())])), color="Dane rzeczywiste"), size=1)+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == 7,][paste0("tmax_fore", loc1())])), color="Dane prognostyczne"), size=1, alpha=0.4)+
      scale_x_continuous(breaks=seq(2006, 2021, by=2))+
      scale_y_continuous(breaks=seq(19, 31, by=2), limits=c(19, 31))+
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, size=11), legend.text=element_text(size=11)) + 
      labs(x="", y=NULL, title="Średnia miesięczna temperatura maksymalna [°C]" , color = '') +
      scale_color_manual(values = c(
        'Dane rzeczywiste' = '#4575b4',
        'Dane prognostyczne' = '#f2b103'))
    p1
    
    p2 <- ggplot(local_data[month(local_data$time) == 7,])+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == 7,][paste0("tmin_real", loc1())])), color="Dane rzeczywiste"), size=1)+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == 7,][paste0("tmin_fore", loc1())])), color="Dane prognostyczne"), size=1, alpha=0.4)+
      scale_x_continuous(breaks=seq(2006, 2021, by=2))+
      scale_y_continuous(breaks=seq(12, 18, by=1), limits=c(12, 18))+
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, size=11), legend.text=element_text(size=11)) + 
      labs(x="", y=NULL, title="Średnia miesięczna temperatura minimalne [°C]", color = '') +
      scale_color_manual(values = c(
        'Dane rzeczywiste' = '#4575b4',
        'Dane prognostyczne' = '#f2b103'))
    
    p3 <- ggplot(local_data[month(local_data$time) == 7,])+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == 7,][paste0("ppt_real", loc1())])), color="Dane rzeczywiste"), size=1)+
      geom_line(aes(x=seq(2006, 2021), y=as.vector(unlist(local_data[month(local_data$time) == 7,][paste0("ppt_fore", loc1())])), color="Dane prognostyczne"), size=1, alpha=0.4)+
      scale_x_continuous(breaks=seq(2006, 2021, by=2))+
      scale_y_continuous(breaks=seq(0, 160, by=40), limits=c(0, 170))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5, size=11), legend.text=element_text(size=11)) + 
      labs(x="", y=NULL, title="Miesięczna suma opadów [mm]", color = '') +
      scale_color_manual(values = c(
        'Dane rzeczywiste' = '#4575b4',
        'Dane prognostyczne' = '#f2b103'))
    
    combined <- p1 + p2 + p3 & theme(legend.position = "bottom")
    combined + plot_layout(guides = "collect")
    
  }, res=96, height=300)
  
  
}

shinyApp(ui, server)

