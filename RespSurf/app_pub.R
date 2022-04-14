library(shiny)
library(tidyverse)
library(plotly)
library(readxl)
library(shinyWidgets)
library(ggpubr)
# library("RColorBrewer")
# library(rio)
# library(openxlsx)

# Load VTR Data ----
url1 <- "https://raw.githubusercontent.com/bji219/ShinyApps/master/RespSurf/Tet10_R.csv"
url2 <- "https://raw.githubusercontent.com/bji219/ShinyApps/master/RespSurf/Hex8_R.csv"
url3 <- "https://raw.githubusercontent.com/bji219/ShinyApps/master/RespSurf/Hex20_R.csv"
url4 <- "https://raw.githubusercontent.com/bji219/ShinyApps/master/RespSurf/Tet10_R_VTR.csv"
url5 <- "https://raw.githubusercontent.com/bji219/ShinyApps/master/RespSurf/Hex8_R_VTR.csv"
url6 <- "https://raw.githubusercontent.com/bji219/ShinyApps/master/RespSurf/Hex20_R_VTR.csv"

# Import RMSE data
tet10_raw <- read_csv(url1, col_types=cols())
hex8_raw <- read_csv(url2, col_types=cols())
hex20_raw <- read_csv(url3, col_types=cols())

# Import VTR Data
Tet10_VTR <- read_csv(url4, col_types=cols())
Hex8_VTR <- read_csv(url5, col_types=cols())
Hex20_VTR <- read_csv(url6, col_types=cols())

# Turn into data frame
tet10_rmse = data.frame(tet10_raw$Slope,tet10_raw$Cutoff,tet10_raw$RMSE)
hex8_rmse = data.frame(hex8_raw$Slope,hex8_raw$Cutoff,hex8_raw$RMSE)
hex20_rmse = data.frame(hex20_raw$Slope,hex20_raw$Cutoff,hex20_raw$RMSE)


colnames(tet10_rmse) <- c('Slope','Cutoff','RMSE')
colnames(hex8_rmse) <- c('Slope','Cutoff','RMSE')
colnames(hex20_rmse) <- c('Slope','Cutoff','RMSE')

# Plane for RMSE surf
planeDf <- data.frame(x = rep(range(tet10_rmse$Cutoff), 2), y = rep(range(tet10_rmse$Slope), each = 2), z = 0.187198) # height of plane is STDV Bio GJ

#Import Stress/Strain Data
stressStrain <- read.csv("G:/Shared drives/Dailey Lab/2. Projects/49. Response Surface Optimization - November 2021/Excel Files/stressStrain_Stats_HPC_all_13k_Cutoff_Refine_v02.csv") #import .csv

# Labels for sliders
cutoff_labels <- c("0","250","500","665","750","1000","1050","1100","1150","1200","1250","1300","1350","1400","1450","1500")
slope_labels <- c("5000","7500","8750","10000","10225","11250","12500","15000","17500","20000") 

# Line of Equality
max_x <- 2.0
eqline <- matrix(c(0,0,max_x,max_x),ncol=2,byrow=TRUE)
colnames(eqline) <- c("X","Y")
rownames(eqline) <- c("1","2")
eqline <- as.table(eqline)
eqline <- as.data.frame.matrix(eqline)

# custom grid style
axx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=FALSE,
  backgroundcolor='rgb(230,230,230)',
  title="Cutoff"
)
axy <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=FALSE,
  backgroundcolor='rgb(230,230,230)',
  title="Slope"
)
axz <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=FALSE,
  backgroundcolor='rgb(230,230,230)',
  title="RMSE"
)

plane_colors <- list('#FF0000','#FF0000')
marker_colors <- list(color = 'rgb(255,0,0)', size = 7)

# Custom theme for GJ vs. VTR plots
theme_GJ <- function(){
  # Labels for plot
  labs(title='Virtual vs. Biomechanical Rigidity',
       x='Biomechanical Torsional Rigidity GJ [Nm^2/deg]',
       y='Virtual Torsional Rigidity VTR [Nm^2/deg]')+
    
    # All theme options
    theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.position = 'right',panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank())+
    xlim(0,max_x)+
    ylim(0,max_x)
}


ui <- fluidPage(
  
  sliderTextInput(inputId = "cut_slide", label = "Soft Tissue Cutoff [mgHA/cc]", choices = cutoff_labels, selected = "665"),
  sliderTextInput(inputId = "slope_slide", label = "Mtl. Assignment Slope", choices = slope_labels, selected = "10225"),
  plotlyOutput("RMSE_Surf"),
  plotOutput("VTR_vs_GJ"),
  fluidRow(column(6,imageOutput("image_tet10")),column(6,imageOutput("image_hex8")))
  
)
server <- function(input, output, session) {
  
  # Create reactive expression for RMSE data set
  filtered_RMSE10 <- reactive({
    tet10_rmse 
  })
  filtered_RMSE8 <- reactive({
    hex8_rmse
  })
  filtered_RMSE20 <- reactive({
    hex20_rmse
  })
  
  # Create reactive expressions for VTR vs. GJ Data
  tet10_vtr_gj <- reactive({
    Tet10_VTR %>% filter(Slope == input$slope_slide, Cut == input$cut_slide) 
  })
  
  hex8_vtr_gj <- reactive({
    Hex8_VTR %>% filter(Slope == input$slope_slide, Cut == input$cut_slide)
  })
  
  hex20_vtr_gj <- reactive({
    Hex20_VTR %>% filter(Slope == input$slope_slide, Cut == input$cut_slide)
  })
  
  # Create Filtered RMSE reference point on surface
  tet10_point <-reactive({
    tet10_raw %>% filter(Slope == input$slope_slide, Cutoff == input$cut_slide) 
  })
  hex8_point <-reactive({
    hex8_raw %>% filter(Slope == input$slope_slide, Cutoff == input$cut_slide) 
  })
  hex20_point <-reactive({
    hex20_raw %>% filter(Slope == input$slope_slide, Cutoff == input$cut_slide) 
  })
  
  color <- rep(0,0.187198)
  # RMSE Individual Plots
  tetsurf <- reactive({
    tetsurf <- plot_ly(filtered_RMSE10(), x = ~Cutoff, y = ~Slope, z = ~RMSE, scene = 'scene') %>%
      layout(title = 'Tet10 RMSE Surface') %>% add_trace(type="mesh3d", intensity = ~RMSE, colorscale= "Viridis", showscale = FALSE ) %>% 
      add_mesh(data = planeDf, x = ~x, y = ~y, z = ~z, opacity = 0.25, facecolor = plane_colors) %>% 
      add_trace(data = tet10_point(), x = ~Cutoff, y = ~Slope, z = ~RMSE, marker = marker_colors)
  })
  
  hex8surf <- reactive({
    hex8surf <- plot_ly(filtered_RMSE8(), x = ~Cutoff, y = ~Slope, z = ~RMSE, scene = 'scene2') %>%
      layout(title = 'Hex8 RMSE Surface') %>% add_trace(type="mesh3d", intensity = ~RMSE, showscale = FALSE ) %>% 
      add_mesh(data = planeDf, x = ~x, y = ~y, z = ~z, opacity = 0.25, facecolor = plane_colors) %>%
      add_trace(data = hex8_point(), x = ~Cutoff, y = ~Slope, z = ~RMSE, marker = marker_colors)
  })
  
  hex20surf <- reactive({
    hex20surf <- plot_ly(filtered_RMSE20(), x = ~Cutoff, y = ~Slope, z = ~RMSE, scene = 'scene3') %>%
      layout(title = 'Hex20 RMSE Surface') %>% add_trace(type="mesh3d", intensity = ~RMSE, showscale = FALSE) %>% 
      add_mesh(data = planeDf, x = ~x, y = ~y, z = ~z, opacity = 0.25, facecolor = plane_colors) %>%
      add_trace(data = hex20_point(), x = ~Cutoff, y = ~Slope, z = ~RMSE, marker = marker_colors)
  })
  
  # Reactive RMSE subplot 
  output$RMSE_Surf <- renderPlotly(
    
    rmse_surfs <- subplot(tetsurf(),hex8surf(),hex20surf())  %>% 
      layout(showlegend = FALSE, title = "RMSE Surfaces", colorscale = TRUE,
             scene = list(domain=list(x=c(0,0.33),y=c(0,1)),
                          xaxis=axx, yaxis=axy, zaxis=axz,
                          aspectmode='cube', camera = list(eye = list(x = -1.5, y = -1.5, z = 1.5))),
             scene2 = list(domain=list(x=c(0.33,0.66),y=c(0,1)),
                           xaxis=axx, yaxis=axy, zaxis=axz,
                           aspectmode='cube', camera = list(eye = list(x = -1.5, y = -1.5, z = 1.5))),
             scene3 = list(domain=list(x=c(0.66,1),y=c(0,1)),
                           xaxis=axx, yaxis=axy, zaxis=axz,
                           aspectmode='cube', camera = list(eye = list(x = -1.5, y = -1.5, z = 1.5))),
             annotations = list(list(x = 0.125, y = 0.95, font = list(size = 16), text = "Tet10", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE), 
                                list(x = 0.5, y = 0.95, font = list(size = 16), text = "Hex8", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE),
                                list(x = 0.85, y = 0.95, font = list(size = 16), text = "Hex20", xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE
                                )))
  )
  
  # VTR Vs. GJ Individual Plots
  tet_vtr_gj_plot <- reactive({
    tet_vtr_gj <- ggplot() +
      
      #Plot line of equality
      geom_line(data = eqline,aes(x = X, y = Y), linetype = 'dashed')+
      
      # Add VTR vs. Bio Points
      geom_point(alpha = 0.7, show.legend=TRUE,data = tet10_vtr_gj(), aes(x=Bio, y=VTR), colour="black", size = 4,stroke =1.5)+ #, color=VTR
      
      labs(title='Tet10 VTR vs. GJ',
           x='Biomechanical Torsional Rigidity GJ [Nm^2/deg]',
           y='Virtual Torsional Rigidity VTR [Nm^2/deg]')+
      
      # All theme options
      theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.position = 'right', panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.title = element_blank())+
      xlim(0,max_x)+
      ylim(0,max_x)
      # scale_color_manual(values = c("VTR" = "#fcce03"))
  })
  
  hex8_vtr_gj_plot <- reactive({
    hex8_vtr_gj_plot <- ggplot() +
      
      #Plot line of equality
      geom_line(data = eqline,aes(x = X, y = Y), linetype = 'dashed')+
      
      # Add VTR vs. Bio Points
      geom_point(alpha = 0.7, show.legend=TRUE,data = hex8_vtr_gj(), aes(x=Bio, y=VTR),size = 4,stroke =1.5)+
      labs(title='Hex8 VTR vs. GJ',
           x='Biomechanical Torsional Rigidity GJ [Nm^2/deg]',
           y='Virtual Torsional Rigidity VTR [Nm^2/deg]')+
      
      # All theme options
      theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.position = 'right',panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.title = element_blank())+
      xlim(0,max_x)+
      ylim(0,max_x)
  })
  
  hex20_vtr_gj_plot <- reactive({
    hex20_vtr_gj_plot <- ggplot() +
      
      #Plot line of equality
      geom_line(data = eqline,aes(x = X, y = Y), linetype = 'dashed')+
      
      # Add VTR vs. Bio Points
      geom_point(alpha = 0.7, show.legend=TRUE,data = hex20_vtr_gj(), aes(x=Bio, y=VTR),size = 4,stroke =1.5)+
      labs(title='Hex20 VTR vs. GJ',
           x='Biomechanical Torsional Rigidity GJ [Nm^2/deg]',
           y='Virtual Torsional Rigidity VTR [Nm^2/deg]')+
      
      # All theme options
      theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.position = 'right',panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.title = element_blank())+
      xlim(0,max_x)+
      ylim(0,max_x)
  })
  
  
  # Output for VTR vs GJ subplot
  output$VTR_vs_GJ <- renderPlot(
     ggarrange(tet_vtr_gj_plot(),hex8_vtr_gj_plot(),hex20_vtr_gj_plot(),
              # labels = c("Tet10","Hex8","Hex20"),
              ncol = 3, nrow = 1)
  )
  
  # Tet10 Images
  # need a www folder within the project directory 
  output$image_tet10 <- renderImage({
    
    return(list(src = paste("www/Tet10/86-07_",input$slope_slide,"_",input$cut_slide,".png",sep=""),
                filetype = "image/png",
                width = "450",
                height = "300",
                alt = paste(input$slope_slide, "Tet10")))
    
  }, deleteFile = FALSE)
  
  
  # Hex Images
  output$image_hex8 <- renderImage({
    
    return(list(src = paste("www/Hex8/86-07_",input$slope_slide,"_",input$cut_slide,".png",sep=""),
                filetype = "image/png",
                width = "450",
                height = "300",
                alt = paste(input$cut_slide, "Hex8")))
    
  }, deleteFile = FALSE)
  
  
  
  
  
}
shinyApp(ui, server)


### 
# Plotly color scales
# aggrnyl     agsunset    blackbody   bluered     blues       blugrn      bluyl       brwnyl
# bugn        bupu        burg        burgyl      cividis     darkmint    electric    emrld
#gnbu        greens      greys       hot         inferno     jet         magenta     magma
#mint        orrd        oranges     oryel       peach       pinkyl      plasma      plotly3
#pubu        pubugn      purd        purp        purples     purpor      rainbow     rdbu
#rdpu        redor       reds        sunset      sunsetdark  teal        tealgrn     turbo
#viridis     ylgn        ylgnbu      ylorbr      ylorrd      algae       amp         deep
#dense       gray        haline      ice         matter      solar       speed       tempo
#thermal     turbid      armyrose    brbg        earth       fall        geyser      prgn
#piyg        picnic      portland    puor        rdgy        rdylbu      rdylgn      spectral
#tealrose    temps       tropic      balance     curl        delta       oxy         edge
# hsv         icefire     phase       twilight    mrybm       mygbm