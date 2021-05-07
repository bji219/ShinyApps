library(bslib)
library(readxl)
library(ggplot2)
library(plotly)
library(readr)
library(magick)
library(magrittr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(dashboardthemes)

# Load VTR Data ----
url <- "https://raw.githubusercontent.com/bji219/ShinyApps/master/MtlOpt/R_plot_Data_4-27.csv"
VTR_Data <- read_csv(url, col_types=cols())

# Xaxis labels 
xCut <-c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500)

# Custom Colors
modColors <- c("0.5 MPa" = "#fcce03","5 MPa" = "#fc8c03","50 MPa" = "#a2a2a3","500 MPa" = "#002fff")
# Custom shapes
modShapes <-  c("0.5 MPa" = 1,"5 MPa" = 2,"50 MPa" = 5,"500 MPa" = 0)

# Line of equality
eqline <- matrix(c(0,0,1.5,1.5),ncol=2,byrow=TRUE)
colnames(eqline) <- c("X","Y")
rownames(eqline) <- c("1","2")
eqline <- as.table(eqline)
eqline <- as.data.frame.matrix(eqline)

# The App 
ui <- dashboardPage(

    skin = "black",    
    
    # Add header
    dashboardHeader(title = 'Optimization'), # seems to be a title length limit
    
    dashboardSidebar(
        sliderInput(inputId = "cutoff",label = "Choose a Soft Callus Cutoff",value = 0, min = 0, max = 1500, step = 100),
        
        selectInput("mod","Filter by Soft Callus Modulus: ",c("","0.5 MPa","5 MPa","50 MPa","500 MPa"), multiple = TRUE) 
        
    ),
    
    dashboardBody(
    tabsetPanel(
    
    tabPanel("Plot",
             
             fluidRow(
             # Add Plot Output 
             column(6,plotOutput("VTR_vs_Bio")),
             # Add Image
             column(6,align = "center",imageOutput("image2"),style = "margin-top: 100px;")
             ),
             
             fluidRow(
            # Add Cutoff Plot
            column(6,plotOutput("Cut_Plot")),
            
            # Add descriptive text
            column(width=6,align = "center", 
                   h4("Cross-section of an ovine tibia depicting the change in material properties with soft callus cutoff.",
                      style = "padding:200px;"))
             )
        ),
    
    tabPanel("gif",
             
             
             fluidRow(
                 
                 # Add the gif to a separate panel
                 img(src="Xsection.gif", height='500px',width='1000px',style="display: block; margin-top: 75px; margin-left: auto; margin-right: auto;")
             ),
             
                # Add descriptive text
                column(width=12,align = "center", 
                    h4("Animated cross-section of an ovine tibia depicting the change in material properties with soft callus cutoff.",
                       style = "padding:50px;"))
             
             ),
    
    tabPanel("Data Table",
             # Adds data table
             dataTableOutput("Table")
            )
        ),
    
    # Also add some custom CSS colors
    tags$head(tags$style(HTML('
                              
        /* body */
        .content-wrapper, .right-side {
                            background-color: #ffffff;
                                }
        /* main sidebar */
        .skin-black .main-sidebar {
                            background-color: #DCDCDC;
                                }
                              
        /* siderbar text color */
        .main-sidebar .sidebar{
            color: #000000;
            }
                              
                              ')))

    )
)

server <- function(input, output, session) {
    
    # Match plot themes
    thematic::thematic_shiny()
    
    # Filter the data by slider input
    filtered <- reactive({
        
        if(is.null(input$mod)){
            
            VTR_Data %>% filter(CutOff == input$cutoff)
            
        }
        else {
            VTR_Data %>% filter(Modulus %in% input$mod) %>% filter(CutOff == input$cutoff) 
        }
    })


    # Filter Data by Slider value
    output$Table <- renderDataTable(filtered())
    
    # Insert THe plots of Cutoff & Stuff here
    output$VTR_vs_Bio <- renderPlot(
        
        ggplot() +
        
        #Plot line of equality
        geom_line(data = eqline,aes(x = X, y = Y), linetype = 'dashed')+
        
        # Add VTR vs. Bio Points
        geom_point(alpha = 0.7, show.legend=TRUE,data = filtered(), aes(x=Bio, y=VTR, color = Modulus, shape = Modulus),size = 4,stroke =1.5)+
        
        # Labels for plot
        labs(title='Virtual vs. Biomechanical Rigidity',
             x='Biomechanical Torsional Rigidity GJ [Nm^2/deg]',
             y='Virtual Torsional Rigidity VTR [Nm^2/deg]')+
        
        # All theme options
        theme(plot.title = element_text(hjust = 0.5,face = "bold"),
              legend.position = 'right',
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.title = element_blank()) + 
            
            # Custom colors
            scale_colour_manual(values = modColors)+
            # Custom shapes  
            scale_shape_manual(values= modShapes)+
            
            xlim(0,1.5)+
            ylim(0,1.5)
        
    )
    
    output$Cut_Plot <- renderPlot(
    
        ggplot() +
            geom_line(data = filtered(), aes(x = xval,y = Line))+
            
            # Labels for plot
            labs(title='Piecewise (Dual Zone) Material Model',
                 x='Soft Callus Cutoff [BMD]',
                 y='Youngs Modulus, E [N/m^2]')+
            
            # All theme options
            theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                  legend.position = 'right',
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  legend.title = element_blank(),
                  axis.text.y = element_blank())+
            xlim(0,1500)+
            ylim(0,5000)+
            scale_x_continuous(labels = as.character(xCut), breaks = xCut)
            
    )
    
    # image2 sends pre-rendered images
    # need a www folder within the project directory 
    output$image2 <- renderImage({
        
        return(list(src = paste("www/",input$cutoff/100+10,".png",sep=""),
                                     filetype = "image/png",
                                     alt = paste(input$cutoff, "Cutoff")))
        
    }, deleteFile = FALSE)
    
    
    # Automatically bookmark every time an input changes- changes URL for sharing
    observe({
        reactiveValuesToList(input)
        session$doBookmark()
    })
    # Update the query string
    onBookmarked(updateQueryString)

}

shinyApp(ui, server, enableBookmarking = "url")
