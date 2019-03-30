library(shiny)
library("data.table")
library("readxl")

ui <- fluidPage(titlePanel("Calculation of Diurnal Variation"),
  sidebarLayout(
    sidebarPanel(
	p("Please set ",strong(span("first column name", style = "color:blue"))," into ",strong(span("'date'", style = "color:blue"))," and ",strong(span("date format", style = "color:blue"))," into ", strong(span("'Y/m/d H:M:S'", style = "color:blue"))," before upload!!!"),
	
        # Copy the line below to make a file upload manager
        fileInput("file", label = h3("File input")),
        hr(),
        fluidRow(column(12, verbatimTextOutput("value"))),
        downloadButton("downloadDataMean", "Download Mean"),
		downloadButton("downloadDataSD", "Download SD"),
		
	br(),
	p("Powered by Frank Chen")	
	     
	  
    ),
    mainPanel(tableOutput("tableMean")
      
    )
  )
)

server <- function(input, output) {
     options(shiny.maxRequestSize=30*1024^2) 
	 
	 # You can access the value of the widget with input$file, e.g.
     output$value <- renderPrint({
      str(input$file)
      }) 
     
	 
	 #Calculation of Diurnal Variation Mean
	 selectedDataMean <- reactive({ 
     inFile <- input$file
     if (is.null(inFile))
      return(NULL)	 
     data <- read_xlsx(inFile$datapath, col_names = TRUE,na = "")
     data <- data.table(data)
     coldata<-colnames(data)
     coldata<-c("Hour",coldata[-1])
     datat<-data[,lapply(.SD, mean,na.rm = TRUE),by=list(hour(as.POSIXct(data$date, format = "%Y/%m/%d %H:%M:%S"))),.SDcols=c(2:ncol(data))] 
     colnames(datat)<-coldata
	 DF<-data.frame(datat)
     })
	 
	 
	 
	 
	 #Calculation of Diurnal Variation SD
	 selectedDataSD <- reactive({ 
     inFile <- input$file
     if (is.null(inFile))
      return(NULL)	 
     data <- read_xlsx(inFile$datapath, col_names = TRUE,na = "")
     data <- data.table(data)
     coldata<-colnames(data)
     coldata<-c("Hour",coldata[-1])
     datat<-data[,lapply(.SD, sd,na.rm = TRUE),by=list(hour(as.POSIXct(data$date, format = "%Y/%m/%d %H:%M:%S"))),.SDcols=c(2:ncol(data))] 
     colnames(datat)<-coldata
	 DF<-data.frame(datat)
     })
	 
	 
	 
	 
	 
	 #output table
     output$tableMean <- renderTable({selectedDataMean()})
	 
	  
    #output  downloadData
    output$downloadDataMean <- downloadHandler(
    filename = function() { 
      paste('Diurnal Variation Mean', '.csv', sep='') },
    content = function(file) {
      write.csv(selectedDataMean(), file, row.names = FALSE)
      }
    )
    
	
	output$downloadDataSD <- downloadHandler(
    filename = function() { 
      paste('Diurnal Variation SD', '.csv', sep='') },
    content = function(file) {
      write.csv(selectedDataSD(), file, row.names = FALSE)
      }
    )
  
  
}

shinyApp(ui, server)