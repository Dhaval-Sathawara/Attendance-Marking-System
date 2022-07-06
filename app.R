library(DT)
library(readr)
library(stringr)
library(readxl)
library(shiny)


ui <- fluidPage(

    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    tags$head(
        tags$style(
            HTML("
           
           h1 {
           font-family: Cambria;
           font-size: 40px;
           color: DarkRed;
           text-align: center;
           }
           h5 {
           font-family: Cambria;
           font-size: 30px;
           color: Black;
           text-align: center;
           }
           body {
           font-family: Cambria;
           font-size: 18px;
           text-align: justify
           }
           
           h2{
           font-size: 50px;
           font-family: Cambria;
           color: Black;
           text-align: center;
           
           }
           
           h3{ /* Header 4 - and the author and data headers use this too  */
           font-size: 30px;
           font-weight:bold;
           font-family: Cambria;
           color: Green;
           text-align: Left;
           }
           
           h4{ /* Header 4 - and the author and data headers use this too  */
           font-size: 30px;
           font-weight:bold;
           font-family: Cambria;
           color: Red;
           text-align: Left;
           }
           
           h6{ /* Header 4 - and the author and data headers use this too  */
           font-size: 30px;
           font-family: Cambria;
           color: Blue;
           text-align: Left;
           }
           
           ")
        )
    ),
    
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    

    titlePanel("ATTENDANCE MARKING SYSTEM"),

    


    sidebarLayout(
        sidebarPanel(
            #absolutePanel(bottom = 30, left =  30,
            #              h5("Developed by"),
            #              h5("Dhaval Sathawara")),
            
            fileInput("file1", "Upload Log File (*.txt)",
                      multiple = FALSE,
                      accept = c(".txt")
            ),
            fileInput("file2", "Upload Attendace File (*.xlsx)",
                      multiple = FALSE,
                      accept = c(".xls",".xlsx")
            ),
            uiOutput("select_sheet"),
            
            uiOutput("enroll_select"),
            br(),
            uiOutput("get_present_count"),
            #br(),
            uiOutput("get_absent_count"),
            #br(),
            uiOutput("get_score"),
            br(),
            br(),
            br(),
            
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Log file", 
                                 h1("Attendace Log"),
                                 verbatimTextOutput("att_log")
                        ),
                        tabPanel("Student List", 
                                 h1("Student List"),
                                 dataTableOutput("studentlist")
                        ),
                        tabPanel("Attendance", 
                                 h3("Download Report"),
                                 uiOutput("dwnldfname"),
                                 downloadButton('downloadData', 'Download'),
                                 h1("Attendance Status"),
                                 dataTableOutput("final_df")
                        )
                        
            )
        )
    )
)


server <- function(input, output) {

    log_file <- reactive(str_to_upper(read_file(input$file1$datapath)))
    
    output$att_log <- renderPrint({
        master <- log_file()
        cat(master)
    })

    attendace_sheet <- reactive(read_excel(input$file2$datapath,input$selected_sheet))
    
    output$studentlist <- renderDT({
        datatable(attendace_sheet(),filter = 'top')
    })
    

    output$select_sheet <- renderUI({
        sample_data <- excel_sheets(input$file2$datapath)
        
        selectInput("selected_sheet", "Select Attendance Sheet ",
                    (sample_data),multiple = F,
                    (sample_data)[1])
        
    })
    
    
    output$enroll_select <- renderUI({
        sample_data <- attendace_sheet()
        selectInput("enroll_head", "Select Enrollment No.",
                    names(sample_data),multiple = F,
                    names(sample_data)[1])
        
    })
        
    status_flag <- function(master, key)
    { 
        
        key=str_to_upper(as.character(key))
        print(key)
        if(is.na(key) | key=="NA")
        {
            status=""
        }  
        else
        {
            count=str_count(master, key)  
            if(count>0)
            {
                status="P"
            }  
            else
            {
                status="A"
            }
        }
        
        return(status)
    }
    
    MARK_ATTENDANCE <- function()
    {
        key=(attendace_sheet()[,input$enroll_head])
        
        master <- log_file()
        
        
        att <- c()
        for(i in 1:lengths(key))
        {
            status <- status_flag(master,key[i,])
            att <- c(att,status)
        }  
        
        final_data <- data.frame(attendace_sheet(),"Attendance"=att)
        return(final_data)
    }
    
    output$final_df <- renderDT({
        FINAL_DATA <- MARK_ATTENDANCE()
        datatable(FINAL_DATA,filter = 'top')
      
    })
    
    output$get_present_count <- renderUI({
        FINAL_DATA <- MARK_ATTENDANCE()
        present_count=sum(FINAL_DATA[,"Attendance"]=="P")
        h3(sprintf("Present : %3d ",present_count))
    })
    
    output$get_absent_count <- renderUI({
        FINAL_DATA <- MARK_ATTENDANCE()
        absent_count=sum(FINAL_DATA[,"Attendance"]=="A")
        h4(sprintf("Absent : %3d ",absent_count))
    })
    
    output$get_score <- renderUI({
        FINAL_DATA <- MARK_ATTENDANCE()
        present_count=sum(FINAL_DATA[,"Attendance"]=="P")
        absent_count=sum(FINAL_DATA[,"Attendance"]=="A")
        score <- present_count/(present_count+absent_count)
        h6(sprintf("Strength :  %3.2f%% ",score*100))
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {paste(input$reportname, '.csv', sep='')},
        content = function(file){
            FINAL_DATA <- MARK_ATTENDANCE()
            write.csv(FINAL_DATA, file)
        }
    )
    
    output$dwnldfname <- renderUI({
        
        fname <- paste(format(Sys.time(),'[%Y-%m-%d] (%A)')," Attendance Report",sep = "")
        textInput("reportname","Filename ",value = fname,width = "50%")
    })
    
    }


shinyApp(ui = ui, server = server)
