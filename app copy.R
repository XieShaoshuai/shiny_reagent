#it is designed based on https://www.nielsvandervelden.com/blog/editable-datatables-in-r-shiny-using-sql/


library(shiny)
library(shinymanager)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)
library(googlesheets4)

# Init DB using credentials data
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("12345", "12345"),
  # password will automatically be hashed
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# you can use keyring package to set database key
library(keyring)
#key_set("12345", "12345")

# Init the database
#create_db(
#  credentials_data = credentials,
#  sqlite_path = "/database.sqlite", # will be created
#  passphrase = key_get("R-shinymanager-key", "obiwankenobi")
  # passphrase = "passphrase_wihtout_keyring"
#)

gs4_auth(email = "shaoshuai.xie@gmail.com", cache = ".secrets")
#id <- gs4_create("Chilab database", sheets = c("demographics", "questionnaire"))
#id
#1z7PzX5WV_gRhmuOhYJpr-Xy4bP0Uc2McduAY8xTQH5w

usethis::use_git_ignore(".secrets")
usethis::use_git_ignore("*/.secrets")


sheet_id <- "https://docs.google.com/spreadsheets/d/1z7PzX5WV_gRhmuOhYJpr-Xy4bP0Uc2McduAY8xTQH5w/"


labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

ui <- fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output"),
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  fluidRow(
    actionButton("add_button", "Add", icon("plus")),
    actionButton("edit_button", "Edit", icon("edit")),
    actionButton("copy_button", "Copy", icon("copy")),
    actionButton("delete_button", "Delete", icon("trash-alt"))
  ),
  br(),
  fluidRow(width="100%",
           DT::dataTableOutput("responses_table", width = "100%")
  )
)

# Wrap your UI with secure_app, enabled admin mode or not
ui <- secure_app(ui, enable_admin = TRUE)


server <- function(input, output, session) {
  
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "/database.sqlite",
      passphrase = key_get("R-shinymanager-key", "obiwankenobi")
      # passphrase = "passphrase_wihtout_keyring"
    )
  )
  
  output$auth_output <- renderPrint({
    typeof(editData())
  })
  
  #my function
  responses_df <- reactive({
    
    input$submit
    input$editxx
    input$Copy
    input$delete_button
    read_sheet(sheet_id, "data")
    
  }) 
  
  #Function for the entry form that will pop-up in a model dialog when the Add and Edit buttons are pressed
  entry_form <- function(button_id){
    
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c("250px", "100px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("name", labelMandatory("name"), placeholder = ""),
                  selectInput("baozhang", labelMandatory("报账"), multiple = FALSE, choices = c("No", "Yes", "No"))
                ),
                textInput("name2", labelMandatory("name"), placeholder = ""),
                textInput("name3", labelMandatory("name"), placeholder = ""),
                textAreaInput("comment", "Comment", placeholder = "", height = 100, width = "354px"),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id, "Submit")
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  entry_edit <- function(button_id2){
    
    showModal(
      modalDialog(
        div(id=("entry_form2"),
            tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c("250px", "100px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("name_2", labelMandatory("name"), placeholder = ""),
                  selectInput("baozhang_2", labelMandatory("报账"), multiple = FALSE, choices = c("No", "Yes", "No"))
                ),
                textInput("name2_2", labelMandatory("name"), placeholder = ""),
                textInput("name3_2", labelMandatory("name"), placeholder = ""),
                textAreaInput("comment_2", "Comment", placeholder = "", height = 100, width = "354px"),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id2, "Edit")
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  
  #add data
  formData <- reactive({
    
    formData <- data.frame(name = input$name,
                           baozhang = input$baozhang,
                           name2 = input$name2,
                           name3 = input$name3,
                           comment = input$comment,
                           stringsAsFactors = FALSE)
    return(formData)
  })
  
  observeEvent(input$add_button, priority = 20,{
    
    entry_form("submit")
    
  })
  observeEvent(input$submit, priority = 20,{
    
    sheet_append(sheet_id,formData(),  "data")   #run formData function
    shinyjs::reset("entry_form")
    removeModal()
    
  })
  
  #edit data
  
  formData2 <- reactive({
    
    formData2 <- tibble(Name=input$name_2,
                           Name2=input$baozhang_2,
                           Name3=input$name2_2,
                           Name4=input$name3_2,
                           "12"=input$comment_2,
                           stringsAsFactors = FALSE)
    return(formData2)
  })
  observeEvent(input$edit_button, priority = 20,{
    
    df <- read_sheet(sheet_id, "data")
    
    showModal(
      if(length(input$responses_table_rows_selected) > 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select only one row." ),easyClose = TRUE)
      } else if(length(input$responses_table_rows_selected) < 1){
        modalDialog(
          title = "Warning",
          paste("Please select a row." ),easyClose = TRUE)
      })  
    if(length(input$responses_table_rows_selected) == 1 ){
      entry_edit("submit_edit")
    }
    
  })
  
  editData <- reactive({
    row0 <- input$responses_table_rows_selected+1
    #row0 <- as.character(row0)
    row0 <- paste0("A",row0)
    row0 <- as.character(row0)
    return(row0)
  })
  
  observeEvent(input$submit, priority = 20,{
    df <- format
    range_write(sheet_id,data=formData2(),  range=editData())   #run formData function
    #shinyjs::reset("entry_form2")
    #removeModal()
    #sheet_append(sheet_id,formData(),  "data")   #run formData function
    shinyjs::reset("entry_form")
    removeModal()
    
  })
  
  #delet data
  deleteData <- reactive({
    row <- input$responses_table_rows_selected+1
    row <- as.character(row)
    return(row)
  })
  
  observeEvent(input$delete_button, priority = 20,{
    
    if(length(input$responses_table_rows_selected)>=1 ){
      range_delete(sheet_id, range = deleteData())
    }
    
    showModal(
      
      if(length(input$responses_table_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
  })
  

  #Displaying the Data Table
  output$responses_table <- DT::renderDataTable({
    df <- responses_df()
  })
}

shinyApp(ui, server)