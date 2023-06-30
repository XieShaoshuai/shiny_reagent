#it is designed based on https://www.nielsvandervelden.com/blog/editable-datatables-in-r-shiny-using-sql/


#library(shiny)
library(shinymanager)
library(shinydashboard)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)
library(googlesheets4)
library(reactable)

#editData2 <- "B2"

# Init DB using credentials data
credentials <- data.frame(
  user = c("chilab", "chilab_manager"),
  password = c("chi12345", "admin12345"),
   #password will automatically be hashed
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
#  passphrase = key_get("R-shinymanager-key", "obiwankenobi"),
#  #passphrase = "passphrase_wihtout_keyring"
#)

gs4_auth(email = "shaoshuai.xie@gmail.com", cache = ".secrets")
#id <- gs4_create("Chilab database", sheets = c("demographics", "questionnaire"))
#id
#1z7PzX5WV_gRhmuOhYJpr-Xy4bP0Uc2McduAY8xTQH5w

#usethis::use_git_ignore(".secrets")
#usethis::use_git_ignore("*/.secrets")


sheet_id <- "https://docs.google.com/spreadsheets/d/1z7PzX5WV_gRhmuOhYJpr-Xy4bP0Uc2McduAY8xTQH5w/"


labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"


#dashboard
header <- dashboardHeader(title = ("Chi-Lab 耗材管理系统") )
#sidebar----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("试剂耗材", tabName='home1',icon = icon("edit")),
    tags$hr(style="border-color: grey;"),
    
    menuItem("服务",tabName='home2',icon=icon("edit")),
    tags$hr(style="border-color: grey;"),
    
    menuItem("校内转账",tabName='home3',icon=icon("edit")),
    tags$hr(style="border-color: grey;"),
    
    tags$hr(style="border-color: grey;"),
    tags$hr(style="border-color: grey;")

  )
  
)


#body----
body <-  dashboardBody(
  tabItems(
    tabItem(tabName = "home1",
            fluidRow(
              box(title=strong("试剂耗材"),status = "info",width=12,
                  verbatimTextOutput("auth_output2"),
                  verbatimTextOutput("auth_output"),
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(appCSS),
                  fluidRow(
                    actionButton("add_button", "新增", icon("plus")),
                    actionButton("edit_button", "修改", icon("edit")),
                    actionButton("edit_button2", "已报账", icon("edit")),
                    actionButton("re_edit", "未报账", icon("copy")),
                    actionButton("delete_button", "Delete", icon("trash-alt"))
                  ),
                  br(),
                  fluidRow(width="100%",
                           DT::dataTableOutput("responses_table",height = "auto" ,width = "100%")
                  )
              )
            )
    ),
    tabItem(tabName = "home2",
            fluidRow(
              box(title=strong("服务"),status = "info",width=12,
                  verbatimTextOutput("auth_output2_2"),
                  verbatimTextOutput("auth_output_2"),
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(appCSS),
                  fluidRow(
                    actionButton("add_button_2", "新增", icon("plus")),
                    actionButton("edit_button_2", "修改", icon("edit")),
                    actionButton("edit_button2_2", "已报账", icon("edit")),
                    actionButton("re_edit_2", "未报账", icon("copy")),
                    actionButton("delete_button_2", "Delete", icon("trash-alt"))
                  ),
                  br(),
                  fluidRow(width="100%",
                           DT::dataTableOutput("responses_table_2",height = "auto" ,width = "100%")
                  )
              )
            )
    ),
    tabItem(tabName = "home3",
            fluidRow(
              box(title=strong("校内转账"),status = "info",width=12,
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(appCSS),
                  fluidRow(
                    actionButton("add_button_3", "新增", icon("plus")),
                    actionButton("delete_button_3", "Delete", icon("trash-alt"))
                  ),
                  br(),
                  fluidRow(width="100%",
                           DT::dataTableOutput("responses_table_3",height = "auto" ,width = "100%")
                  )
              )
            )
    )

  )
)

ui <- dashboardPage(header,sidebar,body,
              skin="blue")




# Wrap your UI with secure_app, enabled admin mode or not
ui <- secure_app(ui,  enable_admin = TRUE)


server <- function(input, output, session) {
  
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output2 <- renderPrint({
    "Bug:点击修改键后，有概率出现选择行数出错bug。需要在弹窗弹出后后检查下方的格式是否为Bxx格式,如果不存在数字，关闭弹出窗口重新操作"
  })
  
  output$auth_output <- renderPrint({
    editData()
  })
  
  output$auth_output2_2 <- renderPrint({
    "Bug:点击修改键后，有概率出现选择行数出错bug。需要在弹窗弹出后后检查下方的格式是否为Bxx格式,如果不存在数字，关闭弹出窗口重新操作"
  })
  
  output$auth_output_2 <- renderPrint({
    editData_2()
  })
  #-------试剂耗材----
  #my function
  responses_df <- reactive({
    
    input$submit
    input$submit_edit
    input$edit_button2
    input$re_edit
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
                  textInput("A", "耗材", placeholder = "")
                ),
                textInput("B", "货号", placeholder = ""),
                textInput("C", "生产商", placeholder = ""),
                textInput("D", "供货", placeholder = ""),
                textInput("E", "联系人", placeholder = ""),
                textInput("F0", "单价", placeholder = ""),
                textInput("G", "规格", placeholder = ""),
                textInput("H", "数量", placeholder = ""),
                textInput("I", "总价", placeholder = ""),
                textInput("J", "采购日期", placeholder = ""),
                textInput("K", "采购人", placeholder = ""),
                textInput("L", "用途", placeholder = ""),
                textInput("M", "报账时间", placeholder = ""),
                textAreaInput("N", "备注", placeholder = "", height = 40, width = "354px"),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id, "Submit")
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  #add data
  formData <- reactive({
    
    formData <- data.frame("耗材" = input$A,
                           "货号" = input$B,
                           "生产商" = input$C,
                           "供货商" = input$D,
                           "联系人" = input$E,
                           "单价" = input$F0,
                           "规格" = input$G,
                           "数量" = input$H,
                           "总价" = input$I,
                           "采购日期" = input$J,
                           "采购人" = input$K,
                           "用途" = input$L,
                           "报账时间" = input$M,
                           "备注" = input$N)
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
  
  #添加报账信息
  editData <- reactive({
    row <- input$responses_table_rows_selected+1
    row <- as.character(row)
    row <- paste0("M",row)
    return(row)
  })
  
  observeEvent(input$edit_button2, priority = 20,{
    if(length(input$responses_table_rows_selected)>=1 ){
      data <- data.frame("报账时间" = rep(as.character(format(Sys.Date(), format="%Y-%m")), 1))
      range_write(sheet_id, data, range = editData(),col_names = FALSE)
    }
    
    showModal(
      
      if(length(input$responses_table_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
  })
  
  #撤销报账信息
  observeEvent(input$re_edit, priority = 20,{
    if(length(input$responses_table_rows_selected)>=1 ){
      data <- data.frame("报账时间" = rep(as.character("未报账"), 1))
      range_write(sheet_id, data, range = editData(),col_names = FALSE)
    }
    
    showModal(
      
      if(length(input$responses_table_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
    
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
  
  
  #edit database
  
  editData2 <- reactive({
    row <- input$responses_table_rows_selected+1
    row <- as.character(row)
    row <- paste0("A",row)
    return(row)
  })
  
  observeEvent(input$edit_button, priority = 20,{
    
    SQL_df <- read_sheet(sheet_id, "data")
    
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
      
      entry_form("submit_edit")
      
      row <- as.character(input$responses_table_rows_selected)
      
      updateTextInput(session, "A", value = as.character(SQL_df[row, 1]))
      updateTextInput(session, "B", value = as.character(SQL_df[row, 2]))
      updateTextInput(session, "C", value = as.character(SQL_df[row, 3]))
      updateTextInput(session, "D", value = as.character(SQL_df[row, 4]))
      updateTextInput(session, "E", value = as.character(SQL_df[row, 5]))
      updateTextInput(session, "F0", value = as.character(SQL_df[row,6]))
      updateTextInput(session, "G", value = as.character(SQL_df[row, 7]))
      updateTextInput(session, "H", value = as.character(SQL_df[row, 8]))
      updateTextInput(session, "I", value = as.character(SQL_df[row, 9]))
      updateTextInput(session, "J", value = as.character(SQL_df[row, 10]))
      updateTextInput(session, "K", value = as.character(SQL_df[row, 11]))
      updateTextInput(session, "L", value = as.character(SQL_df[row, 12]))
      updateTextInput(session, "M", value = as.character(SQL_df[row, 13]))
      updateTextInput(session, "N", value = as.character(SQL_df[row, 14]))
      
    }
    
  })
  
  #Add edit data
  observeEvent(input$submit_edit, priority = 20, {
    data <- data.frame("耗材" = input$A,
                       "货号" = input$B,
                       "生产商" = input$C,
                       "供货商" = input$D,
                       "联系人" = input$E,
                       "单价" = input$F0,
                       "规格" = input$G,
                       "数量" = input$H,
                       "总价" = input$I,
                       "采购日期" = input$J,
                       "采购人" = input$K,
                       "用途" = input$L,
                       "备注" = input$M,
                       "报账时间" = input$N)
    #range_write(sheet_id,data,range=editData2(),col_names = FALSE)
    #data <- data.frame("耗材" = rep(as.character("未报账"), 1))
    range_write(sheet_id, data, range = editData2(),col_names = FALSE)
    shinyjs::reset("entry_form")
    removeModal()
    
  })
  

  #Displaying the Data Table
  output$responses_table <- DT::renderDataTable({
    df <- responses_df()
    datatable(df,selection = 'single',height="auto",width="100%")
  })
  #----  服务----
  #my function
  responses_df_2 <- reactive({
    
    input$submit_2
    input$submit_edit_2
    input$edit_button2_2
    input$re_edit_2
    input$delete_button_2
    read_sheet(sheet_id, "data_2")
    
  }) 
  
  #Function for the entry form that will pop-up in a model dialog when the Add and Edit buttons are pressed
  entry_form_2 <- function(button_id_2){
    
    showModal(
      modalDialog(
        div(id=("entry_form_2"),
            tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c("250px", "100px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("A_2", "实验类型", placeholder = "")
                ),
                textInput("B_2", "货号", placeholder = ""),
                textInput("C_2", "生产商", placeholder = ""),
                textInput("D_2", "供货", placeholder = ""),
                textInput("E_2", "联系人", placeholder = ""),
                textInput("F0_2", "单价", placeholder = ""),
                textInput("G_2", "规格", placeholder = ""),
                textInput("H_2", "数量", placeholder = ""),
                textInput("I_2", "总价", placeholder = ""),
                textInput("J_2", "采购日期", placeholder = ""),
                textInput("K_2", "采购人", placeholder = ""),
                textInput("L_2", "用途", placeholder = ""),
                textInput("M_2", "报账时间", placeholder = ""),
                textAreaInput("N_2", "备注", placeholder = "", height = 40, width = "354px"),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id_2, "Submit")
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  #add data
  formData_2 <- reactive({
    
    formData <- data.frame("实验类型" = input$A_2,
                           "货号" = input$B_2,
                           "生产商" = input$C_2,
                           "供货商" = input$D_2,
                           "联系人" = input$E_2,
                           "单价" = input$F0_2,
                           "规格" = input$G_2,
                           "数量" = input$H_2,
                           "总价" = input$I_2,
                           "采购日期" = input$J_2,
                           "采购人" = input$K_2,
                           "用途" = input$L_2,
                           "报账时间" = input$M_2,
                           "备注" = input$N_2)
    return(formData)
  })
  
  observeEvent(input$add_button_2, priority = 20,{
    
    entry_form_2("submit_2")
    
  })
  observeEvent(input$submit_2, priority = 20,{
    
    sheet_append(sheet_id,formData_2(),sheet='data_2')   #run formData function
    shinyjs::reset("entry_form_2")
    removeModal()
    
  })
  
  #添加报账信息
  editData_2 <- reactive({
    row <- input$responses_table_2_rows_selected+1
    row <- as.character(row)
    row <- paste0("M",row)
    return(row)
  })
  
  observeEvent(input$edit_button2_2, priority = 20,{
    if(length(input$responses_table_2_rows_selected)>=1 ){
      data <- data.frame("报账时间" = rep(as.character(format(Sys.Date(), format="%Y-%m")), 1))
      range_write(sheet_id, data, range = editData_2(),col_names = FALSE,sheet="data_2")
    }
    
    showModal(
      
      if(length(input$responses_table_2_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
  })
  
  #撤销报账信息
  observeEvent(input$re_edit_2, priority = 20,{
    if(length(input$responses_table_2_rows_selected)>=1 ){
      data <- data.frame("报账时间" = rep(as.character("未报账"), 1))
      range_write(sheet_id, data, range = editData_2(),col_names = FALSE,sheet="data_2")
    }
    
    showModal(
      
      if(length(input$responses_table_2_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
    
  })
  
  #delet data
  deleteData_2 <- reactive({
    row <- input$responses_table_2_rows_selected+1
    row <- as.character(row)
    return(row)
  })
  
  observeEvent(input$delete_button_2, priority = 20,{
    
    if(length(input$responses_table_2_rows_selected)>=1 ){
      range_delete(sheet_id, range = deleteData_2(),sheet='data_2')
    }
    
    showModal(
      
      if(length(input$responses_table_2_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
  })
  
  
  #edit database
  
  editData2_2 <- reactive({
    row <- input$responses_table_2_rows_selected+1
    row <- as.character(row)
    row <- paste0("A",row)
    return(row)
  })
  
  observeEvent(input$edit_button_2, priority = 20,{
    
    SQL_df2 <- read_sheet(sheet_id, sheet='data_2')
    
    showModal(
      if(length(input$responses_table_2_rows_selected) > 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select only one row." ),easyClose = TRUE)
      } else if(length(input$responses_table_2_rows_selected) < 1){
        modalDialog(
          title = "Warning",
          paste("Please select a row." ),easyClose = TRUE)
      })  
    
    if(length(input$responses_table_2_rows_selected) == 1 ){
      
      entry_form_2("submit_edit_2")
      
      row <- as.character(input$responses_table_2_rows_selected)
      
      updateTextInput(session, "A_2", value = as.character(SQL_df2[row, 1]))
      updateTextInput(session, "B_2", value = as.character(SQL_df2[row, 2]))
      updateTextInput(session, "C_2", value = as.character(SQL_df2[row, 3]))
      updateTextInput(session, "D_2", value = as.character(SQL_df2[row, 4]))
      updateTextInput(session, "E_2", value = as.character(SQL_df2[row, 5]))
      updateTextInput(session, "F0_2", value = as.character(SQL_df2[row,6]))
      updateTextInput(session, "G_2", value = as.character(SQL_df2[row, 7]))
      updateTextInput(session, "H_2", value = as.character(SQL_df2[row, 8]))
      updateTextInput(session, "I_2", value = as.character(SQL_df2[row, 9]))
      updateTextInput(session, "J_2", value = as.character(SQL_df2[row, 10]))
      updateTextInput(session, "K_2", value = as.character(SQL_df2[row, 11]))
      updateTextInput(session, "L_2", value = as.character(SQL_df2[row, 12]))
      updateTextInput(session, "M_2", value = as.character(SQL_df2[row, 13]))
      updateTextInput(session, "N_2", value = as.character(SQL_df2[row, 14]))
      
    }
    
  })
  
  #Add edit data
  observeEvent(input$submit_edit_2, priority = 20, {
    data2 <- data.frame("耗材" = input$A_2,
                       "货号" = input$B_2,
                       "生产商" = input$C_2,
                       "供货商" = input$D_2,
                       "联系人" = input$E_2,
                       "单价" = input$F0_2,
                       "规格" = input$G_2,
                       "数量" = input$H_2,
                       "总价" = input$I_2,
                       "采购日期" = input$J_2,
                       "采购人" = input$K_2,
                       "用途" = input$L_2,
                       "备注" = input$M_2,
                       "报账时间" = input$N_2)
    #range_write(sheet_id,data,range=editData2(),col_names = FALSE)
    #data <- data.frame("耗材" = rep(as.character("未报账"), 1))
    range_write(sheet_id, data2, range = editData2_2(),col_names = FALSE,sheet='data_2')
    shinyjs::reset("entry_form_2")
    removeModal()
    
  })
  
  
  #Displaying the Data Table
  output$responses_table_2 <- DT::renderDataTable({
    df <- responses_df_2()
    #df <- df[,]
    datatable(df,selection = 'single',height="auto",width="100%")
  })
  
  #校内转账----
  responses_df_3 <- reactive({
    
    input$submit_3
    input$submit_edit_3
    input$edit_button2_3
    input$re_edit_3
    input$delete_button_3
    read_sheet(sheet_id, "data_3")
    
  }) 
  
  #Function for the entry form that will pop-up in a model dialog when the Add and Edit buttons are pressed
  entry_form_3 <- function(button_id_3){
    
    showModal(
      modalDialog(
        div(id=("entry_form_3"),
            tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c("250px", "100px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("A_3", "事项", placeholder = "")
                ),
                textInput("B_3", "金额", placeholder = ""),
                textInput("C_3", "收款单位", placeholder = ""),
                textInput("D_3", "经办人", placeholder = ""),
                textInput("E_3", "时间", placeholder = ""),
                textAreaInput("F_3", "备注", placeholder = "", height = 40, width = "354px"),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id_3, "Submit")
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  #add data
  formData_3 <- reactive({
    
    formData <- data.frame("事项" = input$A_3,
                           "金额" = input$B_3,
                           "收款单位" = input$C_3,
                           "经办人" = input$D_3,
                           "时间" = input$E_3,
                           "单价" = input$F_3)
    return(formData)
  })
  
  observeEvent(input$add_button_3, priority = 20,{
    
    entry_form_3("submit_3")
    
  })
  observeEvent(input$submit_3, priority = 20,{
    
    sheet_append(sheet_id,formData_3(),sheet='data_3')   #run formData function
    shinyjs::reset("entry_form_3")
    removeModal()
    
  })
  
  
  #delet data
  deleteData_3 <- reactive({
    row <- input$responses_table_3_rows_selected+1
    row <- as.character(row)
    return(row)
  })
  
  observeEvent(input$delete_button_3, priority = 20,{
    
    if(length(input$responses_table_3_rows_selected)>=1 ){
      range_delete(sheet_id, range = deleteData_3(),sheet='data_3')
    }
    
    showModal(
      
      if(length(input$responses_table_3_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
  })
  
  
  #Displaying the Data Table
  output$responses_table_3 <- DT::renderDataTable({
    df <- responses_df_3()
    #df <- df[,]
    datatable(df,selection = 'single',height="auto",width="100%")
  })
}

shinyApp(ui, server)