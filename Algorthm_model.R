
# PACKAGES USED IN THE PROJECT

library(shinycssloaders)
library(shiny)
library(tibbletime)
library(shinydashboard)
library(shinyWidgets) ##selection of Branches
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(eeptools)
library(lubridate)
library(anytime)
library(data.table)
library(ggvis)
library(stringr)
library(DT)
library(rmarkdown)
library(stringi)
library(purrr)
library(sjPlot)
library(RODBC)

###----Pswd---- Packages

library(shinyauthr)
library(shinyjs)
library(glue)
library(scales)
library(shinyauthr)
library(rAmCharts)
library(plotly)
library(dygraphs)
library(xts)
library(sodium)
library(shinydashboardPlus)


#### HEAD OF THE PROJECT####

#source('C:/Users/Murera Gisa/Desktop/AFR_Dash/pass.R', local =  TRUE)


user_base <- data_frame(
  user = c("vbucyensenge", "eiryivuze","cmunazilikazi", "jampulire", "mgisa"),
  password = c("pass1234", "pass1234","pass1234", "pass1234", "pass1234"), 
  password_hash = sapply(c("pass1234", "pass1234","pass1234", "pass1234", "pass1234"), sodium::password_store), 
  permissions = c("admin", "standard", "standard", "standard", "standard"),
  name = c("vedaste BUCYENSENGE","ERNESTE IRYIVUZE", "Claire MUNAZILIKAZI", "Janine AMPULIRE", "Murera Gisa")
)
# First dataset

weather_station<- read.csv(file = 'C:/Users/Murera Gisa/Desktop/AFR_Dash/Summary of weather.csv', header =  TRUE)
weather_station$STATION<- as.character(weather_station$STATION)
weather_station <- unique(weather_station)
#str(weather_station)
weather_station$DATE <-format(as.Date(weather_station$DATE, format= "%d/%m/%Y"), "%Y-%m-%d")

# Creating uniqueness of the data
# Arrange the date as yy-mm-dd
#weather_station$DA <- as.numeric(weather_station$DA)      # Change the data into numeric form
#weather_station[is.na(weather_station)]<- 0               # Removing nan with 0 under replacement

# Second dataset

df_oper  <- read.csv(file = 'C:/Users/Murera Gisa/Desktop/AFR_Dash/Operation.csv')

df_bost <- read.csv(file = "C:/Users/Murera Gisa/Desktop/AFR_Dash/Boston_data.csv")
df_bost$DATE<- strptime(as.character(df_bost$DATE), "%d/%m/%Y")
df_bost$DATE<-format(df_bost$DATE,"%Y-%m-%d")

df_oper$Date<- anydate(df_oper$Date)
#str(df_oper)

#df_oper$Date <- format(anydate(df_oper$Date,format= "%Y-%m-%d"), "%Y-%m-%d" ) 
df_oper[is.na(df_oper)]<- 0           # Removing nan with 0 under replacement
df_oper <- unique(df_oper)            # Creating uniqueness of the data
df_oper$Mission.ID<- as.character(df_oper$Mission.ID)
df_oper$High.Explosives.Weight..Tons.<-as.integer(df_oper$High.Explosives.Weight..Tons.)
# Creating timezone system

#df_bost1<- unique(df_bost1)

# Third Dataset



#amizero_data<-read.csv(file =  "C:/Users/Murera Gisa/Desktop/AFR_Dash/amizero.csv")
#amizero_data[is.na(amizero_data)]<- 0 # Removing nas

#amizero_data<- as.Date(amizero_data,"%m/%d/%Y")



# Adding Image or Logo along with the title 


header <- dashboardHeader(title = 'My|Dashboard',titleWidth = 230,
                          tags$li(a(img(src = 'AFR.png',height = "35px",width = "180px",align = "center"),
                                    style = "padding-top:2px; padding-bottom:1px;"),
                                  class = "dropdown"),
                          
                          dropdownMenu(type="messages",badgeStatus = "success",
                                       messageItem(from = "Dashboard", message = "Under Development ")),
                          
                          tags$li(class = "dropdown", style = "padding: 9px;",
                                  shinyauthr::logoutUI("logout")),
                          
                          dropdownMenu(type="messages",badgeStatus = "info",
                                       messageItem(from = "New User", message = "How do I register?",
                                                   icon = icon("question"),
                                                   time = "13:45")),
                          tags$li(class = "dropdown", style = "padding: 9px;",
                                  shinyauthr::logoutUI("logout")),
                          
                          
                          dropdownMenu(type="messages",badgeStatus = "success",
                                       messageItem(from = "Support", message = "Any support , do ask!", 
                                                   icon = icon("life-ring"),
                                                   time = "2020-04-01")),
                          
                          tags$li(class = "dropdown", style = "padding: 9px;",
                                  shinyauthr::logoutUI("logout")),
                          
                          
                          tags$li(class = "dropdown", style = "padding: 9px;",
                                  shinyauthr::logoutUI("logout"))
                          
                          )

## *******SELECTING BRANCH OPTION SETTING*******


background<-pickerInput("countryInput", "Locations",choices=unique(weather_station$STATION),
                        multiple = T ,options = list(`actions-box` = TRUE, `live-search` = TRUE, 
                                                                                                                         `selected-text-format`= "static", title = "Select Identity"),
                        choicesOpt = list(
                          style = rep(("color: black; background: white; font-weight: bold;"),94)))
background1<-pickerInput("countryInput1", "Locations",choices=unique(df_bost$Average_data),multiple = T ,options = list(`actions-box` = TRUE, `live-search` = TRUE, 
                                                                                                                      `selected-text-format`= "static", title = "Select Meandata"),
                         choicesOpt = list(
                           style = rep(("color: black; background:white; font-weight: bold;"),90)))

cer<-conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                      tags$img(src="loading_circle.gif")
)


#**** Now we are going to display validation error in red color*****

TT<-fluidPage(
  
  tags$head( 
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    "))
  ),
  
  ##***  ENDING COLOR VALIDATION MESSAGE ********* #
  
  verbatimTextOutput("server"),
  tags$script('
              $(document).ready(function(){
              var d = new Date();
              var target = $("#clientTime");
              target.val(d.toLocaleString());
              target.trigger("change");
              });
              '),
  textInput("clientTime", "", value = ""),
  verbatimTextOutput("local")
)


#****** Date range Setting******


date_range <-dateRangeInput(inputId = "dates",
                            #label = em("Select time period",style="text-align:center;color:#FFA319;font-size:100%"),
                            label = "Select time period:",
                            start = min("1940-01-01"),
                            end =  max("2024-01-01"),
                            min =  min("1940-01-01"),
                            max =  max("2024-01-01"),
                            format = "yyyy-mm-dd")


#****************************LAYOUT SETUP***********************************


sidebar <- dashboardSidebar(
  collapsed = TRUE,
  div(sidebarUserPanel(textOutput("welcome1"),subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"))),
  sidebarMenu(
    
    date_range,
    
    background,
    
    background1,
    
    menuItem("Home",tabName = "main12", icon = icon("home")),
    
    menuItem("PROJECTS", icon = icon("project"), 
             
             menuSubItem(
               "Overview", tabName = "cust1", icon = icon("pie-chart")),
             
             menuSubItem(
               "Underserved and Vulnerable Segments", tabName = "cust2", icon = icon("group")),
             
             menuSubItem(
               "Agriculture Finance", tabName = "cust4", icon = icon("group")),
             
             menuSubItem(
               "Digital Financial Services", tabName = "cust5", icon = icon("credit-card")),
             
             menuSubItem(
               "Market Dvpt& Facilitation", tabName = "cust6", icon = icon("line-chart")),
             
             menuSubItem(
               "Communication ", tabName = "cust7", icon = icon("list-alt")),
             
             menuSubItem(
               "Monitoring & Evaluation ", tabName = "cust8", icon = icon("line-chart")),
             
             menuSubItem(
               "Research and Policy ", tabName = "cust3d", icon = icon("th")),
             
             menuSubItem(
               "Risk Mitigation(Insurance,LTSS) ", tabName = "cust3", icon = icon("th"))),
    
    menuItem("DATA MANAGEMENT", icon = icon("report"),
             menuSubItem(
               "Overview", tabName = "custA", icon = icon("pie-chart")),
             
    menuItem("Data Validation", icon = icon("database"),
                      menuSubItem(
                        "Overview", tabName = "loan2", icon = icon("line-chart")),
             
             menuSubItem(
               "Tables", tabName = "custB", icon = icon("list-alt")),
             
             menuSubItem(
               "KRAs summary", tabName = "loan5", icon = icon("th")),
             
             menuSubItem(
               " Trends", tabName = "custC", icon = icon("list-alt"))),
    
    
   menuItem("Visit-AFR Website", icon = icon("send",lib='glyphicon'), 
             href = "https://www.afr.rw"),
   TT
  )
  
))
                                
        ####*********************Home contents###*******************************

# **********************Setups **************************************##


body <- dashboardBody(
  tags$head(tags$style(HTML('
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color:#024cfa;
                            }
                            
                            /* logo when hovered */
                            .skin-blue .main-header .logo:hover {
                            background-color:#0b02f5;
                            }
                            
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #05ab31;
                            }
                            
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #0d0d0c;
                            }
                            
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #f7e6e6;
                            }
                            
                            /* other links in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #1d00fc;
                            color: #000000;
                            }
                            
                            
                            /* other links in the sidebarmenu when hovered */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: #967d8a;
                            }
                            /* toggle button when hovered  */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #f58cc0;
                            }
                            
                            /* body */
                            .content-wrapper, .right-side {
                            background-color: #2f8a44;
                            }
                            
                            '))),
  shinyjs::useShinyjs(),
  tags$head(tags$style(".table{margin: 0 auto;}"),
            tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                        type="text/javascript"),
            includeScript("returnClick.js")
  ),
  shinyauthr::loginUI("login"),
  uiOutput("user_table"),
  HTML('<div data-iframe-height></div>'),
  tabItems(
    tabItem(tabName = "main12", uiOutput("bodyHome")),
    
    tabItem(tabName = "cust1",box(textOutput("dater1"), width = 13, status = "warning", background = "yellow"), uiOutput("frow33"), uiOutput("frow3"), uiOutput("frow1")),
    tabItem(tabName = "cust2",box(textOutput("dater3"), width = 13, status = "warning", background = "yellow"), uiOutput("frow44"),uiOutput("frow4"), uiOutput("frow2")),
    tabItem(tabName = "cust3",box(textOutput("dater4"), width = 13, status = "warning", background = "yellow"), uiOutput("frow55"),uiOutput("frow5"), uiOutput("frow6")),
    tabItem(tabName = "cust4",box(textOutput("dater5"), width = 13, status = "warning", background = "yellow"),uiOutput("frow77"),uiOutput("frow7"), uiOutput("frow9")),
    tabItem(tabName = "cust5",box(textOutput("dater6"), width = 13, status = "warning", background = "yellow"),uiOutput("frow88"),uiOutput("frow8"), uiOutput("frow10")),
    tabItem(tabName = "cust6",box(textOutput("dater7"), width = 13, status = "warning", background = "yellow"),uiOutput("frow9ii"),uiOutput("frow9i"), uiOutput("frow6a")),
    tabItem(tabName = "cust7",box(textOutput("dater8"), width = 13, status = "warning", background = "yellow"),uiOutput("frow10ii"),uiOutput("frow10i"), uiOutput("frow9a")),
    tabItem(tabName = "cust3d",box(textOutput("dater9"), width = 13, status = "warning", background = "yellow"),uiOutput("frow55v"),uiOutput("frow5v"), uiOutput("frow6n")),
    tabItem(tabName = "cust8",box(textOutput("dater10"), width = 13, status = "warning", background = "yellow"),uiOutput("frow11i"),uiOutput("frow11"), uiOutput("frow10a")),
    tabItem(tabName = "custA",box(textOutput("dater11"), width = 13, status = "warning", background = "yellow"),uiOutput("frow33e"),uiOutput("frow3e"), uiOutput("frow1e")),
    tabItem(tabName = "custB",box(textOutput("dater12"), width = 13, status = "warning", background = "yellow"),uiOutput("frow44e"),uiOutput("frow4e"), uiOutput("frow2e")),
    
    
   
    tabItem(tabName = "loan2", box(textOutput("dater13"), width = 13, status = "warning", background = "yellow"),uiOutput("frowd"),uiOutput("frowe"),uiOutput("frobb1"),uiOutput("frowb"),uiOutput("frob1"),uiOutput("frob2")),
    tabItem(tabName = "loan5",p("The table shows loan summary smallholders farmers.Everyone  can change column visibility, downloading table and also the data are automated to be printed for any options you need.The data should be downloaded through , XTML,CSV, EXCEL,.."),uiOutput("pro11")),
    tabItem(tabName = "custC",uiOutput("frow5e"),uiOutput("frow66e"), uiOutput("frow6e"))
    
  ))

ui <-dashboardPage(header, sidebar,body, skin='blue')


#******Now have to create the server functions for the dashboard *******


server <- function(input, output, session) {
  
  #********************PASWORD FROM HERE***************
  
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  user_info <- reactive({credentials()$info})
  
  user_data <- reactive({
    req(credentials()$user_auth)
    
    if (user_info()$permissions == "admin") {
      dplyr::starwars[,1:10]
    } else if (user_info()$permissions == "standard") {
      dplyr::storms[,1:11]
    }
    
  })
  
  output$welcome1 <- renderText({
    req(credentials()$user_auth)
    
    glue("Welcome {user_info()$name}")
  })
  
  #-------------------------------------data setup----------------------------------------
  
  ###########################################################################END PASSWORD###############
  
  ################# Plots and data setups###############################################################
  
  DATA1<- reactive({
    validate(
      need(input$countryInput!= "", message="Please select Serial Number")
    )
    weather_station %>%filter(DATE >= input$dates[1],DATE<= input$dates[2],STATION%in% input$countryInput)})
  
  
  
  DATA2 <- reactive({
    validate(
      need(input$countryInput1!= "", message="Please select Number")
    )
    df_bost%>%filter(DATE >= input$dates[1], DATE<= input$dates[2],Average_data%in% input$countryInput1)})
  
  
  #***************BODYHOME ******************************
  
  output$bodyHome<- renderUI({
    req(credentials()$user_auth)
    tabItem(tabName = "main12", value="main_panel",
            fluidRow(
              box(
                title = HTML("<font color=\"#00b3FF\"><b>Welcome to the AFR Dashboard</b></font>"), width = 16, status = "primary",
                
                HTML("The availability of a <b>dashboard for Access to Finance Rwanda</b> and <b>Management of projects/Supports </b> will play a significant role in providing at-a-glance views of the <i>AFR's Poverty reduction </i> and provide insights through automated analytics and reports.
                     ")
              )
            ),
            
            
            fluidRow(
              box(
                title = HTML("<b>Benefits</b>"),width = 4, status = "warning",
                tags$ol(
                  tags$li("SACCO development"), 
                  tags$li("Agriculture Finance"), 
                  tags$li("Digital finance solutions"),
                  tags$li("Inclusive Insurance"),
                  tags$li("Informal Sector Pensions")
                ),
                img(src='AFR2.JPG', align = "center", width="100%"),
                img(src='farmer1.jpg', align = "center", width="100%")
              ),
              box(
                title = HTML("<b>Insights</b>"), width = 4,status = "warning",
                p("The dashboard gives insights of AFR for the  development of sustainable improvements in the livelihoods of poor people, 
                through reduced vulnerability to shocks, increased incomes and employment creation.
                  Also , AFR's strategic focus is stimulating financial sector development by partnering with financial institutions and
                  other stakeholders to increase access to and use of financial services"),
                img(src='market.JPG', align = "center", width="100%"),
                img(src='tea.jpg', align = "center", width="100%")
              ),
              
              box(
                title = HTML("<b> About this dashboard?</b>"), width = 4,status = "warning",
                p(" In order to be familiar with the dashboard, you need to select",span("Financial Sector", style = "color:blue"), "and" 
                  ,span("at least one", style = "color:blue"),"AFR website." ," events and workshops.What is more, AFR identify 
                  and address constraints that prevent the financial market from reaching Rwanda's low-income population, 
                  and promote innovations and learning that result in sustainable change in the financial sector."),
                img(src='fff.jpg', align = "center", width="100%"),
                img(src='woman.jpg', align = "center", width="100%")
                
              )
            ),
            fluidRow(
              box(
                title = "Vedaste at AFR (vedaste@afr.rw)", width = 12, status = "success",
                h4("This dashboard is designed by a Data scientist under MERL."),
                img(src='aims.jpg', align = "center", width="25%")
                
              )
            ),
            
            
           )
         })
  
  #--------------------------- outputs----------------------------------------------------
  
  valueBox2 <- function (value, subtitle, icon = NULL, backgroundColor = "#7cb5ec", textColor = "#FFF", width = 4, href = NULL)
  {
    
    boxContent <- div(
      class = paste0("small-box"),
      style = paste0("background-color: ", backgroundColor, "; color: ", textColor, ";"),
      div(
        class = "inner",
        h3(value),
        p(subtitle)
      ),
      if (!is.null(icon)) {
        div(class = "icon-large", icon)
      }
    )
    if (!is.null(href)) {
      boxContent <- a(href = href, boxContent)
    }
    div(
      class = if (!is.null(width)) paste0("col-sm-", width),
      boxContent
    )
  }
  
  
  #***************dashboard building starts here*************************
  
  output$frow1<- renderUI({
    req(credentials()$user_auth)
    
    fluidRow(
      valueBoxOutput("boxx1",width = 5),
      valueBoxOutput("boxx2",width = 5),
       valueBoxOutput("Country"),
      valueBoxOutput("Air.Force"),
      valueBoxOutput("Aircraft.Series"),
      style = "background-color: white"
      
    )
  
    
  }) 
  
  output$frow10<- renderUI({
    req(credentials()$user_auth)
    
    fluidRow(
      
    p("The user can select the view line(es) by click on an unwanted lines data in legend, the date options ( one year, two years and 
      max for entire period) are available down bar of the chart. The data is available in download options at top right corner of the chart."),
    box(title = 'Balance value in the saving accounts', width =  12, status = "primary", solidHeader = TRUE , collapsible = TRUE,
        amChartsOutput("Date")%>% withSpinner(color = "#02fafa"), style = "height: 500px;"))
    
    
  }) 
  
  output$frow9a<- renderUI({
    req(credentials()$user_auth)
    fluidRow(
      box(
        title = "Financial services"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE
        ,plotOutput("pie11", height = "240px", width="250px")%>% withSpinner(color="#02fafa"),
        p("Country")
      ))
    
    
    })
  
  
  
  
  output$boxx1 <- renderValueBox({
    out <- DATA1()
    valueBox2(
      format(round(sum(out$MEAN_TEMPERATURE), 0),scientific = F, format="G", big.mark = ","),
      subtitle = " Total #of Precipitation",
      icon = icon("thumbs-up"),
      backgroundColor = "#cdff05"
    )
  })
  
  
  
  output$boxx2 <- renderValueBox({
    out <- DATA2()
    valueBox2(
      format(round(sum(out$Average_data), 0),scientific = F, format="fg", big.mark = ","),
      subtitle = "Total average # of data",
      icon = icon("thumbs-up"),
      backgroundColor = "#cdff05"
    )
  })  
  
  
  
  output$pro11<- renderUI({
    req(credentials()$user_auth)
    tabsetPanel(
      #p("The tables captures counts of type, gender, account status and nationality of customers. The data is downloadable and one can change visibilty from option icons"),
      tabPanel("NAME OF SACCO", dataTableOutput("NEW")),
      tabPanel("GENDER", dataTableOutput("NEW1")),
      tabPanel("ACCOUNTS OPENED", dataTableOutput("NEW2")),
      tabPanel("# OF NEW LOANS", dataTableOutput("NEW3")),
      tabPanel("LOANS DISBURSED", dataTableOutput("New4")),
      tabPanel("OUTSTANDING'S AMNT", amChartsOutput("New5")),
      tabPanel("DEPOSITORS", plotOutput("pie11")),
      tabPanel("DEPOSITOR'S AMNT", plotOutput("pie33"))
      
    )}) 
  
  
  
  
  # output$dater <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  # 
  # output$dater1 <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  # 
  # output$dater2 <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  # 
  # output$dater3 <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  # 
  # output$dater4 <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  # 
  # output$dater5 <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  # 
  # output$dater6 <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  # 
  # output$dater7 <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  # 
  # output$dater8 <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  # 
  # output$dater9 <- renderText({
  #   paste0(
  #     "The results for the seleted branch(es) from ",
  #     input$dates[1],
  #     " to ",
  #     input$dates[2]
  #   )
  # })
  
  
  
}


shinyApp(ui, server)
