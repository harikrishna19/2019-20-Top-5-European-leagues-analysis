### final application for the project
### 2019/20 TOP 5 European league analysis (EDA and Ranking)




#importing required libraries

library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(shinythemes)
library(radarchart)
library(shinyjs)
library(shinycssloaders)
library(dplyr)
library(png)
library(dplyr)
library(ggrepel)
library(png)
library(DT)


#loading ui file
dset=read.csv("data/dB_FIFA.csv")

###########
# LOAD UI #
###########

shinyUI(fluidPage(theme=shinytheme("simplex"),
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    
    skin = "black",
    
    dashboardHeader(title="2019/20 Top 5 European Leagues Analysis", titleWidth = 500),
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<a href='https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcT5YHow_ZN43vC7yA1AOjLniewb56qsPf2_Aw&usqp=CAU' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcT5YHow_ZN43vC7yA1AOjLniewb56qsPf2_Aw&usqp=CAU' width = '186'></a>",
                         "<br>",
                         "<p style = 'text-align: center;'><small><a href='https://www.uefa.com/' target='_blank'>UEFA</a></small></p>",
                         "<br>"
                       )),
                    
                       menuItem("Home", tabName = "Home", icon = icon("home")), 
                       menuItem("League Analysis", tabName = "tab_leagues", icon = icon("balance-scale")),
                       menuItem("Player Analysis", tabName = "tab_players", icon = icon("futbol")),
                       menuItem("Ranking", tabName = "tab_ranking", icon = icon("chart-line")),
                       menuItem("Transfer-Mkrt Analysis", tabName = "tab_loan", icon = icon("donate")),
                                   
  
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "<tr>",

                         "<td style='padding: 5px;'><a href='https://www.linkedin.com/in/hari-krishna-a05b44187/' target='_blank'><i class='fab fa-linkedin fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.twitter.com/Hakrishna19' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://github.com/harikrishna19' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>",
                         "</tr>",
                         "</table>",
                         "<br>"),
                         HTML(paste0(
                           "<script>",
                           "var today = new Date();",
                           "var yyyy = today.getFullYear();",
                           "</script>",
                           "<p style = 'text-align: center;'><small>&copy; - <text='krishna19599@gmail.com' target='_blank'>krishna19599@gmail.com</a> - <script>document.write(yyyy);</script></small></p>")
                         ))
                     )
                     
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        
        tabItem(tabName = "tab_players",
                
                column(
                  width = 12,
                  
                  box(
                    title = tagList(icon("bullseye"), "Player Stats"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,background = 'lime',
                    
                    tags$hr(),
                    
                    fluidRow(
                      
                      column(
                        width = 2,
                        boxPad(
                          color = "red",
                          h6("Player 1")),
                        selectizeInput("tp_league", "League:",choices=NULL,selected='NONE'),
                        selectizeInput("tp_team", "Team:",choices=NULL,selected='NONE'),
                        selectizeInput("tp_player", "Player:", choices =NULL,selected='NONE')
                        
                        
                      ),
                      
                      column(
                        width = 8,
                        
                        fluidRow(
                          column(width = 4),
                          column(width = 2, uiOutput("PlayerImg")), 
                          column(width = 2,offset = 1, uiOutput("PlayerImg2")),
                          column(width = 4)
                        ),
                        
                        br(),
                        conditionalPanel(condition = "output.PlayerImg",hr()),
                        
                        column(offset = 1, width = 12,
                               fluidRow(
                                 valueBoxOutput("tp_age",width = 3),
                                 valueBoxOutput("tp_overall",width = 2),
                                 valueBoxOutput("tp_value",width = 3),
                                 valueBoxOutput("tp_contract",width = 3)
                               ),
                               
                               fluidRow(
                                 valueBoxOutput("tp_age2",width = 3),
                                 valueBoxOutput("tp_overall2",width = 2),
                                 valueBoxOutput("tp_value2",width = 3),
                                 valueBoxOutput("tp_contract2",width = 3)
                               )
                        )
                        
                      ),
                      
                      column(
                        width = 2,
                        boxPad(
                          h6("Player 2"),
                          color = "red"),
                        selectizeInput("tp_league2", "League:", choices =NULL,selected='NONE'),
                        selectizeInput("tp_team2", "Team:", choices =NULL,selected='NONE'),
                        selectizeInput("tp_player2", "Player:", choices =NULL,selected='NONE')
                      )
                      
                    ),
                    
                    tags$hr(),
                    tags$br(),
                    
                    conditionalPanel(
                      condition = "exists",
                      
                      column(
                        width = 12,
                        
                        tabsetPanel(type = "pills",
                                    
                                    tabPanel("Radar",
                                             br(), withSpinner(plotlyOutput("tp_radar"))
                                             
                                             
                                             
                                    )
                        )
                      )
                    )
                    
                    
                  ))
  
                
        ),
        
        tabItem(
          # species data section
          tabName = "Home",
           # class = "active" argümanı açılış sayfası eklediğimiz için ilk ekran boş gözüktüğünden eklendi
          
          column(
            width = 12,
            
            fluidRow(
              
              # Welcome Box
              box(title = "2019/20 Analysis", status = "success", width = NULL, solidHeader = TRUE, collapsible = FALSE,background ="black",
                  
                  # Ronaldo FIFA 19 Image
                  column(width = 8, tags$img(src="https://www.wallpapertip.com/wmimgs/53-532185_champions-league-wallpaper-designed-by-joeri-gosens-best.png", align="center", style="width: 100%; height: 75%;")),
                  
                  # Description
                  column(width = 4, 
                         tags$img(src="pl.png", style = "height: 70%; width: 14%;margin-left: 10%;"),
                         tags$img(src="laliga.png", style = "height: 80%; width: 14%; margin-left: 10px;"),
                         tags$img(src="bundesliga.png", style = "height: 80%; width: 14%; margin-left: 10px;"),
                         tags$img(src = "seriea.png", style = "height: 80%; width: 14%; margin-left: 5px;"),
                         tags$img(src = "ligue1.png", style = "height: 80%; width: 14%; margin-left: 5px;"),
                         tags$br(), tags$br(),
                         tags$hr(),
                         fluidRow(column(width = 4), 
                                  column(width = 4, tags$img(src="respectuefa.png", style = "height: 5%; width: 100%")),
                                  column(width = 4)),
                         tags$br(),
                         tags$hr(),
                         tags$p("The dashboard which I have designed involves all the analysis related to 2019/20 Top 5  European leagues.")
                  )
              )
            )
          )
          
          
          
          
          

          
        ),
        
        tabItem(tabName = "tab_loan",
                
                column(
                  width = 12,
                  
                  box(
                    title = tagList(icon("line-chart"), "Team Stats"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,background = "black",
                    
                    tags$hr(),
                    
                    fluidRow(
                      
                      column(
                        width = 2,
                        selectInput("tt_league", "League:", choices = unique(club_record$league)),
                       actionBttn(inputId = "tt_select", tagList(icon("location-arrow"),"Select"), 
                                            size = "sm", color = "success", style = "simple", block = T)
                      ),
                      
                      column(width = 10,
                             tags$img(src="pl.png", style = "height: 70%; width: 10%;margin-left: 15%;"),
                             tags$img(src="laliga.png", style = "height: 80%; width: 12%; margin-left: 10px;"),
                             tags$img(src="bundesliga.png", style = "height: 10%; width: 6%; margin-left: 10px;"),
                             tags$img(src = "seriea.png", style = "height: 10%; width: 4.8%; margin-left: 10px;"),
                             tags$img(src = "ligue1.png", style = "height: 20%; width: 4.5%; margin-left: 10px;")
                      )
                      
                    ),
                    
                    tags$br(),
                    tags$hr(),
                    tags$br(),
                    
                    column(
                      width = 12,
                      
                      conditionalPanel(condition = "input.tt_select",
                                       
                                       tabsetPanel(type = "pills",
                                                   
                                                   tabPanel("Spending",
                                                            br(),
                                                            withSpinner(plotOutput("tt_summary",height = "700px"))),
                                                   
                                                   tabPanel("Profit", 
                                                            withSpinner(plotOutput("tt_profit",height = "700px")))
                                       )
                      )
                    )
                    
                  )
                  
                  
                  
                  
                )    
                
                
                
                
                
                ),
        tabItem(tabName="tab_ranking",
               
                tagList(
                  useShinyjs(),
                  inlineCSS(
                    "
    #loading-content {
    position: absolute;
    background: #FFFFFF;
    opacity: 0.9;
    z-index: 100;
    left: 0;
    right: 0;
    height: 100%;
    text-align: center;
    }
    "
                  ),
                  div(
                    id = "loading-content",
                    h2("Loading...")
                  ),
                  hidden(
                    div(
                      id = "app-content",
                      navbarPage(
                        "Multiple Criteria Decision Making",
                        theme = shinytheme("simplex"),
                        tabsetPanel(
                          tabPanel(
                            "Model",
                            useShinyjs(),
                            fluidRow(
                              column(2, actionButton("showUpload", "Load", icon = icon("upload"))),
                              column(2, downloadButton('downloadFile', 'Save'))
                            ),
                            fluidRow(uiOutput("uploadFileOutput")),
                            br(),
                            fluidRow(aceEditor("ace", mode = "yaml", theme = "clouds", value = "define ahp model here"))
                            
                            
                          ),
                          
                          tabPanel(
                            "Visualize", 
                            grVizOutput("visualizeTree"),
                            value = "visualizePanel"
                          ),
                          
                          tabPanel(
                            "Analyze", 
                            
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons(
                                  inputId = "ahpmethod", 
                                  label = "AHP Priority Calculation Method: ", 
                                  choices = c("Eigenvalues", "Mean of Normalized Values", "Geometric Mean"),
                                  selected = "Eigenvalues"
                                ),
                                
                                radioButtons(
                                  inputId = "sort", 
                                  label = "Sort Order: ", 
                                  choices = c("Total Priority", "Priority", "Original"),
                                  selected = "Total Priority"
                                ),
                                
                                radioButtons(
                                  inputId = "variable", 
                                  label = "Variable: ", 
                                  choices = c("Total Contribution", "Priority", "Score"),
                                  selected = "Total Contribution"
                                ),
                                
                                uiOutput("decisionMaker"),
                                textInput(inputId = "cutoff", label = "Filter by weight contribution: ", value = "0"),
                                textInput(inputId = "level", label = "Filter n levels: ", value = "0")
                              ),
                              mainPanel(formattableOutput("table"))
                              
                            ),
                            value = "analysis"
                          ),
                          #position = "fixed-top",
                          #tags$style(type="text/css", "body {padding-top: 70px;}"),
                          id = "navbar",
                          
                          tabPanel(
                            "Data",
                            icon = icon("database"),
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons(
                                  "source",
                                  "Select source of data",
                                  choices = c(
                                    "Example dataset" = "example",
                                    "Upload dataset" = "upload"
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.source == 'example'",
                                  helpText("This is a fabricated dataset")
                                ),
                                conditionalPanel(
                                  condition = "input.source == 'upload'",
                                  fileInput(
                                    "upload_data",
                                    "Please upload your data",
                                    multiple = FALSE,
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"
                                    )
                                  ),
                                  helpText("File should be in .csv format")
                                ),
                                actionButton(
                                  "use",
                                  "Use dataset"
                                ),
                                conditionalPanel(
                                  condition = "input.use",
                                  br(),
                                selectizeInput(
                                    "alternative",
                                    "Select column containing alternatives",
                                    choices = NULL
                                  ),
                                 selectizeInput(
                                    "attribute_max",
                                    "'The higher, the better' attribute(s)",
                                    choices = NULL,
                                    multiple = TRUE,
                                    options = list(
                                      "live-search" = TRUE,
                                      "actions-box" = TRUE
                                    )
                                  ),
                                  selectizeInput(
                                    "attribute_min",
                                    "The lower, the better' attribute(s)",
                                    choices = NULL,
                                    multiple = TRUE,
                                    options = list(
                                      "live-search" = TRUE,
                                      "actions-box" = TRUE
                                    )
                                  ),
                                  actionButton(
                                    "arrange",
                                    "Arrange dataset"
                                  )
                                )
                              ),
                              mainPanel(
                                withSpinner(DT::dataTableOutput("tab_dataset"), type = 5, color = "#34495e")
                              )
                            )
                          ),
                          tabPanel(
                            "Analysis",
                            icon = icon("line-chart"),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("setting_panel"),
                                selectizeInput(
                                  "method",
                                  "Method",
                                  choices = c(
                      
                                    "TOPSIS Linear" = "TOPSISLinear",
                                    "TOPSIS Vector" = "TOPSISVector"
                          
                                  ),
                                  selected = "TOPSISVector"
                                ),
                              
                                actionButton(
                                  "apply",
                                  "Apply"
                                )
                              ),
                              mainPanel(
                                withSpinner(DT::dataTableOutput("tab_res"), type = 5, color = "#34495e")
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )  
                
                
                
                
                 ),

  
        
        tabItem(tabName = "tab_leagues",
                
                column(
                  width = 12,
                  
                  box(
                    title = tagList(icon("bullseye"), "League Stats"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,background = "black",
                    
                    tags$hr(),
                    
                    fluidRow(
                      
                      column(
                        width = 2,
                        selectInput("tl_league", "League:", choices = unique(dset$League)),
                        actionBttn(inputId = "tl_select", tagList(icon("location-arrow"),"Select"), 
                                            size = "sm", color = "success", style = "simple", block = T)
                      ),
                      
                      column(width = 10,
                             tags$img(src="pl.png", style = "height: 70%; width: 10%;margin-left: 15%;"),
                             tags$img(src="laliga.png", style = "height: 80%; width: 12%; margin-left: 10px;"),
                             tags$img(src="bundesliga.png", style = "height: 10%; width: 6%; margin-left: 10px;"),
                             tags$img(src = "seriea.png", style = "height: 10%; width: 4.8%; margin-left: 10px;"),
                             tags$img(src = "ligue1.png", style = "height: 20%; width: 4.5%; margin-left: 10px;")
                      )
                      
                    ),
                    
                    tags$br(),
                    tags$hr(),
                    tags$br(),
                    
                    column(
                      width = 12,
                      
                      conditionalPanel(condition = "input$tl_select",
                                       
                                       fluidRow(
                                         valueBoxOutput("values"),
                                         valueBoxOutput("numofplayers"),
                                         valueBoxOutput("teams")
                                       ),
                                       
                                       br(),
                                       
                                       fluidRow(
                                         
                                         conditionalPanel(
                                           condition = "input$tl_select",
                                           
                                           column(width = 6,
                                                  div(style="clear:both;  margin-left: 20px;"),
                                                  withSpinner(plotOutput("comp_plot"))
                                                  
                                           ),
                                           
                                           column(width = 6,
                                                  
                                                  tabsetPanel(
                                                    
                                                    tabPanel("Nationality",
                                                             br(),
                                                             withSpinner(
                                                               plotlyOutput(outputId = "league_nat1"))
                                                    ),
                                                    
                                                    tabPanel("Players", 
                                                             br(),
                                                             withSpinner(
                                                               plotOutput(outputId = "league_values", height = "100%")
                                                             )
                                                    ),
                                                    
                                                    tabPanel("Comparison", 
                                                             
                                                             br(),
                                                             
                                                             fluidRow(
                                                               column(width = 6, selectInput("comp_league",'League', choices = c("League", "Team", "Position","Transfer"))),
                                                               column(width = 6, selectInput("comp_graph",'Graph', choices = "Bar"))
                                                             ),
                                                             
                                                             br(), withSpinner(plotOutput("league_comp1"))
                                                    )
                                                  )
                                           )
                                         )
                                         
                                         
                                         
                                       )
                                       
                                       
                                       
                      )
                      
                    )
                  )
                  
                )
                
        )
      
                
        )
        
        
        
      
  #
      )   
              
      ))
      )
      
    # end dashboardBody
    
  # end dashboardPage
  





