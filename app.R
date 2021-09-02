

# Loading Packages --------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(htmltools)
library(ggplot2)
library(tidyverse)
library(radarchart)
library(ellipsis)
library(readr)
library(stringi)
library(stringr)
library(magrittr)
library(png)
library(Rcpp)
library(pillar)
library(shinyWidgets)
library(plotly)
library(DT)
library(utils)
library(data.table)
library(bslib)
library(maps)
library(ggrepel)
library(showtext)
library(gridExtra)
library(ggpubr)
library(shinycssloaders)
library(dashboardthemes)
# Importing Data For Analysis ---------------------------------------------


source("01-clean.R")
#source("readData.R")

# UI ----------------------------------------------------------------------

ui <- fluidPage(theme=shinytheme("united"),
                  
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
            
                    #skin = "black",
                    
                    dashboardHeader(title="2019/20 Top 5 European Leagues Analysis", titleWidth = 500),
                    
                    dashboardSidebar(width = 300,
                                     sidebarMenu(
                                       HTML(paste0(
                                         "<br>",
                                         "<img src='https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTlakFARt_VgdoAK5V4BnirOpMa4wX0HJoyPw&usqp=CAU'width = '295'></img>",
                                         "<br>",
                                         "<p style = 'text-align: center;'><small><a href='https://www.uefa.com/' target='_blank'>UEFA</a></small></p>",
                                         "<br>"
                                       )),
                                       
                                       menuItem("Home", tabName = "Home", icon = icon("home")), 
                                       menuItem("League Analysis", tabName = "tab_leagues", icon = icon("balance-scale")),
                                       menuItem("Player Analysis", tabName = "tab_players", icon = icon("futbol")),
                                       #menuItem("Ranking", tabName = "tab_ranking", icon = icon("chart-line")),
                                       menuItem("Loan Analysis", tabName = "tab_loan", icon = icon("donate")),
                                       
                                       
                                       HTML(paste0(
                                         "<br><br><br><br><br><br><br><br><br>",
                                         "<table style='margin-left:auto; margin-right:auto;'>",
                                         "<tr>",
                                         
                                         "<td style='padding: 5px;'><a href='https://www.linkedin.com/in/hari-krishna-a05b44187/' target='_blank'><i class='fab fa-linkedin fa-lg'></i></a></td>",
                                         "<td style='padding: 5px;'><a href='https://www.twitter.com/Hakrishna19' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
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
                      
                      shinyDashboardThemes(
                        theme = "grey_dark"
                      ),
                      
                      
                      tabItems(
                        

                        tabItem(tabName = "tab_players",

                                column(
                                  width = 12,

                                  box(
                                    title = tagList(icon("bullseye"), "Player Stats"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,

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
                                        #conditionalPanel(condition = "output.PlayerImg",hr()),

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

                                

                                      column(
                                        width = 12,

                                        tabsetPanel(type = "pills",

                                                    tabPanel("Radar",
                                                             br(),withSpinner(plotlyOutput("tp_radar"))



                                                    
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
                              box(title = "2019/20 Analysis", status = "success", width = NULL, solidHeader = TRUE, collapsible = FALSE,skin="black",

                                  # Ronaldo FIFA 19 Image
                                  column(width = 8, tags$img(src="bb.jpg", align="center", style="width: 100%; height: 150%;")),

                                  # Description
                                  column(width = 4,
                                         tags$img(src="img.jpg",style = "height: 70%; width: 84%;margin-left: 10%;",align="center"),
                                         # tags$img(src="pl.png", style = "height: 70%; width: 14%;margin-left: 10%;"),
                                         # tags$img(src="laliga.png", style = "height: 80%; width: 14%; margin-left: 10px;"),
                                         # tags$img(src="bundesliga.png", style = "height: 80%; width: 14%; margin-left: 10px;"),
                                         # tags$img(src = "seriea.png", style = "height: 80%; width: 14%; margin-left: 5px;"),
                                         # tags$img(src = "ligue1.png", style = "height: 80%; width: 14%; margin-left: 5px;"),
                                         tags$br(), tags$br(),
                                         tags$hr(),
                                         # fluidRow(column(width = 4),
                                         #          #column(width = 4, tags$img(src="respectuefa.png",align="center", style = "height: 5%; width: 200%")),
                                         #          column(width = 4)),
                                         # tags$br(),
                                         # tags$hr(),
                                         tags$p("The dashboard which I have designed visualizations related to 2019/20 Top 5 European  leagues.")
                                         
                                  )
                              )
                            )
                          )







                        ),

                        tabItem(tabName = "tab_loan",
                                
                                column(
                                  width = 12,
                                  
                                  box(
                                    title = tagList(icon("line-chart"), "Team Stats"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,
                                    
                                    tags$hr(),
                                    
                                    fluidRow(
                                      
                                      column(
                                        width = 2,
                                        selectInput("tt_league", "League:", choices = unique(club_record$league),selected =c("Ligue 1")),
                                        actionBttn(inputId = "tt_select", tagList(icon("location-arrow"),"Select"),
                                                   size = "sm", color = "danger", style = "stretch", block = T)
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
                                      
                                      conditionalPanel(condition = "input.tt_league",
                                                       
                                                       tabsetPanel(type = "pills",
                                                                   
                                                                   tabPanel("Spending",
                                                                            br(),
                                                                          withSpinner(plotOutput("tt_summary",height = "700px"))),
                                                                   
                                                                    tabPanel("Profit", 
                                                                          withSpinner(plotOutput("tt_profit",height = "700px")))
                                                                   
                                                                    # 
                                                                    # tabPanel("Transfers", 
                                                                    #          DT::dataTableOutput("tt_top",height = "700px"))
                                                                   
                                                                   # tabPanel("BMI", 
                                                                   #          br(),
                                                                   #          column(width = 6, tableOutput("tt_bmi")),
                                                                   #          column(width = 6, plotOutput("tt_bmi2")))
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
                                    title = tagList(icon("bullseye"), "League Stats"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,theme=shinytheme('united'),

                  tags$hr(),

                  fluidRow(

                    column(
                      width = 2,
                      selectInput("tl_league", "League:", choices = unique(dset$League)),
                      actionBttn(inputId = "tl_select", tagList(icon("location-arrow"),"Select"),
                                 size = "sm", color = "warning", style = "stretch", block = T)
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

                    conditionalPanel(condition = "input.tl_league",

                                                         fluidRow(
                                                         withSpinner(valueBoxOutput("values")),
                                                         withSpinner(valueBoxOutput("numofplayers")),
                                                         withSpinner(valueBoxOutput("teams"))
                                                       ),
                                                                          br(),
                                                                          fluidRow(
                                                                            conditionalPanel(
                                                       condition = "input.tl_league",
                                                                         column(width = 6,
                                                        div(style="clear:both;  margin-left: 20px;"),
                                                        plotOutput("comp_plot")
                                                                           ),
                                                                             column(width = 6,
                                                                                    tabsetPanel(
                                                                                      tabPanel("Nationality",
                                                                      br(),
                                                                           
                                                                                withSpinner(plotlyOutput(outputId = "league_nat1"))
                                                                  ),
                 
                                                                     tabPanel("Players",
                                                                              br(),
                                                                             
                                                                          withSpinner(plotOutput(outputId = "league_values", height = "100%")
                                                                          )
                                                             ),
                                                                                tabPanel("Comparison",
                                                                                   br(),
                                                                                        fluidRow(
                                                                             column(width = 6, selectInput("comp_league",'League', choices = c("League", "Team", "Position"))),
                                                                               column(width = 6, selectInput("comp_graph",'Graph', choices = "Bar"))
                                                                             ),

                                                                             br(),withSpinner(plotOutput("league_comp1")
                                                                    ))
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
)
                  )

# # end dashboardBody
# 
# # end dashboardPage
# 
# )))


# Server ------------------------------------------------------------------


#source("data/global.R")
server<- function(input, output, session) {
  

  
  rv<-reactiveValues(df=NULL)
  rv$df<-dset
  
  
  
  rvLeague <- reactiveValues(League = NULL,Team=NULL)
  
  
  
  # Observe Event League Selection
  observeEvent(input$tl_select, {
    
    req(input$tl_league)
    
    rvLeague$League <- input$tl_league
    
    
    
    
    # 1. Value Box ---------------------------------------------------------------
    
    
    output$values <- renderValueBox({
      
      if(is.null(rvLeague$League)) return(NULL)
      
      valueBox(
        color = "green",
        value = rv$df %>% filter(League %in% rvLeague$League) %>% 
          summarise(total = paste0("€", round(sum(Value_Euros1 / 1000000000), digits = 1), "B")) %>% pull(total),
        subtitle = "Total League Value",
        icon=icon("landmark")
      )
      
    })
    output$numofplayers <- renderValueBox({
      
      if(is.null(rvLeague$League)) return(NULL)
      
      valueBox(color = "purple",
               value = rv$df %>% filter(League %in% rvLeague$League) %>% select(club) %>% nrow(),
               subtitle = "Number of Players",
               icon=icon("portrait")
      )
    })
    
    output$teams <- renderValueBox({
      
      if(is.null(rvLeague$League)) return(NULL)
      valueBox(
        color = "orange",
        value = rv$df %>% filter(League %in% rvLeague$League) %>% select(club) %>% distinct() %>% nrow(),
        "Number of Teams",
        icon=icon("project-diagram")
      )
    })
    
    
    # 2. Best Team ---------------------------------------------------------------
    bestLeague <- rv$df %>% filter(League %in% rvLeague$League)
    
    output$comp_plot<-renderPlot({
      rv$df %>% 
        group_by(League) %>% 
        summarise(mean = mean(overall)) %>% 
        arrange(-mean) %>% 
        head(20)
      rv$df %>% 
        group_by(League, Class) %>% 
        summarise(mean = mean(overall)) %>% 
        ungroup() %>% 
        filter(League %in% rvLeague$League) %>% 
        ggplot(aes(reorder(League, mean), mean, fill = Class))+
        geom_col(position = "fill")+
        geom_text(aes(label = round(mean,digits = 2)), position = position_fill(0.5))+
        coord_flip()+
        theme_minimal()+
        theme(legend.position = "top")+
        labs(x = NULL, y = NULL, title = "League Power for every position class")
    })
    
    
    
    # 3. Map ---------------------------------------------------------------------
    output$league_nat1 <-  renderPlotly({
      
      world_map <- map_data("world")
      
      numofplayers <- world_map %>% 
        mutate(region = as.character(region)) %>% 
        left_join((rv$df %>% mutate(nationality = as.character(nationality),
                                    nationality = if_else(nationality %in% "England", "UK", nationality)) %>%
                     filter(League == rvLeague$League) %>%
                     count(nationality, name = "Number of Player") %>%
                     rename(region = nationality) %>%
                     mutate(region = as.character(region))), by = "region")
      
      ggplotly(
        ggplot(numofplayers, aes(long, lat, group = group))+
          geom_polygon(aes(fill = `Number of Player` ), color = "white", show.legend = FALSE)+
          scale_fill_viridis_c(option = "C")+
          theme_void()+
          labs(fill = "Number of Players",
               title = "Nationality of The Players in The League"))
      
    })
    
    # 4. Player Value -------------------------------------------------------------------
    
    output$league_values <- renderPlot(width = "auto",{
      
      if(is.null(rvLeague$League)) return(NULL)
      
      rv$df %>% 
        filter(League == rvLeague$League) %>% 
        arrange(-Value_Euros1) %>% 
        group_by(Class) %>% 
        top_n(n = 10, wt = Value_Euros1) %>% 
        ggplot(aes(reorder(Player, Value_Euros1), Value_Euros1, fill = Class, label = paste0("€", Value_Euros1 / 1000000, "M")))+
        geom_col(show.legend = FALSE)+
        geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
        coord_flip()+
        theme_minimal()+
        facet_wrap(Class~., scales = "free")+
        scale_fill_manual(values = c("red", "blue", "green" ,"purple"))+
        theme(axis.text.x=element_blank(),
              strip.background =element_rect(fill="gray"),
              strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))+
        labs(title = "Most Valuable Players", x = NULL, y = NULL)
      
      
      
    }, height = function() {
      session$clientData$output_league_values_width})
    
    
    # 5. Comprasion --------------------------------------------------------------
    
    output$league_comp1 <- renderPlot({
      
      if(is.null(rvLeague$League)) return(NULL)
      
      if(input$comp_league == "League" && input$comp_graph == "Bar"){
        
        rv$df %>% 
          group_by(League) %>% 
          summarise(Total.Value = sum(as.integer(Value_Euros1), na.rm = TRUE)) %>% 
          ggplot(aes(reorder(League, Total.Value), Total.Value, fill = Total.Value))+
          geom_col(show.legend = FALSE)+
          coord_flip()+
          theme_minimal()+
          labs(x = NULL, y = "League Total Value")+
          scale_fill_gradient(low = "#132B43", high = "#56B1F7")+
          theme(axis.line.y = element_line(colour = "darkslategray"),
                axis.ticks.x = element_line(colour = "darkslategray"))+
          scale_y_continuous(labels = c("0 €", "2 Billion €", "4 Billion €", "6 Billion €"))
        
        
      }else if(input$comp_league == "Team" && input$comp_graph == "Bar"){
        
        p <- rv$df %>%
          filter(League == rvLeague$League) %>% 
          group_by(club) %>% 
          summarise(Total.Value = sum(Value_Euros1)) %>% 
          ggplot(aes(reorder(club, Total.Value), Total.Value, fill = Total.Value))+
          geom_col(show.legend = FALSE)+
          coord_flip()+
          theme_minimal()+
          labs(x = NULL, y = "Team Total Value")+
          scale_fill_gradient(low = "#132B43", high = "#56B1F7")+
          theme(axis.line.y = element_line(colour = "darkslategray"),
                axis.ticks.x = element_line(colour = "darkslategray"))
        
        if(rvLeague$League %in% c("Bundesliga", "Serie A")){
          
          p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M", "€800M")) # Bundesliga & Serie A 
          
        }else if(rvLeague$League == "Ligue 1"){ 
          
          p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M","€800M")) # Ligue 1
          
        }else if(rvLeague$League == "La Liga"){
          
          p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M","€800M")) # "La Liga"
          
        }else if(rvLeague$League == "Premier League"){
          
          p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M","€800M")) # Premier League
          
        }else{return(NULL)}
        
      }else if(input$comp_league == "Position" && input$comp_graph == "Bar"){
        
        p <- rv$df %>%
          filter(League == rvLeague$League) %>% 
          group_by(Class) %>% 
          summarise(Total.Value = sum(as.numeric(Value_Euros1))) %>% 
          ggplot(aes(reorder(Class, Total.Value), Total.Value, fill = Total.Value))+
          geom_col(show.legend = FALSE)+
          coord_flip()+
          theme_minimal()+
          labs(x = NULL, y = "Position Total Value")+
          scale_fill_gradient(low = "#132B43", high = "#56B1F7")+
          theme(axis.line.y = element_line(colour = "darkslategray"),
                axis.ticks.x = element_line(colour = "darkslategray"))
        
        if(rvLeague$League %in% c("Bundesliga", "Serie A")){
          
          p+scale_y_continuous(labels =  c("€0", "€200M", "€400M", "€600M","800M")) # Bundesliga % Serie A
          
        }else if(rvLeague$League == "La Liga"){
          
          p+scale_y_continuous(labels =  c("€0", "€200M", "€400M", "€600M","€800M","")) # La Liga
          
        }else if(rvLeague$League == "Premier League"){
          
          p+scale_y_continuous(labels =  c("€0", "€200M", "€400M", "€600M","€800M",""))  # Premier League
          
        }else if(rvLeague$League == "Ligue 1"){
          
          p+scale_y_continuous(labels =  c("€0", "€200M", "€400M", "€600M","800M"))  # Ligue 1
          
        }else{return(NULL)}
        
        
      }
      
      
      
      
      
    })
    
    
  })
  


      # ObserveEvent sonu

      rv<-reactiveValues(df=NULL)
      rv$df<-dset

      rvPlayer <- reactiveValues(League = NULL, Team = NULL, Player = NULL,
                                 League2 = NULL, Team2 = NULL, Player2 = NULL)
      observeEvent(input$tp_league,{
          updateSelectizeInput(session = session,inputId = 'tp_team',choices =unique(dset$club[dset$League==input$tp_league]),server = TRUE)
      })
      observeEvent(input$tp_team,{
          updateSelectizeInput(session = session,inputId = 'tp_player',choices = unique(dset$Name.Pos[dset$League==input$tp_league&dset$club==input$tp_team]),server = TRUE)
      })
      updateSelectizeInput(session = session,inputId = 'tp_league',choices = unique(dset$League),server = TRUE)
      updateSelectizeInput(session=session,inputId = 'tp_team',choices = unique(dset$club),server=TRUE)
      updateSelectizeInput(session = session,inputId = 'tp_player',choices = unique(dset$Name.Pos),server = TRUE)


      observeEvent(input$tp_league2,{
          updateSelectizeInput(session = session,inputId = 'tp_team2',choices =unique(dset$club[dset$League==input$tp_league2]),server = TRUE)
      })
      observeEvent(input$tp_team2,{
          updateSelectizeInput(session = session,inputId = 'tp_player2',choices = unique(dset$Name.Pos[dset$League==input$tp_league2&dset$club==input$tp_team2]),server = TRUE)
      })



      updateSelectizeInput(session = session,inputId = 'tp_league2',choices = unique(dset$League),server = TRUE)
      updateSelectizeInput(session=session,inputId = 'tp_team2',choices = unique(dset$club),server =TRUE)


      updateSelectizeInput(session = session,inputId = 'tp_player2',choices = unique(dset$Name.Pos),server = TRUE)
      observe({


          req(input$tp_league)
          req(input$tp_team)
          req(input$tp_player)

          rvPlayer$League <- input$tp_league
          rvPlayer$Team   <- input$tp_team
          rvPlayer$Player <- input$tp_player

          req(input$tp_league2)
          req(input$tp_team2)
          req(input$tp_player2)

          rvPlayer$League2 <- input$tp_league2
          rvPlayer$Team2   <- input$tp_team2
          rvPlayer$Player2 <- input$tp_player2

          df_Players <- rv$df %>%
              filter(League %in% rvPlayer$League, club %in% rvPlayer$Team, Name.Pos %in% rvPlayer$Player)

          df_Players2 <- rv$df %>%
              filter(League %in% rvPlayer$League2, club %in% rvPlayer$Team2, Name.Pos %in% rvPlayer$Player2)


          # 1. Image -------------------------------------------------------------------


          # 2. Value Box ---------------------------------------------------------------

          # Player 1
          output$tp_age <- renderValueBox({
              valueBox(df_Players$age, subtitle  = 'Age', color = "red")
          })

          output$tp_overall <- renderValueBox({
              valueBox(df_Players$overall, subtitle  = 'Overall', color = "red")
          })


          output$tp_value <- renderValueBox({
              valueBox(df_Players$value_eur, subtitle = 'Value', color = "red")
          })


          output$tp_contract <- renderValueBox({
              valueBox(df_Players$contract_valid_until, subtitle  = 'Contract Validity', color = "red")
          })

          # Player 2

          output$tp_age2 <- renderValueBox({
              valueBox(df_Players2$age, subtitle = 'Age', color = "blue")
          })

          output$tp_overall2 <- renderValueBox({
              valueBox(df_Players2$overall, subtitle = 'Overall', color = "blue")
          })


          output$tp_value2 <- renderValueBox({
              valueBox(df_Players2$value_eur, subtitle = 'Value', color = "blue")
          })

          output$tp_contract2 <- renderValueBox({
              valueBox(df_Players2$contract_valid_until, subtitle = 'Contract Validity', color = "blue")
          })


          # 3. Radar -------------------------------------------------------------------

          output$tp_radar <- renderPlotly({


              radarP1 <- rv$df %>% filter(League %in% rvPlayer$League, club %in% rvPlayer$Team, Name.Pos %in% rvPlayer$Player)
              radarP2 <- rv$df %>% filter(League %in% rvPlayer$League2, club %in% rvPlayer$Team2, Name.Pos %in% rvPlayer$Player2)


              plot_ly(
                  type = 'scatterpolar',
                  fill = 'toself',
                  mode="lines+markers+text"
              ) %>%
                  add_trace(
                      r = c(radarP1 %>% pull(pace),radarP1 %>% pull(shooting), radarP1 %>% pull(passing), radarP1 %>% pull(dribbling), radarP1 %>% pull(defending),radarP1 %>% pull(physic)),
                      theta = c('Pace','Shooting','Passing', 'Dribbling', 'Defending','Physic'),
                      name = rvPlayer$Player
                  ) %>%
                  add_trace(
                      r = c(radarP2 %>% pull(pace),radarP2 %>% pull(shooting), radarP2 %>% pull(passing), radarP2 %>% pull(dribbling), radarP2 %>% pull(defending),radarP1 %>% pull(physic)),
                      theta = c('Pace','Shooting','Passing', 'Dribbling', 'Defending','Physic'),
                      name = rvPlayer$Player2
                  ) %>%
                  layout(
                      polar = list(
                          radialaxis = list(
                              visible = T,
                              range = c(0,100)
                          )))


          })%>% bindCache(input$tp_league,input$tp_team,input$tp_player,input$tp_league2,input$tp_team2,input$tp_player2, cache = "session")
          output$PlayerImg <- renderUI({
              img(src='unknown.png',height=100,width=100)
          })

          output$PlayerImg2 <- renderUI({
              img(src='unknown.png',height=100,width=100)
          })



      })
  
  
  
  
  observeEvent(input$tt_select,{
    rvleag<-reactiveValues(df=NULL)
    rvleag$df<-club_record
    
    
    
    
    rvLeag <- reactiveValues(League = NULL,Team=NULL)
    req(input$tt_league)
    
    rvLeag$League <- input$tt_league
    
    aa <- eventReactive(input$tt_select,{
      
      if(is.null(rvLeag$League)) return(NULL)
      v <- rvleag$df %>%
        filter(league == rvLeag$League) %>%
        group_by(club) %>%
        ggplot(aes(reorder(club,expenditure), expenditure, fill = expenditure))+
        geom_col(show.legend = FALSE)+
        geom_text(
          aes(label = sprintf("%0.2f", expenditure), hjust = -0.2),
          size = 3.5
        ) +
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "Team Total Value")+
        scale_fill_gradient(low = "khaki", high = "seagreen")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"))
      if(rvLeag$League %in% c("1 Bundesliga", "Serie A")){
        #
        v+scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Bundesliga & Serie A
        
      }else if(rvLeag$League == "Ligue 1"){
        
        v+scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Ligue 1
        
      }else if(rvLeag$League == "Laliga"){
        
        v+ scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # "La Liga"
        
      }else if(rvLeag$League == "Premier League"){
        
        v+ scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Premier League
        
      }
      else{
        return(NULL)}
    })
    
    
    bb <- eventReactive(input$tt_select,{
      
      
      if(is.null(rvLeag$League)) return(NULL)
      
      v<-club_record %>%
        filter(league == rvLeag$League) %>%
        group_by(club) %>%
        ggplot(aes(x = reorder(club, profit), y = profit)) +
        geom_col(show.legend = FALSE) +
        geom_text(
          aes(
            label = sprintf("%0.2f", profit),
            y = 0,
            hjust = if_else(profit > 0, 1.2, -0.2)
          ),
          size = 3.5
        ) +
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "Team Total Value")+
        scale_fill_gradient(low = "khaki", high = "seagreen")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"))
      if(rvLeag$League %in% c("1 Bundesliga", "Serie A")){
        
        v+ scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Bundesliga & Serie A
        
      }else if(rvLeag$League == "Ligue 1"){
        
        v+scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Ligue 1
        
      }else if(rvLeag$League == "Laliga"){
        
        v+ scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # "La Liga"
        
      }else if(rvLeag$League == "Premier League"){
        
        v+ scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Premier League
        
      }
      else{
        return(NULL)}
      
    })
    
    
    
    output$tt_summary<-renderPlot({
      
      aa()
    })
    
    output$tt_profit<-renderPlot({
      bb()
    })
    
  
  })
} 


shinyApp(ui, server)




