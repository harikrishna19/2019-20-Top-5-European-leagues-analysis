#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(shinyAce)
library(d3heatmap)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(formattable)
library(MCDM)


library(shinyAce)
library(formattable)
library(shinyjs)
library(DiagrammeR)
library(ahp)

#loading all five leagues loan data for loan analysis for the players
player_loan<-read.csv("C:\\Users\\krishna\\Desktop\\PROJECT\\Transfermarkt-master\\Transfermarkt-master\\data\\2019\\201920loandata.csv")

#utils::View(player_loan)

DoCalculation <- function(input) {
  
  tryCatch({
    modelString <- input$ace
    #print(modelString)
    ahpTree <- LoadString(modelString)
    Calculate(ahpTree, GetMethod(input))
    return (ahpTree)
  },
  error = function(e) {
    return (NULL)
  })
  
}


GetTable <- function(input, ahpTree) {
  
  tryCatch({
    if (!is.null(ahpTree)) {
      dm <- ifelse(
        is.null(input$decisionMaker),                                                                             
        yes = "Total", 
        no = input$decisionMaker
      )
      
      co <- TryAsNumeric(input$cutoff, 0)
      le <- TryAsNumeric(input$level, 0)
      renderFormattable(
        AnalyzeTable(
          ahpTree, 
          decisionMaker = dm,
          variable = GetVariable(input),
          sort = GetSort(input),
          pruneFun = function(x, dm) PruneByCutoff(x, dm, co) && PruneLevels(x, dm, le)
          
        )
      )
    } else {
      print("not printing table")
      renderUI("")
    }
  },
  error = function(e) {
    warning(as.character(e))
    renderUI(as.character(e))
  })
}

GetMethod <- function(input) {
  if (is.null(input$ahpmethod)) return (PrioritiesFromPairwiseMatrixEigenvalues)
  switch (
    input$ahpmethod,
    `Eigenvalues` = PrioritiesFromPairwiseMatrixEigenvalues,
    `Mean of Normalized Values` = PrioritiesFromPairwiseMatrixMeanNormalization,
    `Geometric Mean` = PrioritiesFromPairwiseMatrixGeometricMean
  )
}


GetVariable <- function(input) {
  if (is.null(input$variable)) return ("weightContribution")
  switch (
    input$variable,
    `Total Contribution` = "weightContribution",
    `Score` = "score",
    `Priority` = "priority"
  )
}

GetSort <- function(input) {
  if (is.null(input$sort)) return ("priority")
  switch (
    input$sort,
    `Priority` = "priority",
    `Total Priority` = "totalPriority",
    `Original` = "orig"
  )
}

TryAsNumeric <- function(input, defaultVal) {
  if (is.numeric(input)) return (input)
  
  res <- tryCatch(
    as.numeric(input),
    warning = function(e) defaultVal,
    error = function(e) defaultVal
    
  )
  if (!is.na(defaultVal) && is.na(res)) res <- defaultVal
  return (res)
}

#loading packages


if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr", "ggplot2", "readr", "showtext")
source("C:\\Users\\krishna\\Desktop\\PROJECT\\Transfermarkt-master\\Transfermarkt-master\\src\\01-clean.R")


#analysis


# DATA AND ANALYSIS
# Look only at movements with a fee
over_loan <- read_csv("C:\\Users\\krishna\\Desktop\\PROJECT\\Transfermarkt-master\\Transfermarkt-master\\data\\2019\\201920loandata.csv") %>% tidy_transfers()
transfers <- over_loan %>% filter(!is_loan | fee > 0)
#View(transfers)


club_spending <-transfers %>%
  filter(movement == "In") %>%
  group_by(club,league) %>%
  summarise(expenditure = sum(fee, na.rm = TRUE)) %>%
  mutate(expenditure = expenditure / 1e6) %>%
  mutate(league=league)



#profits and transfers




club_sales <- transfers %>%
  filter(movement == "Out") %>%
  group_by(club,league) %>%
  summarize(income = sum(fee, na.rm = TRUE)) %>%
  mutate(income = income / 1e6,
         league=league)

club_record <- merge(club_spending, club_sales) %>%
  mutate(profit = income - expenditure)

shinyServer<- function(input, output, session) {
    

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
                color = "orange",
                value = rv$df %>% filter(League %in% rvLeague$League) %>% 
                    summarise(total = paste0("€", round(sum(Value_Euros1 / 1000000000), digits = 1), "B")) %>% pull(total),
                subtitle = "Total League Value",
                icon=icon("landmark")
            )
            
        })
        output$numofplayers <- renderValueBox({
            
            if(is.null(rvLeague$League)) return(NULL)
            
            valueBox(color = "orange",
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
                scale_fill_manual(values = c("seagreen", "orchid", "steelblue" ,"khaki"))+
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
                    scale_fill_gradient(low = "khaki", high = "seagreen")+
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
                    scale_fill_gradient(low = "khaki", high = "seagreen")+
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
                    scale_fill_gradient(low = "khaki", high = "seagreen")+
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
            valueBox(df_Players$contract_valid_until, subtitle  = 'Contract.Valid.Until', color = "red")
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
            valueBox(df_Players2$contract_valid_until, subtitle = 'Contract.Valid.Until', color = "blue")
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
            
            
        })
        output$PlayerImg <- renderUI({
            img(src='unknown.png',height=100,width=100)
        })
        
        output$PlayerImg2 <- renderUI({
            img(src='unknown.png',height=100,width=100)
        })
        
        
        
    })

    rvleag<-reactiveValues(df=NULL)
    rvleag$df<-club_record
    
    rvLeag <- reactiveValues(League = NULL,Team=NULL)
    
    observeEvent(input$tt_select, {
      
      req(input$tt_league)
      
      rvLeag$League <- input$tt_league
      
      #to_player loan to plot
      
      
      
      # Wolves
      output$tt_summary<-renderPlot({ 
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
      output$tt_profit<-renderPlot({
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
        
          scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Premier League
        
        }
         else{
        return(NULL)}
        
        
      })
      
    })    
    
    
    observeEvent(input$navbar, {
      print(paste0("event: navbar ", input$navbar))
      
      # Visualize
      if(input$navbar == "visualizePanel") {
        ahpTree <- DoCalculation(input)
        output$visualizeTree <- renderGrViz(grViz(generate_dot(GetGraph(ahpTree))))
      }
      
      
      if(input$navbar == "AHP File Format") {
        #cannot find images
        #output$fileFormat <- renderUI(includeMarkdown(rmarkdown::render(system.file("doc", "file-format.Rmd", package="ahp"))))
        
        #does not produce toc
        #output$fileFormat <- renderUI(includeMarkdown(system.file("doc", "file-format.Rmd", package="ahp")))
        
        #only works nicely if theme is the same for shiny app and for vignette
        output$fileFormat <- renderUI(includeHTML(system.file("doc", "file-format.html", package="ahp")))
      }
    })
    
    # Show Upload
    observeEvent(input$showUpload ,{
      print("event: showUpload")
      sampleFiles <- list.files(system.file("extdata", package="ahp"), full.names = TRUE)
      sampleFiles <- basename(sampleFiles[!file.info(sampleFiles)$isdir])
      output$uploadFileOutput <- renderUI({
        #input$uploadFile
        fluidRow(
          column(
            4,
            selectInput("examples", 
                        "Load package example: ", 
                        choices = c("", sampleFiles), 
                        selected = ""
            )
          ),
          column(
            8,
            HTML('<label class="control-label" for="examples">Load file from disk: </label>'),
            br(),
            fileInput('uploadFile', NULL, width="80%")
          )
        )
        
      })
    })
    
    # Upload File
    observeEvent(input$uploadFile, {
      fileContent <- readChar(input$uploadFile$datapath, file.info(input$uploadFile$datapath)$size)
      updateAceEditor(session, "ace", value = fileContent)
      output$uploadFileOutput <- renderUI("")
    })
    
    
    # Examples
    observeEvent(input$examples, {
      print("event: examples")
      if (nchar(input$examples) > 0) {
        ahpFile <- system.file("extdata", input$examples, package="ahp")
        fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
        updateAceEditor(session, "ace", value = fileContent)
        output$uploadFileOutput <- renderUI("")
      }
      
    })
    
    # Calculate AHP
    observe({
      
      input$variable
      input$decisionMaker
      input$ahpmethod
      input$sort
      #input$ace
      input$cutoff
      input$level
      input$navbar
      
      if (input$navbar == "analysis") {
        print("Calculate tree")
        tryCatch({
          ahpTree <- DoCalculation(input)
          #print(GetDataFrame(ahpTree))
          decisionMakers <- ahp:::GetDecisionMakers(ahpTree)
          if(length(decisionMakers) > 1) {
            output$decisionMaker <- renderUI({
              radioButtons("decisionMaker", 
                           "Decision Maker: ", 
                           choices = c("Total", decisionMakers), 
                           selected = ifelse(is.null(input$decisionMaker), yes = "Total", no = input$decisionMaker))
            })
            show(id = "decisionMaker", anim = TRUE)
            sel <- ifelse(length(input$sort) > 0, input$sort, "Total Priority")
            #browser()
            updateRadioButtons(session, "sort", choices = c("Total Priority", "Priority", "Original"), selected = sel)
          } else {
            #browser()
            sel <- ifelse(length(input$sort) == 0 || input$sort == "Total Priority", "Priority", input$sort)
            updateRadioButtons(session, "sort", choices = c("Priority", "Original"), selected = sel)
            #Cannot just remove, because otherwise the value sticks!
            updateRadioButtons(session, "decisionMaker", selected = "Total")
            hide(id = "decisionMaker", anim = TRUE)
          }
          #browser()
          output$table <- GetTable(input, ahpTree)
          
        },
        error = function(e) {
          output$table <- renderFormattable(formattable(TRUE, yes = as.character(e)))
          
        })
      } 
    })
    
    
    
    
    
    ## Event Handlers
    #############################
    
    
    
    
    output$downloadFile <- downloadHandler(
      
      
      filename = function() {
        nme <- 'model.ahp'
        try(
          {
            #try to derive name from model spec
            modelString <- input$ace
            myAhpTree <- LoadString(modelString)
            nme <- paste0(gsub('[^a-zA-Z]', '', myAhpTree$name), ".ahp")
          },
          silent = TRUE
        )
        return (nme)
      },
      content = function(file) {
        writeChar(input$ace, file)
      }
    )
    
    
    hide(id = "loading-content", anim = TRUE, animType = "fade")
    show("app-content")
    
    observe({
      if (input$source == "example") {
        enable("use")
      } else if (input$source == "upload" & !is.null(input$upload_data)) {
        enable("use")
      } else {
        disable("use")
      }
    })
    
    rawdata <- eventReactive(input$use, {
      if (input$source == "example") {
        read_csv("./data/dummy.csv")
      } else if (input$source == "upload") {
        read_csv(input$upload_data$datapath)
      }
    })
    
    observeEvent(input$use, {
      updatePickerInput(
        session = session,
        inputId = "alternative",
        choices = names(rawdata())
      )
      
      updatePickerInput(
        session = session,
        inputId = "attribute_max",
        choices = names(rawdata())
      )
      
      updatePickerInput(
        session = session,
        inputId = "attribute_min",
        choices = names(rawdata())
      )
    })
    
    observe({
      toggleState(id = "arrange", condition = !is.null(input$alternative) & {
        !is.null(input$attribute_max) | !is.null(input$attribute_min)
      })
    })
    
    observeEvent(input$arrange, {
      showModal(
        modalDialog(
          title = strong("Dataset is set!"),
          "Please have a look at the arranged dataset, is it already in correct set up?", 
          br(),
          "If so, please continue to 'Analysis' tab! Otherwise, you can rearrange the dataset.",
          size = "m",
          easyClose = TRUE, 
          fade = TRUE
        )
      )
    })
    
    dataset <- eventReactive(input$arrange, {
      cost <- style(
        "background-color" = csscolor("darkred"),
        color = "white",
        display = "block",
        "border-radius" = "4px",
        "padding" = "0 4px"
      )
      
      benefit <- style(
        "background-color" = csscolor("seagreen"),
        color = "white",
        display = "block",
        "border-radius" = "4px",
        "padding" = "0 4px"
      )
      
      if (!is.null(input$attribute_max) & !is.null(input$attribute_min)) {
        res <- rawdata() %>%
          select(one_of(input$alternative, input$attribute_max, input$attribute_min)) %>% 
          formattable(
            list(area(col = isolate(input$attribute_max)) ~ formatter("span", style = benefit),
                 area(col = isolate(input$attribute_min)) ~ formatter("span", style = cost))
          )
      } else if  (is.null(input$attribute_min)) {
        res <- rawdata() %>%
          select(one_of(input$alternative, input$attribute_max, input$attribute_min)) %>% 
          formattable(
            list(area(col = isolate(input$attribute_max)) ~ formatter("span", style = benefit))
          )
      } else if (is.null(input$attribute_max)) {
        res <- rawdata() %>%
          select(one_of(input$alternative, input$attribute_max, input$attribute_min)) %>% 
          formattable(
            list(area(col = isolate(input$attribute_min)) ~ formatter("span", style = cost))
          )
      }
      return(res)
    })
    
    output$tab_dataset <- DT::renderDataTable({
      dataset() %>%
        as.datatable(
          rownames = FALSE,
          caption = "Columns with green colour define attributes which are desirable in high values, whereas columns with red colour define attributes which are undesirable in high values.",
          style = "bootstrap",
          extensions = c("Scroller", "Buttons"),
          options = list(
            dom = "Brt",
            autoWidth = FALSE,
            scrollX = TRUE,
            deferRender = TRUE,
            scrollY = 300,
            scroller = TRUE,
            buttons =
              list(
                list(
                  extend = "copy"
                ),
                list(
                  extend = "collection",
                  buttons = c("csv", "excel"),
                  text = "Download"
                )
              )
          )
        )
    },
    server = FALSE
    )
    
    
    
    
    
    
    
    #code change
    observeEvent(input$arrange, {
      output$setting_panel <- renderUI({
        tagList(
          map(
            c(input$attribute_max, input$attribute_min),
            ~ numericInput(
              inputId = paste0("weight_", .x), label = paste("Weight for", .x),
              min = 0, max = 1, value = 1 / length(c(input$attribute_max, input$attribute_min))
            )
          ),
          helpText("The sum of weights should be equal to 1")
        )
      })
    })
    
    observe({
      toggleState(
        id = "apply",
        condition = !is.null(dataset())
      )
    })
    
    res <- eventReactive(input$apply, {
      if (input$method == "MetaRanking") {
        cb <- c(
          rep("max", length(input$attribute_max)),
          rep("min", length(input$attribute_min))
        )
        
        w <- map_dbl(
          c(input$attribute_max, input$attribute_min),
          ~ input[[paste0("weight_", .x)]]
        )
        
        res <- dataset() %>%
          as.data.frame() %>%
          `rownames<-`(.[, input$alternative]) %>%
          select_if(is.numeric) %>%
          as.matrix() %>%
          MetaRanking_custom(weights = w, cb =  cb, v = input$v, lambda = input$lambda) %>%
          as_tibble() %>%
          rename(Alternative = Alternatives,
                 "TOPSIS Vector" = TOPSISVector,
                 "TOPSIS Linear" = TOPSISLinear,
                 "Meta Ranking (Sum)" = MetaRanking_Sum,
                 "Meta Ranking (Aggregate)" = MetaRanking_Aggreg) %>%
          mutate(Alternative = pull(dataset()[, input$alternative])) %>% 
          mutate_if(is_double, .funs = funs(round(., 2)))
        
      } else if (input$method == "MMOORA") {
        cb <- c(
          rep("max", length(input$attribute_max)),
          rep("min", length(input$attribute_min))
        )
        
        w <- map_dbl(
          c(input$attribute_max, input$attribute_min),
          ~ input[[paste0("weight_", .x)]]
        )
        
        res <- dataset() %>%
          as.data.frame() %>%
          `rownames<-`(.[, input$alternative]) %>%
          select_if(is.numeric) %>%
          as.matrix() %>%
          MMOORA(w, cb) %>%
          as_tibble() %>%
          rename(Alternative = Alternatives,
                 "Ratio System" = RatioSystem,
                 "Ranking (Ratio System)" = Ranking,
                 "Reference Point" = ReferencePoint,
                 "Ranking (Reference Point)" = Ranking.1,
                 "Multiplicative Form" = MultiplicativeForm,
                 "Ranking (Multipicative Form)" = Ranking.2,
                 "Overal Ranking (Multi MOORA)" = MultiMooraRanking) %>% 
          mutate(Alternative = pull(dataset()[, input$alternative])) %>% 
          mutate_if(is_double, .funs = funs(round(., 2)))
      } else if (input$method == "RIM") {
        
      } else if (input$method == "TOPSISLinear") {
        cb <- c(
          rep("max", length(input$attribute_max)),
          rep("min", length(input$attribute_min))
        )
        
        w <- map_dbl(
          c(input$attribute_max, input$attribute_min),
          ~ input[[paste0("weight_", .x)]]
        )
        
        res <- dataset() %>%
          as.data.frame() %>%
          `rownames<-`(.[, input$alternative]) %>%
          select_if(is.numeric) %>%
          as.matrix() %>%
          TOPSISLinear(w, cb) %>%
          as_tibble() %>%
          rename(Alternative = Alternatives,
                 "R index" = R) %>%
          mutate(Alternative = pull(dataset()[, input$alternative])) %>% 
          mutate_if(is_double, .funs = funs(round(., 2)))
      } else if (input$method == "TOPSISVector") {
        cb <- c(
          rep("max", length(input$attribute_max)),
          rep("min", length(input$attribute_min))
        )
        
        w <- map_dbl(
          c(input$attribute_max, input$attribute_min),
          ~ input[[paste0("weight_", .x)]]
        )
        
        res <- dataset() %>%
          as.data.frame() %>%
          `rownames<-`(.[, input$alternative]) %>%
          select_if(is.numeric) %>%
          as.matrix() %>%
          TOPSISVector(w, cb) %>%
          as_tibble() %>%
          rename(Alternative = Alternatives,
                 "R index" = R) %>%
          mutate(Alternative = pull(dataset()[, input$alternative])) %>% 
          mutate_if(is_double, .funs = funs(round(., 2)))
      } else if (input$method == "VIKOR") {
        cb <- c(
          rep("max", length(input$attribute_max)),
          rep("min", length(input$attribute_min))
        )
        
        w <- map_dbl(
          c(input$attribute_max, input$attribute_min),
          ~ input[[paste0("weight_", .x)]]
        )
        
        res <- dataset() %>%
          as.data.frame() %>%
          `rownames<-`(.[, input$alternative]) %>%
          select_if(is.numeric) %>%
          as.matrix() %>%
          VIKOR(w, cb, v = input$v) %>%
          as_tibble() %>%
          rename(Alternative = Alternatives,
                 "S index" = S,
                 "R index" = R,
                 "Q index" = Q) %>%
          mutate(Alternative = pull(dataset()[, input$alternative])) %>% 
          mutate_if(is_double, .funs = funs(round(., 2)))
      } else if (input$method == "WASPAS") {
        cb <- c(
          rep("max", length(input$attribute_max)),
          rep("min", length(input$attribute_min))
        )
        
        w <- map_dbl(
          c(input$attribute_max, input$attribute_min),
          ~ input[[paste0("weight_", .x)]]
        )
        
        res <- dataset() %>%
          as.data.frame() %>%
          `rownames<-`(.[, input$alternative]) %>%
          select_if(is.numeric) %>%
          as.matrix() %>%
          WASPAS(w, cb, lambda = input$lambda) %>%
          as_tibble() %>%
          rename(Alternative = Alternatives,
                 "WSM Score" = WSM,
                 "WPM Score" = WPM,
                 "Q index" = Q) %>%
          mutate(Alternative = pull(dataset()[, input$alternative])) %>% 
          mutate_if(is_double, .funs = funs(round(., 2))) 
      }
      return(res)
    })
    
    output$tab_res <- DT::renderDataTable({
      res() %>%
        datatable(
          rownames = FALSE,
          style = "bootstrap",
          extensions = c("Scroller", "Buttons"),
          options = list(
            dom = "Brt",
            autoWidth = FALSE,
            scrollX = TRUE,
            deferRender = TRUE,
            scrollY = 300,
            scroller = TRUE,
            buttons =
              list(
                list(
                  extend = "copy"
                ),
                list(
                  extend = "collection",
                  buttons = c("csv", "excel"),
                  text = "Download"
                )
              )
          )
        )
    },
    server = FALSE
    )
    
    
    
    
}

        
      
    
    
    


    


