#'
#' Functions to clean scraped Transfermarkt data.
#'

# Dependencies
library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)
# suppressMessages(suppressWarnings(library(shiny)))
# suppressMessages(suppressWarnings(library(shinyWidgets)))
# suppressMessages(suppressWarnings(library(shinyjs)))
# suppressMessages(suppressWarnings(library(shinycssloaders)))
# suppressMessages(suppressWarnings(library(DT)))
# suppressMessages(suppressWarnings(library(tidyverse)))
# suppressMessages(suppressWarnings(library(formattable)))
# suppressMessages(suppressWarnings(library(shinydashboardPlus)))
# suppressMessages(suppressWarnings(library(shinydashboard)))
# suppressMessages(suppressWarnings(library(plotly)))
# suppressMessages(suppressWarnings(library(maps)))
# suppressMessages(suppressWarnings(library(png)))
# suppressMessages(suppressWarnings(library(readr)))
# suppressMessages(suppressWarnings(library(showtext)))
# suppressMessages(suppressWarnings(library(ggrepel)))
# suppressMessages(suppressWarnings(library(ellipsis)))
# suppressMessages(suppressWarnings(library(Rcpp)))
# suppressMessages(suppressWarnings(library(shinythemes)))





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
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(DT)
library(utils)
library(data.table)


dset=read.csv("ori_FIFA.csv",encoding = "UTF-8")
player_loan<-read.csv("201920loandata.csv",encoding = "UTF-8")

tidy_transfers <- function(df) {
    # Helper function to convert currency values from characters to numerics
    value_as_numeric <- function(val) {
        if (is.na(val) || val == "-" || val == "?") {
            val <- NA_real_
        } else if (stri_sub(val, -1, -1) == "m") {
            val <- as.numeric(stri_sub(val, 2, -2)) * 1000000
        } else if (stri_sub(val, -3, -1) == "Th.") {
            val <- as.numeric(stri_sub(val, 2, -4)) * 1000
        } else {
            val <- as.numeric(stri_sub(val, 2, -1))
        }
        return(val)
    }
    
    # Helper function to set loan status based on fee
    format_fees_and_loans <- function(fee, movement, is_loan, loan_status) {
        if (!is.na(fee)) {
            if (startsWith(fee, "End of loan")) {
                is_loan <- TRUE
                loan_status <- if_else(movement == "In",
                                       "Returning from loan", "End of loan")
                fee <- "$0"
            } else if (startsWith(fee, "Loan fee")) {
                is_loan <- TRUE
                loan_status <- if_else(movement == "In",
                                       "Loan in", "Loan out")
                fee <- gsub("Loan fee:", "", fee)
            } else if (fee == "Loan") {
                is_loan <- TRUE
                loan_status <- if_else(movement == "In",
                                       "Loan in", "Loan out")
                fee <- "$0"
            } else if (fee == "Free transfer") {
                fee <- "$0"
            }
        }
        return(list("fee" = fee,
                    "is_loan" = is_loan,
                    "loan_status" = loan_status))
    }
    
    # Rename columns, format league and window, initialize loan status
    transfers <- df %>%
        rename(
            club = "Club",
            name = "Name",
            age = "Age",
            nationality = "Nationality",
            position = "Position",
            pos = "Pos",
            market_value = "MarketValue",
            club_of_transfer = "ClubInvolved",
            country_of_transfer = "CountryInvolved",
            fee = "Fee",
            movement = "Movement",
            season = "Season",
            window = "Window",
            league = "League",
            profile = "Profile"
        ) %>%
        mutate(
            age = as.integer(age),
            season = as.integer(season),
            league = stri_trans_totitle(gsub("-", " ", league)),
            window = if_else(window == "s", "Summer", "Winter"),
            is_loan = FALSE,
            loan_status = NA_character_
        )
    
    # Temporary dataframe to hold formatted columns
    tidy_fee_and_loan <- bind_rows(mapply(format_fees_and_loans,
                                          transfers$fee,
                                          transfers$movement,
                                          transfers$is_loan,
                                          transfers$loan_status,
                                          SIMPLIFY = FALSE))
    
    # Update fee and loan status, convert fee and market value to numerics
    transfers <- transfers %>%
        mutate(
            fee = tidy_fee_and_loan$fee,
            is_loan = tidy_fee_and_loan$is_loan,
            loan_status = tidy_fee_and_loan$loan_status
        ) %>%
        mutate(
            market_value = sapply(market_value, value_as_numeric),
            fee = sapply(fee, value_as_numeric)
        )
    
    return(transfers)
}
over_loan <- read_csv("201920loandata.csv") %>% tidy_transfers()
transfers <- over_loan %>% filter(!is_loan | fee > 0)

club_spending <-transfers %>%
    filter(movement == "In") %>%
    group_by(club,league) %>%
    summarise(expenditure = sum(fee, na.rm = TRUE)) %>%
    mutate(expenditure = expenditure / 1e6) %>%
    mutate(league=league)


club_sales <- transfers %>%
    filter(movement == "Out") %>%
    group_by(club,league) %>%
    summarize(income = sum(fee, na.rm = TRUE)) %>%
    mutate(income = income / 1e6,
           league=league)

club_record <- merge(club_spending, club_sales) %>%
    mutate(profit = income - expenditure)







