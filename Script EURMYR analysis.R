#START

#Libraries for data cleaning and graph designing

install.packages("ggplot2")
library("ggplot2")

install.packages("dplyr")
library("dplyr")

install.packages("tidyr")
library("tidyr")

#Data from ECB, FED & Malaysian Central Bank

setwd("C:\\Users\\carlo\\Desktop\\Udemy\\R")

Datos <- read.csv("TiposDeCambio.csv")

#Data cleaning of the UER/MYR exchange rat obtained in the ECB database

Datos1 <- Datos[,c("Date", "USD", "MYR")]
DatosTC <- Datos1[1:1776,] #Data only from 01012018

head(DatosTC)

class(DatosTC$Date)
DatosTC$Date <- as.Date(DatosTC$Date, format = "%Y-%m-%d")
class(DatosTC$Date)

DatosTC$MYR <- as.numeric(DatosTC$MYR)

#FIRST GRAPHIC TO CHECK EVERYTHING IS GOING OK

EUR_MYR_Graph <- ggplot(data=DatosTC, aes(x=Date, y=MYR)) +
  geom_line(size=0.1, color="blue") +
  scale_y_continuous(breaks=seq(3.0, 5.5, by = 0.1)) 

#FED Rates 
#Data cleaning

FED_Data <- read.csv("EFFR.csv")
FED_Data

class(FED_Data$Series.Description)
FED_Data$Series.Description <- as.Date(FED_Data$Series.Description, format = "%Y-%m-%d")
class(FED_Data$Series.Description)

FED_Data <- FED_Data %>% rename(Date = Series.Description)
FED_Data <- FED_Data %>% rename(EFFR = Federal.funds.effective.rate)

FED_Data$EFFR <- as.numeric(FED_Data$EFFR)

#Checking with a graph

FED_Data_Graph <- ggplot(data=FED_Data, aes(x=Date, y=EFFR)) +
  geom_line(size=0.1, color="blue") +
  scale_y_continuous(breaks=seq(0.0, 5.5, by = 0.5)) 


#EURO rates (ESTR)

ECB_Data <- read.csv("ECB_Data.csv")

class(ECB_Data$DATE)

ECB_Data$DATE <- as.Date(ECB_Data$DATE, format = "%Y-%m-%d")
class(ECB_Data$DATE)

ECB_Data$TIME.PERIOD <- NULL #Removed non necessary column
ECB_Data <- ECB_Data %>% rename(ESTR = Euro.short.term.rate...Volume.weighted.trimmed.mean.rate..EST.B.EU000A2X2A25.WT.) 
ECB_Data <- ECB_Data %>% rename(Date = DATE) 

ECB_Data$ESTR <- as.numeric(ECB_Data$ESTR)
class(ECB_Data$ESTR)

#Add NA values to 2018 ESTR because ESTR was not create till 2019 and so there's no data

# Create a full sequence of dates
date_range <- seq(from = as.Date("2018-01-01"), 
                  to = as.Date("2019-09-30"), 
                  by = "day")

Dates_frame <- data.frame(Date = date_range, ESTR = 0)

ECB_Data_2018 <- full_join(Dates_frame, ECB_Data)

class(ECB_Data_2018$ESTR)

#Checking ESTR data with a graph

ESTR_Graph <- ggplot(data=ECB_Data_2018, aes(x=Date, y=ESTR)) +
  geom_line(size=0.1, color="green") +
  scale_y_continuous(breaks=seq(0.0, 5.5, by = 0.5)) 

#Merging data

Complete_data <- merge(ECB_Data_2018, combined_data, by = "Date", all.x = TRUE)

# Retain only ESTR.x and remove ESTR.y
Complete_data <- Complete_data %>%
  select(-ESTR.y) %>%
  rename(ESTR = ESTR.x)

summary(Complete_data)

#Adding OPR from Malaysia Central Bank

OPR_Data <- read.csv("OPR MYR.csv")

OPR_Data$Date <- as.Date(OPR_Data$Date, format = "%d/%m/%Y")
class(OPR_Data$Date)

OPR_Data$Rate <- as.numeric(OPR_Data$Rate)
class(ECB_Data$ESTR)

#Adding values to Date in the OPR data because OPR is only updated few times a year
#Adding values to to the rate column

date_range_OPR <- seq(from = as.Date("2018-01-01"), 
                  to = as.Date("2024-12-31"), 
                  by = "day")

Dates_frame_OPR <- data.frame(Date = date_range_OPR, OPR = 0)

OPR_Data_Full <- full_join(Dates_frame_OPR, OPR_Data)
OPR_Data_Full$OPR <- NULL

OPR_Data_Full <- OPR_Data_Full %>%
  fill(Rate, .direction = "down")

#Checking OPR data with a graph

OPR_Graph <- ggplot(data=OPR_Data_Full, aes(x=Date, y=Rate)) +
  geom_line(size=0.1, color="black") +
  scale_y_continuous(breaks=seq(0.0, 5.5, by = 0.5))

#Merging data

Final_Data <- merge.data.frame(Complete_data, OPR_Data_Full, by.x = "Date")
Final_Data <- Final_Data %>% rename(OPR = Rate)

class(Final_Data$OPR)

#Filling NA values in case some are there

Final_Data <- Final_Data %>%
  fill(ESTR, EFFR, USD, MYR, OPR, .direction = "down")

class(Final_Data$Date) #Check class is OK
head(Final_Data) #Preview

###############################################################################

#All graphs so far

#Individual Ones
EUR_MYR_Graph #Exchange rate Data
FED_Data_Graph #EFFR Data
ESTR_Graph #EURO Data
OPR_Graph #Malaysian Central Bank Data

######Currency rates vs EUR/MYR#####
Final_Data <- Final_Data %>%
  rename(`EURMYR` = "EUR/MYR")

#All rates vs EUR/MYR exchange rate

Rates_vs_EUR_MYR <- ggplot(data = Final_Data, aes(x = Date)) +
  # Plot EFFR on the primary y-axis
  geom_line(aes(y = EFFR, color = "EFFR"), size = 0.5) +
  # Plot MYR, rescaled to match the primary axis
  geom_line(aes(y = (EURMYR - myr_min) * scale_factor_myr + effr_min, color = "EURMYR"), size = 0.5) +
  # Plot ESTR, rescaled to match the primary axis
  geom_line(aes(y = ESTR, color = "ESTR"), size = 0.5) +
  # Plot OPR, rescaled to match the primary axis
  geom_line(aes(y = OPR, color = "OPR"), size = 0.5) +
  # Custom colors for lines
  scale_color_manual(values = c("EFFR" = "blue", "EURMYR" = "red", "ESTR" = "green", "OPR" = "black")) +
  # Primary y-axis
  scale_y_continuous(
    name = "Currency interest rates",
    sec.axis = sec_axis(~(. - effr_min) / scale_factor_myr + myr_min, name = "EUR/MYR")
  ) +
  theme_minimal() +
  labs(title = "EUR/MYR vs ESTR vs EFFR vs OPR", x = "Date", color = "Series") +
  theme(legend.position = "bottom", axis.title.y.right = element_text(color = "red"))

##Each rate vs EUR/MYR

#EFFR vs EURMYR
EFFR_vs_EURMYR <- ggplot(data = Final_Data, aes(x = Date)) +
  # Plot EFFR on the primary y-axis
  geom_line(aes(y = EFFR, color = "EFFR"), size = 0.5) +
  # Plot MYR, rescaled to match the primary axis
  geom_line(aes(y = (EURMYR - myr_min) * scale_factor_myr + effr_min, color = "EURMYR"), size = 0.5)+
  scale_color_manual(values = c("EFFR" = "blue", "EURMYR" = "red")) +
  # Primary y-axis
  scale_y_continuous(
    name = "EFFR(USD) rate",
    sec.axis = sec_axis(~(. - effr_min) / scale_factor_myr + myr_min, name = "EUR/MYR")
  ) +
  theme_minimal() +
  labs(title = "EUR/MYR vs EFFR (USD)", x = "Date", color = "Series") +
  theme(legend.position = "bottom", axis.title.y.right = element_text(color = "red"))

#ESTR vs EURMYR

ESTR_vs_EURMYR <- ggplot(data = Final_Data, aes(x = Date)) +
  # Plot EFFR on the primary y-axis
  geom_line(aes(y = ESTR, color = "ESTR"), size = 0.5) +
  # Plot MYR, rescaled to match the primary axis
  geom_line(aes(y = (EURMYR - myr_min) * scale_factor_myr + estr_min, color = "EURMYR"), size = 0.5)+
  scale_color_manual(values = c("EURMYR" = "red", "ESTR" = "green")) +
  # Primary y-axis
  scale_y_continuous(
    name = "ESTR(EURO) rate",
    sec.axis = sec_axis(~(. - estr_min) / scale_factor_myr + myr_min, name = "EUR/MYR")
  ) +
  theme_minimal() +
  labs(title = "EUR/MYR vs ESTR (EURO)", x = "Date", color = "Series") +
  theme(legend.position = "bottom", axis.title.y.right = element_text(color = "red"))


#OPR vs EURMYR

#Need  to adjust things to have the y axis scale fit

min_eur_myr <- min(Final_Data$EURMYR, na.rm = TRUE)
max_eur_myr <- max(Final_Data$EURMYR, na.rm = TRUE)
min_opr2 <- min(Final_Data$OPR, na.rm = TRUE)
max_opr2 <- max(Final_Data$OPR, na.rm = TRUE)


OPR_vs_EURMYR <- ggplot(data = Final_Data, aes(x = Date)) +
  # Plot EFFR on the primary y-axis
  geom_line(aes(y = OPR, color = "OPR"), size = 0.5) +
  # Plot MYR, rescaled to match the primary axis
  geom_line(aes(y = (EURMYR - myr_min) * (max_opr2 - min_opr2) / (max_eur_myr - min_eur_myr) + min_opr2, color = "EUR/MYR"), size = 0.5) +
  scale_color_manual(values = c("EUR/MYR" = "red", "OPR" = "black")) +
  # Primary y-axis
  scale_y_continuous(
    name = "OPR(MYR) rate",
    sec.axis = sec_axis(~(. - min_opr2) * (max_eur_myr - min_eur_myr) / (max_opr2 - min_opr2) + min_eur_myr, name = "EUR/MYR")
  ) +
  theme_minimal() +
  labs(title = "EUR/MYR vs OPR (MYR)", x = "Date", color = "Series") +
  theme(
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "red"),  # Make EUR/MYR label red
    axis.title.y.left = element_text(color = "black") # Keep OPR label black
  )


#All graphs so far

#Individual Ones
EUR_MYR_Graph #Exchange rate Data
FED_Data_Graph #EFFR Data
ESTR_Graph #EURO Data
OPR_Graph #Malaysian Central Bank Data

#All together
Rates_vs_EUR_MYR

#Each rate vs EURMYR
EFFR_vs_EURMYR #USD EFFR vs EURMYR
ESTR_vs_EURMYR #EURO ESTR vs EURMYR
OPR_vs_EURMYR #MYR OPR vs EURMYR

#####################MAKING THEM INTERACTIVE!!#################################

install.packages("plotly")
library(plotly)

EUR_MYR_Graph_Interactive <- ggplotly(EUR_MYR_Graph)
FED_Data_Graph_Interactive <- ggplotly(FED_Data_Graph)
ESTR_Graph_Interactive <- ggplotly(ESTR_Graph)
OPR_Graph_Interactive <- ggplotly(OPR_Graph)

Rates_vs_EUR_MYR_Interactive <- ggplotly(Rates_vs_EUR_MYR)

EFFR_vs_EURMYR_Interactive <- ggplotly(EFFR_vs_EURMYR)
ESTR_vs_EURMYR_Interactive <- ggplotly(ESTR_vs_EURMYR)
OPR_vs_EURMYR_Interactive<- ggplotly(OPR_vs_EURMYR)

#Interactive individual graphs

EUR_MYR_Graph_Interactive
FED_Data_Graph_Interactive
ESTR_Graph_Interactive
OPR_Graph_Interactive 

#Problem with combined interactive graphs
#Y axis on the right does not appear and the EUR/MYR value appears scaled


###################TRY OTHER TYPE OF INTERACTIVITY##############################

install.packages("shiny")
library(shiny)

install.packages("jquerylib")
library(jquerylib)

#######
max_opr <- max_opr2
min_opr <- min_opr2


#Create a simple Shiny app to handle click events
#FED DATA
#############
ui_FED_Data_Graph_Interactive <- fluidPage(
  titlePanel("Interactive plot"),
  plotlyOutput("FED_Data_Graph_Interactive"),
  verbatimTextOutput("click_info")  # Display clicked information
)

server_FED_Data_Graph_Interactive <- function(input, output) {
  
  # Render the plot
  output$FED_Data_Graph_Interactive <- renderPlotly({
    ggplot_obj <- ggplot(data = Final_Data, aes(x = Date)) +
      geom_line(aes(y = EFFR, color = "EFFR"), size = 0.5) +
      geom_line(aes(y = (EURMYR - min_eur_myr) * (effr_max - effr_min) / (max_eur_myr - min_eur_myr) + effr_min, color = "EUR/MYR"), size = 0.5) +
      scale_color_manual(values = c("EUR/MYR" = "red", "EFFR" = "blue")) +
      scale_y_continuous(
        name = "EFFR (USD) rate",
        sec.axis = sec_axis(~(. - effr_min) * (max_eur_myr - min_eur_myr) / (effr_max - effr_min) + min_eur_myr, name = "EUR/MYR")
      ) +
      theme_minimal() +
      labs(title = "EUR/MYR vs EFFR (USD)", x = "Date", color = "Series") +
      theme(
        legend.position = "bottom",
        axis.title.y.right = element_text(color = "red"),
        axis.title.y.left = element_text(color = "blue")
      )
    
    ggplotly(ggplot_obj)
  })
  
  # Capture click event and display info
  output$click_info <- renderPrint({
    click_data <- event_data("plotly_click")
    if (!is.null(click_data)) {
      # Get the clicked x-coordinate (Date)
      x_val <- as.Date(click_data$x, origin = "1970-01-01")  # Ensure it's a proper date
      
      # Find the row in the data corresponding to the clicked Date
      clicked_row <- Final_Data[Final_Data$Date == x_val, ]
      
      if (nrow(clicked_row) > 0) {
        # Extract values from the clicked row
        eur_myr_val <- clicked_row$EURMYR
        effr_val <- clicked_row$OPR
        
        # Print the information with formatted date
        cat("Date: ", format(x_val, "%d-%m-%Y"), "\n")
        cat("EFFR: ", effr_val, "\n")
        cat("EUR/MYR: ", eur_myr_val, "\n")
      } else {
        cat("No data available for the selected date.\n")
      }
    } else {
      cat("Click on the graph to see the values.\n")
    }
  })}


#FED DATA
#OPR DATA
#########

# UI: Display the interactive plot and clicked value info
ui_OPR_Graph_Interactive <- fluidPage(
  titlePanel("Interactive Plot with Real EUR/MYR Values"),
  plotlyOutput("OPR_Graph_Interactive"),
  verbatimTextOutput("click_info")  # Show clicked information
)

# Server: Generate the plot and handle click events
server_OPR_Graph_Interactive <- function(input, output) {
  
  # Render the interactive plot
  output$OPR_Graph_Interactive <- renderPlotly({
    ggplot_obj <- ggplot(data = Final_Data, aes(x = Date)) +
      geom_line(aes(y = OPR, color = "OPR"), size = 0.5) +
      geom_line(aes(y = (EURMYR - min_eur_myr) * (max_opr - min_opr) / (max_eur_myr - min_eur_myr) + min_opr, color = "EUR/MYR"), size = 0.5) +
      scale_color_manual(values = c("EUR/MYR" = "red", "OPR" = "black")) +
      scale_y_continuous(
        name = "OPR(MYR) rate",
        sec.axis = sec_axis(~(. - min_opr) * (max_eur_myr - min_eur_myr) / (max_opr - min_opr) + min_eur_myr, name = "EUR/MYR")
      ) +
      theme_minimal() +
      labs(title = "EUR/MYR vs OPR (MYR)", x = "Date", color = "Series") +
      theme(
        legend.position = "bottom",
        axis.title.y.right = element_text(color = "red"),
        axis.title.y.left = element_text(color = "black")
      )
    
    ggplotly(ggplot_obj)
  })
  
  # Display the clicked values, including the actual EUR/MYR value
  output$click_info <- renderPrint({
    click_data <- event_data("plotly_click")
    if (!is.null(click_data)) {
      # Get the clicked x-coordinate (Date)
      x_val <- as.Date(click_data$x, origin = "1970-01-01")  # Ensure it's a proper date
      
      # Find the row in the data corresponding to the clicked Date
      clicked_row <- Final_Data[Final_Data$Date == x_val, ]
      
      if (nrow(clicked_row) > 0) {
        # Extract values from the clicked row
        eur_myr_val <- clicked_row$EURMYR
        opr_val <- clicked_row$OPR
        
        # Print the information with formatted date
        cat("Date: ", format(x_val, "%d-%m-%Y"), "\n")
        cat("OPR: ", opr_val, "\n")
        cat("EUR/MYR: ", eur_myr_val, "\n")
      } else {
        cat("No data available for the selected date.\n")
      }
    } else {
      cat("Click on the graph to see the values.\n")
    }
  })
  
}#EUR_MYR_Graph_Interactive##OPR DATA
#OPR DATA (DID IT WITH EUR/MYR DATA LOL!)
#ESTR DATA
#ESTR DATA
############

# Create a simple Shiny app to handle click events
ui_ESTR_Graph_Interactive <- fluidPage(
  titlePanel("Interactive plot"),
  plotlyOutput("ESTR_Graph_Interactive"),
  verbatimTextOutput("click_info")  # Display clicked information
)

server_ESTR_Graph_Interactive <- function(input, output) {
  
  # Render the plot
  output$ESTR_Graph_Interactive <- renderPlotly({
    ggplot_obj <- ggplot(data = Final_Data, aes(x = Date)) +
      geom_line(aes(y = ESTR, color = "ESTR"), size = 0.5) +
      geom_line(aes(y = (EURMYR - min_eur_myr) * (estr_max - estr_min) / (max_eur_myr - min_eur_myr) + estr_min, color = "EUR/MYR"), size = 0.5) +
      scale_color_manual(values = c("EUR/MYR" = "red", "ESTR" = "blue")) +
      scale_y_continuous(
        name = "ESTR (EURO) rate",
        sec.axis = sec_axis(~(. - estr_min) * (max_eur_myr - min_eur_myr) / (estr_max - estr_min) + min_eur_myr, name = "EUR/MYR")
      ) +
      theme_minimal() +
      labs(title = "EUR/MYR vs ESTR (EURO)", x = "Date", color = "Series") +
      theme(
        legend.position = "bottom",
        axis.title.y.right = element_text(color = "red"),
        axis.title.y.left = element_text(color = "blue")
      )
    
    ggplotly(ggplot_obj)
  })
  
  # Capture click event and display info
  output$click_info <- renderPrint({
    click_data <- event_data("plotly_click")
    if (!is.null(click_data)) {
      # Get the clicked x-coordinate (Date)
      x_val <- as.Date(click_data$x, origin = "1970-01-01")  # Ensure it's a proper date
      
      # Find the row in the data corresponding to the clicked Date
      clicked_row <- Final_Data[Final_Data$Date == x_val, ]
      
      if (nrow(clicked_row) > 0) {
        # Extract values from the clicked row
        eur_myr_val <- clicked_row$EURMYR
        estr_val <- clicked_row$ESTR
        
        # Print the information with formatted date
        cat("Date: ", format(x_val, "%d-%m-%Y"), "\n")
        cat("ESTR: ", estr_val, "\n")
        cat("EUR/MYR: ", eur_myr_val, "\n")
      } else {
        cat("No data available for the selected date.\n")
      }
    } else {
      cat("Click on the graph to see the values.\n")
    }
  })}



#ESTR DATA

#All in one
#################

ui_Rates_vs_EUR_MYR <- fluidPage(
  titlePanel("Interactive EUR/MYR vs ESTR vs EFFR vs OPR Plot"),
  plotlyOutput("Rates_vs_EUR_MYR"),
  verbatimTextOutput("click_info")  # Display values at clicked point
)

server_Rates_vs_EUR_MYR <- function(input, output) {
  
  # Render the interactive plot
  output$Rates_vs_EUR_MYR <- renderPlotly({
    ggplot_obj <- ggplot(data = Final_Data, aes(x = Date)) +
      geom_line(aes(y = EFFR, color = "EFFR"), size = 0.5) +
      geom_line(aes(y = (EURMYR - myr_min) * scale_factor_myr + effr_min, color = "EURMYR"), size = 0.5) +
      geom_line(aes(y = ESTR, color = "ESTR"), size = 0.5) +
      geom_line(aes(y = OPR, color = "OPR"), size = 0.5) +
      scale_color_manual(values = c("EFFR" = "blue", "EURMYR" = "red", "ESTR" = "green", "OPR" = "black")) +
      scale_y_continuous(
        name = "Currency interest rates",
        sec.axis = sec_axis(~(. - effr_min) / scale_factor_myr + myr_min, name = "EUR/MYR")
      ) +
      theme_minimal() +
      labs(title = "EUR/MYR vs ESTR vs EFFR vs OPR", x = "Date", color = "Series") +
      theme(
        legend.position = "bottom",
        axis.title.y.right = element_text(color = "red"),
        axis.title.y.left = element_text(color = "black")
      )
    
    ggplotly(ggplot_obj)
  })
  
  # Display values at the clicked point
  output$click_info <- renderPrint({
    click_data <- event_data("plotly_click")
    if (!is.null(click_data)) {
      # Get the clicked x-coordinate (Date)
      x_val <- as.Date(click_data$x, origin = "1970-01-01")  # Ensure proper date format
      
      # Find the row in the data corresponding to the clicked Date
      clicked_row <- Final_Data[Final_Data$Date == x_val, ]
      
      if (nrow(clicked_row) > 0) {
        # Extract values from the clicked row
        effr_val <- clicked_row$EFFR
        eur_myr_val <- clicked_row$EURMYR
        estr_val <- clicked_row$ESTR
        opr_val <- clicked_row$OPR
        
        # Print the information with formatted date
        cat("Date: ", format(x_val, "%d-%m-%Y"), "\n")
        cat("EFFR: ", effr_val, "\n")
        cat("EUR/MYR: ", eur_myr_val, "\n")
        cat("ESTR: ", estr_val, "\n")
        cat("OPR: ", opr_val, "\n")
      } else {
        cat("No data available for the selected date.\n")
      }
    } else {
      cat("Click on the graph to see the values.\n")
    }
  })
}



######################

# EUR_MYR_Graph_Interactive
shinyApp(ui = ui_OPR_Graph_Interactive, server = server_OPR_Graph_Interactive)

# FED_Data_Graph_Interactive
shinyApp(ui = ui_FED_Data_Graph_Interactive, server = server_FED_Data_Graph_Interactive)

# ESTR_Graph_Interactive
shinyApp(ui = ui_ESTR_Graph_Interactive, server = server_ESTR_Graph_Interactive)

# Currency rates vs EUR/MYR
shinyApp(ui = ui_Rates_vs_EUR_MYR, server = server_Rates_vs_EUR_MYR)


