#Read the imported Excel file
library(readxl)
november_orders <- read_excel("C:/Users/tt055367/Desktop/Bitirme/Data/november_orders.xlsx")
View(november_orders)

#Rename the file
nov_po <- november_orders

#Check if there exists NA's
sum(is.na(nov_po$`Müşteri siparişi`))
sum(is.na(nov_po$'Ötelenmemesi Gereken Sipariş'))
sum(is.na(nov_po$'Gövde Stok'))
sum(is.na(nov_po$'Kabin Stok'))
sum(is.na(nov_po$'TR6 Gövde'))
sum(is.na(nov_po$'Üretim versiyonu'))
sum(is.na(nov_po$'Satış organizasyonu'))
sum(is.na(nov_po$'Fiili btş.trm.'))
sum(is.na(nov_po$'Pln.bşl.termini'))
sum(is.na(nov_po$'Yeni Tarih'))
sum(is.na(nov_po$'Seri'))

#Remove the NA's
clean_siparis <- na.omit(nov_po$`Müşteri siparişi`)
clean_otelemeYok <- na.omit(nov_po$`Ötelenmemesi Gereken Sipariş`)
clean_govde <- na.omit(nov_po$'Gövde Stok')
clean_kabin <- na.omit(nov_po$'Kabin Stok')
clean_TR6 <- na.omit(nov_po$'TR6 Gövde')
clean_versiyon <- na.omit(nov_po$'Üretim versiyonu')
clean_organizasyon <- na.omit(nov_po$'Satış organizasyonu')
clean_bitis <- na.omit(nov_po$'Fiili btş.trm.')
clean_baslangic <- na.omit(nov_po$'Pln.bşl.termini')
clean_yeniTarih <- na.omit(nov_po$'Yeni Tarih')
clean_seri <- na.omit(nov_po$'Seri')

#Check if the removal is successful
sum(is.na("TR6 Gövde"))
sum(is.na("Gövde Stok"))


# Orders without a body or cabin should be postponed, filter them
nov_po_filtered <- filter(nov_po, 'Gövde Stok' == "0" & 'Kabin Stok' == "0")


# Load required libraries
library(lubridate)
library(dplyr)

# Create a data frame with all the days of 2024
days_2024 <- data.frame(Date = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "days"))

# Extract month and year information
days_2024 <- days_2024 %>%
  mutate(Month = month(Date), Year = year(Date))

# Split the data frame by month
days_by_month <- split(days_2024, f = days_2024$Month)

# Display the first few rows of each month's data frame
lapply(days_by_month, function(month_df) head(month_df))

# Define a vector of holidays
public_holidays_2024 <- c("2024-01-01", "2024-04-09", "2024-04-10", "2024-04-11", "2024-04-12", "2024-04-23", "2024-05-01", "2024-05-19", "2024-06-15", "2024-06-16", "2024-06-17", "2024-06-18", "2024-06-19", "2024-07-15", "2024-08-30", "2024-10-29", "2024-12-31")

# Generate Sundays using the dates_2024 vector
sundays_2024 <- days_2024[weekdays(days_2024$Date) == "Sunday", ]

# Combine public holidays and Sundays
all_holidays <- c(public_holidays_2024, sundays_2024$Date)

# Add a column indicating whether each date is a holiday
days_2024$IsHoliday <- as.Date(days_2024$Date) %in% as.Date(all_holidays)

# Change date format to "31/12/2024"
days_2024$Date <- format(days_2024$Date, "%d/%m/%Y")

# Find the work days
work_days <- days_2024 %>%
  filter(!(Date %in% all_holidays) & IsHoliday == TRUE)


library(shiny)
library(readxl)
library(dplyr)

ui <- fluidPage(
  titlePanel("November Orders Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel File", accept = c(".xlsx")),
      tags$hr(),
      checkboxGroupInput("check_columns", "Columns to Check", choices = colnames(november_orders)),
      tags$hr(),
      actionButton("btn_process", "Process Data"),
      tags$hr(),
      textOutput("message")
    ),
    
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    data(read_excel(input$file$datapath))
  })
  
  observeEvent(input$btn_process, {
    if (!is.null(data())) {
      columns_to_check <- input$check_columns
      check_na <- lapply(columns_to_check, function(col) sum(is.na(data()[[col]])))
      output$message <- renderText({
        paste("NA count for each selected column:", sapply(check_na, toString))
      })
      
      # Perform additional operations if needed
      
      # Display cleaned data
      output$table <- renderTable({
        na.omit(data())
      })
    }
  })
}

shinyApp(ui, server)
