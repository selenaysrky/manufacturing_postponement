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
nov_po_filtered <- filter(nov_po, `Gövde Stok` == "0" & `Kabin Stok` == "0")


# Load required libraries
library(lubridate)
library(dplyr)

# Create a data frame with all the days of 2024
days_2024 <- data.frame(Date = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "days"))

# Extract month and year information
days_2024 <- days_2024 %>%
  mutate(Month = month(Date), Year = year(Date))

# Change date format to "31/12/2024"
days_2024$Date <- format(days_2024$Date, "%d/%m/%Y")

# Split the data frame by month
days_by_month <- split(days_2024, f = days_2024$Month)

# Display the first few rows of each month's data frame
lapply(days_by_month, function(month_df) head(month_df))

