#Reading the imported Excel file
library(readxl)
november_orders <- read_excel("C:/Users/tt055367/Desktop/Bitirme/Data/november_orders.xlsx")
View(november_orders)

#Renaming the file
nov_po <- november_orders

#Checking if there exists NA's
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

#Removing the NA's
clean_siparis <- na.omit(nov_po$`Müşteri siparişi`)
sum(is.na(clean_siparis))
clean_otelemeYok <- na.omit(nov_po$`Ötelenmemesi Gereken Sipariş`)
sum(is.na(clean_otelemeYok))
clean_govde <- na.omit(nov_po$'Gövde Stok')
sum(is.na(clean_govde))
clean_kabin <- na.omit(nov_po$'Kabin Stok')
sum(is.na(clean_kabin))
clean_TR6 <- na.omit(nov_po$'TR6 Gövde')
sum(is.na(clean_TR6))
clean_versiyon <- na.omit(nov_po$'Üretim versiyonu')
sum(is.na(clean_versiyon))
clean_organizasyon <- na.omit(nov_po$'Satış organizasyonu')
sum(is.na(clean_organizasyon))
clean_bitis <- na.omit(nov_po$'Fiili btş.trm.')
sum(is.na(clean_bitis))
clean_baslangic <- na.omit(nov_po$'Pln.bşl.termini')
sum(is.na(clean_baslangic))
clean_yeniTarih <- na.omit(nov_po$'Yeni Tarih')
sum(is.na(clean_yeniTarih))
clean_seri <- na.omit(nov_po$'Seri')
sum(is.na(clean_seri))

#Checking if the removal is successful
sum(is.na("TR6 Gövde"))
sum(is.na("Gövde Stok"))


# Orders without a body or cabin should be postponed, filter them
nov_po_filtered <- filter(nov_po, `Gövde Stok` == "0" & `Kabin Stok` == "0")
print(nov_po_filtered)

# Adding a new column for prioritized orders
nov_po <- mutate(nov_po, "Customer Priority Orders" = NA)
nov_po


