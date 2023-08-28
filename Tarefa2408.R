# Tarefa 24-08-23
# Organizar o endereço:
# Rua/local
# Número
# Cidade/planeta

library(tidyverse)
library(pdftools)
library(lubridate)

# Lendo PDF

pdf <- pdf_text("C:/Users/jessi/Downloads/cadastro.pdf")
pdf <- str_split_1(pdf, "\\n")
pdf

# Extrair endereço

address <- pdf[grepl("Endereço",pdf)]
address

ad1 <- str_extract(address,".*,") # retorna apenas o nome da rua e número
gsub(",","",ad1)

ad2 <- str_extract(address, "\\,.*$")
ad2 <- gsub(".*,","",ad2)
ad2 <- gsub("CEP:.*$","",ad2)
ad2

data_frame <- data.frame(Endereço = ad1, Localização = ad2)
data_frame
