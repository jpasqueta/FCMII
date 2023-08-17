library(tidyverse)
library(pdftools)

# Lendo PDF

pdf <- pdf_text("C:/Users/jessi/Downloads/cadastro.pdf")
pdf <- str_split_1(pdf, "\\n")
pdf

# extraindo o CEP

z <- pdf[grepl("CEP",pdf)]

gsub("\\D","",z) # aparece somente o número do CEP


