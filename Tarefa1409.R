# Tarefa 14-09-23

library(tidyverse)  
library(lubridate)   

dados <- readLines("RACA_COR.CNV")

# Pré-processamento dos dados

dados <- gsub("10 2 L", "ID  RAÇA/COR  CÓDIGO", dados) # Substituir "10 2 L" por "ID  RAÇA/COR  CÓDIGO" nas linhas dos dados
dados <- gsub("\xc7", "Ç", dados) # Substituir "Ç"
dados <- gsub("<c3>", "Ã", dados) # Substituir "Ã"
dados <- gsub("RA<c7>A\\/COR", "", dados) # Remover "RAÇA/COR" em excesso 
dados <- gsub("RAÇA/COR.*)", "INDEVIDOS", dados) # Para agrupar os "INDEVIDOS"
dados <- gsub("RAÇA/COR NÃO EXIGIDO", "NÃO EXIGIDO", dados) # Substituir "RAÇA/COR NÃO EXIGIDO" por "NÃO EXIGIDO"
dados <- gsub("1M,1G,1C,DE, D,87", "06,09,1M,1G,1C,DE,D,87", dados) # Substituir "1M,1G,1C,DE, D,87" por "06,09,1M,1G,1C,DE,D,87"
dados <- dados[-c(9, 10)] # Remover as linhas de índice 9 e 10 que já foram agrupadas nos "INDEVIDOS"

erro <- str_extract_all(dados[8], "(?<=\\d{2}).*$") # Para remover a virgula
dados[8] <- gsub(erro, "", dados[8]) # Removendo a virgula 

dados <- trimws(dados, "both") # Remover espaços em branco no início e no final das linhas
dados_divididos <- strsplit(dados, "\\s{2,}")

# Extrair as colunas RAÇA/COR e CÓDIGO 
raca_cor <- sapply(dados_divididos[-1], `[`, 2)
codigo <- sapply(dados_divididos[-1], `[`, 3)

# Criar o dataframe apenas com as colunas RAÇA/COR e CÓDIGO
df <- data.frame(`RAÇA/COR` = raca_cor, CÓDIGO = codigo)

print(df)


# Versão Thomas -----------------------------------------------------------------

# Lê os dados de um arquivo chamado "RACA_COR.CNV", ignorando a primeira linha, usando ";" como separador e encoding "latin1".
tab <- read.table("RACA_COR.CNV", skip = 1, sep = ";", encoding = "latin1")

# Extrai a coluna V1 dos dados lidos do arquivo e a atribui a tab$V1
tab$V1

# Extrai os códigos que começam com "1M" da décima linha da coluna V1,
# divide-os em uma lista usando vírgula como separador e remove espaços em branco.
cod_prob <- str_extract(tab$V1[10], "1M.*") %>% 
  str_split_1(",") %>% trimws("both")

# Cria um vetor com a descrição "RAÇA/COR  (OUTROS INDEVIDOS)" com base no número de códigos obtidos anteriormente.
desc_prob <- rep("RAÇA/COR  (OUTROS INDEVIDOS)", length(cod_prob))

# Remove vírgulas e espaços em branco dos códigos na coluna V1, 
# extrai os últimos dígitos (possivelmente códigos) e remove espaços em branco.
cod_bom <- tab$V1 %>% 
  gsub(",", "", .) %>% 
  str_extract("\\d+\\s*$") %>% trimws("both")

# Seleciona os primeiros nove códigos na coluna V1.
cod_bom <- cod_bom[1:9]

# Remove vírgulas e espaços em branco dos códigos nas nove primeiras linhas da coluna V1,
# extrai o texto entre o início e os dois últimos dígitos, 
# remove os primeiros dígitos e, finalmente, remove espaços em branco.
desc_bom <- tab$V1[1:9] %>% 
  gsub(",", "", .) %>% 
  trimws("both") %>% 
  str_extract("(?<=^\\d).*(?=\\d{2}$)") %>% 
  gsub("^\\d+", "", .) %>% 
  trimws("both") 

# Cria um dataframe com as descrições e códigos obtidos anteriormente.
df <- data.frame(Descrição  = c(desc_prob, desc_bom), Código = c(cod_bom, cod_prob)) # Arrumei a ordem