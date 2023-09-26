library(RSelenium)
library(wdman)
library(tidyverse)
library(rvest)
library(netstat)

wpage <- "https://emec.mec.gov.br"
maxpages <- 13

selenium()

selenium(retcommand = TRUE, check = TRUE)


remote_driver <-  rsDriver(
  browser = "firefox",
  port = free_port(),
  verbose = FALSE,
  chromever = NULL
)

remDr <- remote_driver$client

remDr$navigate(wpage)
Sys.sleep(15)

barra <- remDr$findElement(using = "id", "mainFrame")
hh <- barra$getElementAttribute(attrName = "src")[[1]]

remDr$navigate(hh)
Sys.sleep(15)

# tarefa do dia 21/09/2023
consulta_buttom <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div[2]/div/div[3]/ul/li[1]/a")
consulta_buttom$clickElement()
Sys.sleep(15)

