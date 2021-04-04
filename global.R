library(shiny)
library(shinydashboard)
library(fresh) #bootstrap
library(shinyjs) #bootstrap support in dashboardBody and tooltip
library(shinyalert)
library(uuid)
library(DT)
library(odbc)
library(DBI)
library(tidyverse)
# library(RCurl)
library(config)


# basic authentication ----------------------------------------------------

login_details <- data.frame(user = c("datak"),
                            pswd = c("555"))


# db connection -----------------------------------------------------------

#look at config.yml file for db connection either local or in production 
dw <- config::get('dataconnection') 
#wyhen in shinyapps.io in production, select shinyapps, otherwise commentout
Sys.setenv(R_CONFIG_ACTIVE = 'default') # default or shinyapps. see config.yml

#connect to aws server
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = dw$driver,
                      Database = dw$database,
                      UID = dw$uid,
                      PWD = dw$pwd,
                      Server = dw$server,
                      Port = dw$port)

#check tables in con database
# dbListTables(con)


# Input Finance-------------------------------------------------------------------

IQ <- 0.16 # Install and qualification cost. %against DPN
Spare_Maint <- 0.027 # % agaisnt equipment capex. spare and maintenance cost per equipment
FXrate <- 107 #JPY/USD
DL_AnnualSalary <- (3954/FXrate) * 56 *(13*4) #56 is hrs/wk/ppl, 168/3. this is annual salary per ppl
IDL_AnnualSalary <- (5110/FXrate) * 56 *(13*4) #56 is hrs/wk/ppl, this is annual salary per ppl
DL_IDL <- 10 # # of DL per IDL ratio
TTE <- 0 #% against labor. travel and expense
ProcMU <- 0.2 # mark up % for all processing cost
CapExMU <- 0.18 # capex margin %
OH <- 0.27 # overhead, %


# ToolReq -----------------------------------------------------------------

#tool requirement
ToolReq <- function(xSPW, xPH, GU) {
  ToolReq <- xSPW/(xPH * GU * 168)
}


# Labor -------------------------------------------------------------------

#labor cost, for both direct and indirect
LaborCost <- function(ToolReq, DLperToolperDay, DL_IDL, kSPW, TTE) {
  DL <- ToolReq * DLperToolperDay * DL_AnnualSalary/(kSPW*13*4)
  IDL <- ToolReq * DLperToolperDay/DL_IDL * IDL_AnnualSalary/(kSPW*13*4)
  Labor <- (DL + IDL) * (1+TTE)
}


# CS_OH -------------------------------------------------------------------

#facility and ovehead cost
CS_OH <- function(Labor, Mtrl, CapEx, OH, Spare_Maint){
  Result <- (Labor+Mtrl+(Spare_Maint*5)*DPN(CapEx))*OH/(1-OH)
}


# DPN ---------------------------------------------------------------------

#depreciation
DPN <- function(CapEx_1kPSPW){
  Result <- CapEx_1kPSPW/(1000*13*4*5)
}


# MU ----------------------------------------------------------------------

#markup
MU <- function(Labor, Mtrl, OH, CapEx, Spare_Maint, MU, CapExMU){
  Result <- (Labor+Mtrl+OH+DPN(CapEx)*(Spare_Maint*5))*MU/(1-MU)+DPN(CapEx)*CapExMU/(1-CapExMU)
}
