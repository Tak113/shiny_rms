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