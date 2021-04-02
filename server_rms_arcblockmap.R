#enable shiny alert
useShinyalert()

#make abm_df reactive on the user event
abm_df <- reactiveValues()

#function to create data under abm_df and load "equip" database
  arc_name_selection_abm <- dbReadTable(con, "arc") %>% select(-ArcName_Note) #for a selection
  pb_name_selection_abm <- dbReadTable(con, "procblock") %>% select(-c(MfgFomat,ProcBlock_Note)) #for a selection
  abm_df$data <- dbReadTable(con, "arcblockmap") %>%  # for a db update
    left_join(arc_name_selection_abm, by="uuid_arc") %>% #add ArcName from uuid_arc
    left_join(pb_name_selection_abm, by="uuid_pb")     #add ProcBlock from uuid_pb

#for DT reload, tried to preserve table UI (eg. sort, page) but does not work so far
# proxy <- DT::dataTableProxy("abm_table")

# render selectinput in UI ------------------------------------------------
#in shiny app, the ui function runs before the server function. need to move to server making this as reactive expression
output$ui_abm_selection <- renderUI({
  selectInput("abm_selection",
              label = "Product Name;",
              choices = c("All", unique(arc_name_selection_abm$ArcName)),
              selected = "All",
              multiple = TRUE)
})

# form prep ---------------------------------------------------------------

#Form function for data entry, to be called in both 'add' and 'edit' event
#button_id_abm is a variable and button id which will navigate to each event
entry_form_abm <- function(button_id_abm) {
  
  showModal(
    modalDialog(
      title = strong("Entry Form"),
      fluidRow(
        column(6,
               numericInput("Seq_PB", "ProcBlock Sequence", value = "", min = 0),
               selectInput("ArcName", "Product Name", choices = c("",unique(as.character(arc_name_selection_abm$ArcName)))),
               selectInput("ProcBlock", "ProcBlock", choices = c("",unique(as.character(pb_name_selection_abm$ProcBlock))))
        )
      ),
      footer = tagList(
        actionButton(button_id_abm, "Submit"),
        modalButton("Cancel")
      ),
      size = "m", #window size. m:medium
      easyClose = TRUE
    )
  )
  
}

#Save function for form data into data frame format from form function, finding uuid for ArcName and ProcBlock based on user's selection on ArcName and ProcBlock
formData_abm <- reactive({
  
  formData_abm <- data.frame(
    uuid_abm = UUIDgenerate(),
    Seq_PB = input$Seq_PB,
    ArcName = input$ArcName,
    ProcBlock = input$ProcBlock
  ) %>% 
    mutate_each(funs(as.numeric), c(Seq_PB)) %>% #match variable type to original database
    mutate_each(funs(as.character), c(uuid_abm, ArcName, ProcBlock)) %>%  #match variable type to original database
    left_join(arc_name_selection_abm, by="ArcName") %>% #add uuid_arc from ArcName
    left_join(pb_name_selection_abm, by="ProcBlock")  #add uuid_pb from ProcBlock
  
  #paste0(str(formData_abm))
  return(formData_abm)
  
})


# Add ---------------------------------------------------------------------

#Add data button
observeEvent(input$add_button_abm, {
  entry_form_abm("submit_new_abm")
})
observeEvent(input$submit_new_abm, {
  abm_df$data <- bind_rows(abm_df$data, formData_abm()) #add new record at bottom
  shinyjs::reset("entry_form_abm")
  removeModal()
})  


# Delete ------------------------------------------------------------------

observeEvent(input$delete_button_abm, {
  showModal(
    if(length(input$abm_table_rows_selected)>=1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Are you sure you would like to delete selected row(s)?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_delete_abm", "Yes")
        ),
        easyClose = TRUE
      ) 
    } else if(length(input$abm_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select row(s)"),
        easyClose = TRUE
      )
    }
  )
})

observeEvent(input$submit_delete_abm, {
  uuid_selection_abm <- interim_table_abm()[input$abm_table_rows_selected, "uuid_abm"] #identify uuid for the selected row
  uuid_index_abm <- match(uuid_selection_abm, abm_df$data$uuid_abm) #get matched row index using UUID
  abm_df$data <- abm_df$data[-uuid_index_abm,] #delete row
  # cat(file=stderr(), "test", uuid_index_abm, "\n")
  removeModal()
  # DT::replaceData(proxy, interim_table_abm(), resetPaging = FALSE)
})

# Copy --------------------------------------------------------------------

observeEvent(input$copy_button_abm, {
  if(length(input$abm_table_rows_selected)>=1) {
    row_selection_abm <- interim_table_abm()[input$abm_table_rows_selected, "uuid_abm"] #identify uuid for the selected rows
    copy_df_abm <- abm_df$data %>% filter(uuid_abm %in% row_selection_abm) #filter by selected uuid rows
    copy_df_abm$uuid_abm <- replicate(nrow(copy_df_abm), UUIDgenerate()) #replace with new uuid for copied rows
    abm_df$data <- bind_rows(abm_df$data, copy_df_abm) #add copy records at bottom
  } else if(length(input$abm_table_rows_selected)<1) {
    showModal(
      modalDialog(
        h3(strong("Warning")),
        paste("Please select row(s)"),
        easyClose = TRUE
      )
    ) 
  } 
})

# Edit --------------------------------------------------------------------

#modal window. update form based on a selected row
observeEvent(input$edit_button_abm, {
  
  showModal(
    if(length(input$abm_table_rows_selected)>1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select only one row"),
        easyClose = TRUE
      )
    } else if(length(input$abm_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select a row"),
        easyClose = TRUE
      )
    }
  )
  
  if(length(input$abm_table_rows_selected)==1) {
    
    #load entry form
    entry_form_abm("submit_edit_abm")
    
    #update each value in entry form based on selected row
    updateNumericInput(session, "Seq_PB", value = interim_table_abm()[input$abm_table_rows_selected, "Seq_PB"])
    updateSelectInput(session, "ArcName", selected = interim_table_abm()[input$abm_table_rows_selected, "ArcName"],
                      choices = c("",unique(as.character(arc_name_selection_abm$ArcName))))
    updateSelectInput(session, "ProcBlock", selected = interim_table_abm()[input$abm_table_rows_selected, "ProcBlock"],
                      choices = c("",unique(as.character(pb_name_selection_abm$ProcBlock))))
    
  }
})

#update to database
observeEvent(input$submit_edit_abm, {
  uuid_selection_abm <- interim_table_abm()[input$abm_table_rows_selected, "uuid_abm"] #identify uuid for the selected row
  uuid_index_abm <- match(uuid_selection_abm, abm_df$data$uuid_abm) #get matched row index using UUID
  # cat(file=stderr(), "test", abm_df$data[uuid_index_abm,1], "\n")
  abm_df$data[uuid_index_abm, 2:6] <- formData_abm()[,2:6] #replace data with updated data. make sure column Seq_PB matches. uuid_abm is 1st column in formData_abm while last column in database ##### RDB setting
  shinyjs::reset("entry_form_abm")
  removeModal()
})

# Fetch -------------------------------------------------------------------

observeEvent(input$fetch_button_abm, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Any unsaved event will be lost. Do you still want to refresh?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_refresh_abm", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
observeEvent(input$ok_refresh_abm, {
  removeModal()
  # session$reload()
  
  #function to create data under abm_df and load "equip" database
  arc_name_selection_abm <- dbReadTable(con, "arc") %>% select(-ArcName_Note) #for a selection
  pb_name_selection_abm <- dbReadTable(con, "procblock") %>% select(-c(ProcBlock_Note,MfgFomat)) #for a selection
  abm_df$data <- dbReadTable(con, "arcblockmap") %>%  # for a db update
    left_join(arc_name_selection_abm, by="uuid_arc") %>% #add ArcName from uuid_arc
    left_join(pb_name_selection_abm, by="uuid_pb")     #add ProcBlock from uuid_pb
})


# Render Table ------------------------------------------------------------

#store the currently filtered table in a reactive
interim_table_abm <- reactive({
  
  abm_df$data <- abm_df$data[,c("uuid_abm","Seq_PB","ArcName","ProcBlock","uuid_arc","uuid_pb")] #reorder to follow original data frame order, plus ArcName and ProcBlock
  
  table <- abm_df$data 
  if (!"All" %in% input$abm_selection){
    return(table[table$ArcName %in% input$abm_selection,])
  }else{
    return(table)
  }
  table
})

output$abm_table <- DT::renderDataTable({
  #this is for debugging. this won't work for datatable but work for a single data
  #cat(file=stderr(), "input$abm_selection includes", input$abm_selection, "\n")
  interim_table_abm() %>%
    select(-uuid_abm, -uuid_arc, -uuid_pb) #remove UUID ################################################################need to add uuid_arc and uuid_pb ##RDB setting
},
rownames = FALSE,
server = TRUE,
options = list (
  pageLength = 20
)
)


# Save to database --------------------------------------------------------

#confirmation window
observeEvent(input$save_button_abm, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Are you sure to save change(s) you made into database?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_save_abm", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
#save to database
observeEvent(input$ok_save_abm, {
  data_for_save_abm <- abm_df$data %>% select(-ArcName, -ProcBlock)
  data_for_save_abm <- data_for_save_abm[order(data_for_save_abm$uuid_arc,data_for_save_abm$Seq_PB),] #sort by seq
  dbWriteTable(con, "arcblockmap", data_for_save_abm, overwrite = T)
  removeModal()
  shinyalert(title = "Saved to database", type = "success")
})  



# Download ----------------------------------------------------------------

output$download_button_abm <- downloadHandler(
  filename = function() {
    paste("PF_DB_ArcBlockMap_", Sys.Date(), ".csv", sep="") #sep="" is no space between words (in this case PFxx and date)
  },
  content = function(file) {
    write.csv(abm_df$data, file, row.names=F)
  }
)