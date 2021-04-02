#enable shiny alert
useShinyalert()


#make pb_df reactive on the user event
pb_df <- reactiveValues()

#function to create data under pb_df and load "procblock" database
dbCon <- function() {
  pb_df$data <- dbReadTable(con, "procblock")
}
#load
dbCon()


# form prep ---------------------------------------------------------------

#Form function for data entry, to be called in both 'add' and 'edit' event
#button_id_pb is a variable and button id which will navigate to each event
entry_form_pb <- function(button_id_pb) {
  
  showModal(
    modalDialog(
      title = strong("Entry Form"),
      fluidRow(
        column(6,
               textInput("ProcBlock", "ProcBlock", placeholder = ""),
               textInput("MfgFomat", "MfgFomat", placeholder = ""),
               textAreaInput("ProcBlock_Note", "ProcBlock_Note", placeholder = "put process block assumptions such as what's included")
        )
      ),
      footer = tagList(
        actionButton(button_id_pb, "Submit"),
        modalButton("Cancel")
      ),
      size = "m", #window size. m:medium
      easyClose = TRUE
    )
  )
  
}

#Save function for form data into data frame format from form function
formData_pb <- reactive({
  
  formData_pb <- data.frame(uuid_pb = UUIDgenerate(),
                            ProcBlock = input$ProcBlock,
                            MfgFomat = input$MfgFomat,
                            ProcBlock_Note = input$ProcBlock_Note
  ) %>% 
    mutate_each(funs(as.character), c(uuid_pb, ProcBlock, ProcBlock_Note, MfgFomat)) #match variable type to original database
  
  #paste0(str(formData))
  return(formData_pb)
  
})


# Add ---------------------------------------------------------------------

#Add data button
observeEvent(input$add_button_pb, {
  entry_form_pb("submit_new_pb")
})
observeEvent(input$submit_new_pb, {
  pb_df$data <- bind_rows(pb_df$data, formData_pb()) #add new record at bottom
  shinyjs::reset("entry_form_pb")
  removeModal()
})  


# Delete ------------------------------------------------------------------

observeEvent(input$delete_button_pb, {
  showModal(
    if(length(input$pb_table_rows_selected)>=1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Are you sure you would like to delete selected row(s)?"),
        footer = tagList(
          actionButton("submit_delete_pb", "Yes"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ) 
    } else if(length(input$pb_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select row(s)"),
        easyClose = TRUE
      )
    }
  )
})
observeEvent(input$submit_delete_pb, {
  pb_df$data <- pb_df$data[-input$pb_table_rows_selected,]
  removeModal()
})


# Copy --------------------------------------------------------------------

observeEvent(input$copy_button_pb, {
  if(length(input$pb_table_rows_selected)>=1) {
    row_selection_pb <- pb_df$data[input$pb_table_rows_selected, "uuid_pb"] #identify uuid for the selected rows
    copy_df_pb <- pb_df$data %>% filter(uuid_pb %in% row_selection_pb) #filter by selected uuid rows
    paste(copy_df_pb)
    copy_df_pb$uuid_pb <- replicate(nrow(copy_df_pb), UUIDgenerate()) #replace with new uuid for copied rows
    pb_df$data <- bind_rows(pb_df$data, copy_df_pb) #add copy records at bottom
  } else if(length(input$pb_table_rows_selected)<1) {
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
observeEvent(input$edit_button_pb, {
  
  showModal(
    if(length(input$pb_table_rows_selected)>1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select only one row"),
        easyClose = TRUE
      )
    } else if(length(input$pb_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select a row"),
        easyClose = TRUE
      )
    }
  )
  
  if(length(input$pb_table_rows_selected)==1) {
    
    #load entry form
    entry_form_pb("submit_edit_pb")
    
    #update each value in entyr form based on selected row
    updateTextInput(session, "ProcBlock", value = pb_df$data[input$pb_table_rows_selected, "ProcBlock"])
    updateTextInput(session, "MfgFomat", value = pb_df$data[input$pb_table_rows_selected, "MfgFomat"])
    updateTextAreaInput(session, "ProcBlock_Note", value = pb_df$data[input$pb_table_rows_selected, "ProcBlock_Note"])
  }
})

#update to database
observeEvent(input$submit_edit_pb, {
  uuid_selection_pb <- pb_df$data[input$pb_table_rows_selected, "uuid_pb"] #identify uuid for the selected row
  uuid_index_pb <- match(uuid_selection_pb, pb_df$data$uuid_pb) #get matched row index using UUID
  pb_df$data[uuid_index_pb, 2:4] <- formData_pb()[,2:4] #replace data with updated data. make sure column order matches. uuid_pb is 1st column in formData while last column in database
  shinyjs::reset("entry_form_pb")
  removeModal()
})

# Fetch -------------------------------------------------------------------

observeEvent(input$fetch_button_pb, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Any unsaved event will be lost. Do you still want to refresh?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_refresh_pb", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
observeEvent(input$ok_refresh_pb, {
  removeModal()
  session$reload()
  # dbCon()
})


# Render Table ------------------------------------------------------------

output$pb_table <- DT::renderDataTable({
  
  table <- pb_df$data %>% select(-uuid_pb) #remove UUID
  dt <- datatable(
    table,
    selection = 'multiple',
    escape = FALSE,
    rownames = FALSE,
    options = list (
      pageLength = 20
    )
  )
})


# Save to database --------------------------------------------------------

#confirmation window
observeEvent(input$save_button_pb, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Are you sure to save change(s) you made into database?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_save_pb", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
#save to database
observeEvent(input$ok_save_pb, {
  dbWriteTable(con, "procblock", pb_df$data, overwrite = T)
  removeModal()
  shinyalert(title = "Saved to database", type = "success")
})  



# Download ----------------------------------------------------------------

output$download_button_pb <- downloadHandler(
  filename = function() {
    paste("PF_DB_ProcBlock_", Sys.Date(), ".csv", sep="") #sep="" is no space between words (in this case PFxx and date)
  },
  content = function(file) {
    write.csv(pb_df$data, file, row.names=F)
  }
)