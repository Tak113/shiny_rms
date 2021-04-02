#enable shiny alert
useShinyalert()


#make arc_df reactive on the user event
arc_df <- reactiveValues()

#create data under arc_df and load "ArcName" database
arc_df$data <- dbReadTable(con, "arc")


# form prep ---------------------------------------------------------------

#Form function for data entry, to be called in both 'add' and 'edit' event
#button_id_arc is a variable and button id which will navigate to each event
entry_form_arc <- function(button_id_arc) {
  
  showModal(
    modalDialog(
      title = strong("Entry Form"),
      fluidRow(
        column(6,
               textInput("ArcName", "ArcName", placeholder = ""),
               textAreaInput("ArcName_Note", "ArcName_Note", placeholder = "put technology and manufacturing assumptions here")
        )
      ),
      footer = tagList(
        actionButton(button_id_arc, "Submit"),
        modalButton("Cancel")
      ),
      size = "m", #window size. m:medium
      easyClose = TRUE
    )
  )
  
}

#Save function for form data into data frame format from form function
formData_arc <- reactive({
  
  formData_arc <- data.frame(uuid_arc = UUIDgenerate(),
                             ArcName = input$ArcName,
                             ArcName_Note = input$ArcName_Note
  ) %>% 
    mutate_each(funs(as.character), c(uuid_arc, ArcName, ArcName_Note)) #match variable type to original database
  
  #paste0(str(formData))
  return(formData_arc)
  
})


# Add ---------------------------------------------------------------------

#Add data button
observeEvent(input$add_button_arc, {
  entry_form_arc("submit_new_arc")
})
observeEvent(input$submit_new_arc, {
  arc_df$data <- bind_rows(arc_df$data, formData_arc()) #add new record at bottom
  shinyjs::reset("entry_form_arc")
  removeModal()
})  


# Delete ------------------------------------------------------------------

observeEvent(input$delete_button_arc, {
  showModal(
    if(length(input$arc_table_rows_selected)>=1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Are you sure you would like to delete selected row(s)?"),
        footer = tagList(
          actionButton("submit_delete_arc", "Yes"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ) 
    } else if(length(input$arc_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select row(s)"),
        easyClose = TRUE
      )
    }
  )
})
observeEvent(input$submit_delete_arc, {
  arc_df$data <- arc_df$data[-input$arc_table_rows_selected,]
  removeModal()
})


# Copy --------------------------------------------------------------------

observeEvent(input$copy_button_arc, {
  if(length(input$arc_table_rows_selected)>=1) {
    row_selection_arc <- arc_df$data[input$arc_table_rows_selected, "uuid_arc"] #identify uuid for the selected rows
    copy_df_arc <- arc_df$data %>% filter(uuid_arc %in% row_selection_arc) #filter by selected uuid rows
    paste(copy_df_arc)
    copy_df_arc$uuid_arc <- replicate(nrow(copy_df_arc), UUIDgenerate()) #replace with new uuid for copied rows
    arc_df$data <- bind_rows(arc_df$data, copy_df_arc) #add copy records at bottom
  } else if(length(input$arc_table_rows_selected)<1) {
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
observeEvent(input$edit_button_arc, {
  
  showModal(
    if(length(input$arc_table_rows_selected)>1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select only one row"),
        easyClose = TRUE
      )
    } else if(length(input$arc_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select a row"),
        easyClose = TRUE
      )
    }
  )
  
  if(length(input$arc_table_rows_selected)==1) {
    
    #load entry form
    entry_form_arc("submit_edit_arc")
    
    #update each value in entyr form based on selected row
    updateTextInput(session, "ArcName", value = arc_df$data[input$arc_table_rows_selected, "ArcName"])
    updateTextAreaInput(session, "ArcName_Note", value = arc_df$data[input$arc_table_rows_selected, "ArcName_Note"])
  }
})

#update to database
observeEvent(input$submit_edit_arc, {
  uuid_selection_arc <- arc_df$data[input$arc_table_rows_selected, "uuid_arc"] #identify uuid for the selected row
  uuid_index_arc <- match(uuid_selection_arc, arc_df$data$uuid_arc) #get matched row index using UUID
  arc_df$data[uuid_index_arc, 2:3] <- formData_arc()[,2:3] #replace data with updated data. make sure column order matches. uuid_arc is 1st column in formData while last column in database
  shinyjs::reset("entry_form_arc")
  removeModal()
})

# Fetch -------------------------------------------------------------------

observeEvent(input$fetch_button_arc, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Any unsaved event will be lost. Do you still want to refresh?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_refresh_arc", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
observeEvent(input$ok_refresh_arc, {
  removeModal()
  session$reload()
})


# Render Table ------------------------------------------------------------

output$arc_table <- DT::renderDataTable({
  
  table <- arc_df$data %>% select(-uuid_arc) #remove UUID
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
observeEvent(input$save_button_arc, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Are you sure to save change(s) you made into database?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_save_arc", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
#save to database
observeEvent(input$ok_save_arc, {
  dbWriteTable(con, "arc", arc_df$data, overwrite = T)
  removeModal()
  shinyalert(title = "Saved to database", type = "success")
})  



# Download ----------------------------------------------------------------

output$download_button_arc <- downloadHandler(
  filename = function() {
    paste("PF_DB_Architecture_", Sys.Date(), ".csv", sep="") #sep="" is no space between words (in this case PFxx and date)
  },
  content = function(file) {
    write.csv(arc_df$data, file, row.names=F)
  }
)