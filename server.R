server <- function(input, output, session) {
  
  
  # login modules -----------------------------------------------------------
  
  #set Logged = FLASE as defalt and showing loging window
  login = FALSE
  USER <- reactiveValues(login = login)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
      passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
      h6("Basic Authentication"),
      h6('Username : datak, Password : 555'),
      if (failed)
        div(tags$b("Invalid crendentials. Use given username and password", style = "color: red;")),
      footer=tagList(
        actionButton("login", "Login")
      )
    )
  }
  
  # Show modal
  # This `observe` is suspended only whith right user credential otherwise keep showing model window
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful, remove the modal. 
  #login verification
  obs2 <- observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          
          Id.username <- which(login_details$user %in% Username)
          Id.password <- which(login_details$pswd %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0){
            if (Id.username == Id.password) {
              USER$login <-  TRUE
              obs1$suspend()
              removeModal()
            } else {
              showModal(dataModal(failed = TRUE))
            }
          }
        } 
      }
    }    
  })
  
  
  # mini sidebar setting ----------------------------------------------------
  
  #Make sidebar collapse into icons instead of nothing
  runjs({"
        var el2 = document.querySelector('.skin-purple');
        el2.className = 'skin-purple sidebar-mini';
        "})
  
  
  # load files --------------------------------------------------------------
  source('server_rms_arc.R', local = TRUE)
  source('server_rms_procblock.R', local = TRUE)
  source('server_rms_arcblockmap.R', local = TRUE)
  source('server_rms_equip.R', local = TRUE)
  # source('server_rms_blockopsmap.R', local = TRUE)
  # source('server_rms_mtrl.R', local = TRUE)
  # source('server_app_master.R', local = TRUE)
  
  
}


