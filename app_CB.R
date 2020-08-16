######################################################
## Load required packages

library(tidyverse)
library(shiny)
library(shinyjs)
library(digest)


######################################################
## Set file paths **EACH USER SHOULD UPDATE**

# *Update with your source path to the app folder*
# app_path <- #ADD HERE

# Responses path (no need to change)
responses_path <- file.path(app_path, "responses/")

# Data file to pull abstracts from
input_file <- "clean_for_app_full.RDS"


######################################################
## Define save and load functions

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# Save function
saveData <- function(data) {
    # Generate unique file name
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    # Write csv to responses sub-folder
    write.csv(x = data, 
              file = file.path(responses_path, fileName),
              row.names = FALSE, quote = T)
}

# Load function (Null default to handle first run)
loadPriors <- function(filesInfo=NULL, prior_responses=NULL) {
    # Set working directory to responses sub-folder
    setwd(responses_path)
    
    # Determine whether prior responses have been loaded
    if (is.null(prior_responses)) {
        # Get current file list
        filesInfo <- list.files()
        
        # Load prior responses for first time
        prior_responses <- map(filesInfo,
                               function(x) {
                                   read_csv(x, col_names = T,
                                            skip_empty_rows = T,
                                            col_types = "ccd")
                               })
    }
    else {
        # Get new file list
        new_files <- setdiff(list.files(), filesInfo)

        # If >= 1 new file then append to prior_responses and filesInfo
        if (length(new_files) != 0) {
            new_responses <- map(new_files,
                                 function(x) {
                                     read_csv(x, col_names = T,
                                              skip_empty_rows = T,
                                              col_types = "ccd")
                                 })
            prior_responses <- bind_rows(prior_responses, new_responses)
            filesInfo <- c(filesInfo, new_files)
        }
    }
    # Return current file list and responses as a list object
    out <- list(filesInfo = filesInfo,
                prior_responses = bind_rows(prior_responses))
    out
}


###################################################
## Define User Interface components

## Step 1: Identify coder, ask for new data
names <- c("None Selected", "Carrie", "Kathleen", 
           "Katie", "Manika", "Mike")

ui_intro <- sidebarLayout(
    sidebarPanel(
        # who is coding? (will not re-set automatically)
        selectInput("name", "Coder Name", names)
    ),
    mainPanel(
        # show a new abstract button
        actionButton("new_abstract", "Show a New Abstract") 
    )
)

## Step 1b: Check coder can code
ui_check <- fluidRow(
    shinyjs::hidden(
        div(
            id = "limit_msg",
            h3("You've coded enough!")
        )
    )  
)


## Step 2: Code an Abstract
# coding choices
codes <- c("None selected",
           "Gender research", 
           "Gender implications only",
           "Irrelevant research",
           "Not a research article",
           "Abstract missing",
           "I'm not sure")

# layout
ui_code <- sidebarLayout(
    sidebarPanel(
        div( 
            id = "form",
            # make a coding decision
            radioButtons("code_choice", "Coding Choice", codes),
            # submit decision
            actionButton("submit", "Submit")
        )
    ),
    # display the title and abstract
    mainPanel(
        # display current item to code
        tableOutput("journal_title"),
        tableOutput("article_title"),
        tableOutput("abstract")
    )
)

## Step 3: Confirm submission 
ui_confirm <- fluidRow(
    shinyjs::hidden(
        div(
            id = "thankyou_msg",
            h3("Response submitted!")
        )
    )  
)

## Combine UI elements
ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Code Journal Abstracts for Gender Content"),
    
    ui_intro,
    ui_check,
    ui_code,
    ui_confirm
)

###################################################
## Define Server functions

# Define the fields we want to save 
fields <- c("name", "code_choice")

server <- function(input, output, session) {
    
    # Call data to code ------------------------------------------
    dataset <- readRDS(file.path(app_path, input_file))
    
    # Call prior responses and extract file and data list elements
    priorData <- loadPriors()
    fileInfo <- priorData$filesInfo
    priors <- priorData$prior_responses
    
    # Check if current coder has done too much -------------------
    observeEvent(input$name, {
        limit = nrow(dataset) / (length(names) - 1)
        current = sum(priors$name == input$name)
        if (current >= 2 * limit){
            # display message if so
            shinyjs::show("limit_msg")
        }
    })
    
    # Select a random article to code -----------------------------
    to_code <- eventReactive(input$new_abstract, {
        
        # refresh prior responses
        priorData <- loadPriors(fileInfo, priors)
        fileInfo <- priorData$filesInfo
        priors <- priorData$prior_responses
        
        # exclude articles already coded twice
        exclude1 <- priors %>%
            group_by(UID) %>%
            mutate(total = length(UID)) %>%
            filter(total == 2) %>%
            select(UID)
        
        # exclude articles already coded by current coder
        exclude2 <- priors %>%
            filter(name == input$name) %>%
            select(UID)
        
        # combine exclusions
        exclude <- bind_rows(exclude1, exclude2)
        
        # random selection from the remainder
        dataset %>% 
            data.frame() %>%
            filter(!(UID %in% exclude$UID)) %>%
            dplyr::sample_n(size = 1) 
    })
    
    # display journal, title, and abstract ----------------------
    output$journal_title <- renderTable(to_code() %>% select(SO))
    output$article_title <- renderTable(to_code() %>% select(TI)) 
    output$abstract <- renderTable(to_code() %>% select(AB))
    
    # Aggregate form data and article ID -----------------------
    formData <- reactive({
        data <- sapply(fields, function(x) input[[x]])
        # append article ID
        data <- c(data, # coder name and choice
                  to_code() %>% select(UID)) # article ID
        # transpose
        data <- t(data)
        data
    })
    
    # Enable or disable the submit button -------------------------
    observe({
        # check if all mandatory fields have a value
        mandatoryFilled <-
            vapply(fields,
                   function(x) {
                       !is.null(input[[x]]) && input[[x]] != ""
                   },
                   logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        
        # enable/disable the submit button
        shinyjs::toggleState(id = "submit", 
                             condition = mandatoryFilled)
    })
    
    # When Submit button is clicked, save form data and confirm -----
    observeEvent(input$submit, {
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::hide("journal_title")
        shinyjs::hide("article_title")
        shinyjs::hide("abstract")
        shinyjs::show("thankyou_msg")
    })
    
    # When "Show Abstract" button is clicked again, display clean form ------
    observeEvent(input$new_abstract, {
        shinyjs::show("form")
        shinyjs::show("journal_title")
        shinyjs::show("article_title")
        shinyjs::show("abstract")
        shinyjs::hide("thankyou_msg")
    })    
}


###################################################
## Run the app
shinyApp(ui, server)

