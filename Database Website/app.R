# packages
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(shinythemes)
library(stringr)
library(shinyWidgets)

# dataset
gender_research <- readRDS("gender_research_articles.rds") %>%
  select(-c("UID", "SR_FULL", "AU_UN_NR")) 

# documentation
articles_doc = read_csv("data_doc_original.csv")
summ_doc = read_csv("data_doc_summ.csv")
code_doc = read_csv("coding_decisions.csv")

# journals
journal = unique(gender_research$SO)

# years
years = sort(unique(gender_research$PY, decreasing = TRUE))

gender_research[4139, 6] = "Eighty subjects viewed a videotaped simulated debate between an incumbent candidate and his challenger for a fictitious post of County Commissioner. In a 2 by 2 by 2 design, either the incumbent candidate or his subordinate was accused of sexual or financial misconduct, and the incumbent candidate either denied the misconduct charges or apologized for the misconduct. Consistent with Schlenker's (1980) theory of \"impression management,\" denial of misconduct charges was a more effective strategy in obtaining votes than apologizing for misconduct. The candidate was also viewed more negatively if he was directly accused of misconduct than if his subordinate was accused. Voting patterns of subjects in the study were consistent with ratings of the honesty, ethics, and trustworthiness of the incumbent candidate."

# gender_research <- read_csv("gender_research.csv", 
#                             locale = readr::locale(encoding = "UTF-8"))

# display with title, author, year, abstract, journals, and code choice
display_data <- gender_research %>%
  select(c("TI", "AU", "PY", "AB", "SO", "code_choice", "gender_journ"))

# searchable dataset
gender_research_search = gender_research %>%
  mutate(search = tolower(TI)) %>%
  mutate(search = str_replace_all(search, "[^[:alnum:]]", ""))

# select data based on coding choice
byCode = function(input, .data) {
  
  # checks to make sure a filter was specified
  if (length(input) != 0) {
    data_return = .data %>%
      filter(code_choice %in% input)
  }
  # return the original data otherwise
  else {
    data_return = .data
  }
  
  return(data_return)
}

# search for corresponding article
findByTitle = function(search_string, .data) {
  searcher = search_string %>%
    tolower() %>%
    str_replace_all("[^[:alnum:]]", "")
  
  find_data = tryCatch({
    .data %>%
      filter(str_detect(search, searcher))
    }, 
    error = function(e) {
      return(data.frame())
    })
  
  # if no result is found, return NA
  if (nrow(find_data) == 0) return(NA)
  if (searcher == "") return(NA)
  
  return(find_data[1, ])
}

# summary dataset
summary_data <- readRDS("gender_research_summary.rds") %>%
  rename("Scopus Count" = "count_sco", 
         "Web of Science Count" = "count_wos", 
         "Total Articles" = "n_all", 
         "No. Gender Keywords" = "n_keyword", 
         "No. Gender Keywords (Unique)" = "n_key_uniq", 
         "Prop. Unique" = "prop_uniq_key")

# shiny app UI
ui <- fluidPage(
  theme = shinytheme("paper"), 
  navbarPage(
"A Dataset of Gender and Politics Research",
  id = "journals_app", 
  # tab for mainpage
  tabPanel("Main", 
          h1(strong("A Dataset of Gender and Politics Research"), style = "font-size:40px;"), 
          "This application provides access to the data presented and discussed in Carolyn Barnett, Michael FitzGerald, Katie Krumbholz, and Manika Lamba, \"Gender Research in Political Science Journals: A Dataset,\"",
          em("PS: Political Science and Politics"), 
          ", 2022. The article and its accompanying online appendix discuss in detail the methodology used to collect and code the articles in the dataset. The full dataset is also available in .rds format, with additional documentation, on the Harvard Dataverse at ", 
          a(href = "https://doi.org/10.7910/DVN/UVWRTV", "https://doi.org/10.7910/DVN/UVWRTV"), 
          ". The authors thank John Kim for creating this Shiny app. Any questions should be directed to one of the study authors listed above. ", 
          h1(strong("License & Citation"), style = "font-size:30px"), 
          "Downloads from this dataset may be used in accordance with the terms of the Creative Commons Attribution-NonCommercial-ShareAlike license (see details at", 
          a(href = "https://creativecommons.org/licenses/by-nc-sa/4.0/", "https://creativecommons.org/licenses/by-nc-sa/4.0/"), "). ",
          h3(strong("Suggested Citation"), style = "font-size:20px"), 
          "Barnett, Carolyn, Michael FitzGerald, Katie Krumbholz, and Manika Lamba. 2022. \"Gender Research in Political Science: A Dataset.\" benidjones.shinyapps.io/gender_research/.",
          h1(strong("Usage"), style = "font-size:30px;"), 
          "The app is divided into four tabs: Main (you are here), Dataset, Article Details, and Summary Data. ", 
          h3(strong("Dataset"), style = "font-size:20px;"), 
          "Here, you may view, filter, and search the dataset on a subset of its columns (Title, Authors, Year, Abstract, Journal, Code Choice, and Gender Journal). Major filters for Coding Decision, Publication Year, Journal Type, and Journal Name are located on the left. ", 
          "Clicking the DOWNLOAD button downloads a .csv file of the dataset, with the major filters applied. Clicking on any article name will link to its corresponding Article Details entry. ", 
          h3(strong("Article Details"), style = "font-size:20px;"), 
          "Here, you may view most of the features of the data from the original dataset for a given article. You can select an article via the Dataset tab, or search using the search bar.", 
          h3(strong("Summary Data", style = "font-size:20px;")), 
          "Here, summary data from the original paper is available in raw format.", 
          h1(strong("Documentation"), style = "font-size:30px;"), 
          "Documentation on coding decisions is as follows: ", 
          tableOutput("code_doc"), 
          "Documentation for the article data is as follows: ", 
          tableOutput("articles_doc"), 
          "Documentation for the summary data is as follows: ", 
          tableOutput("summ_doc"), 
          #mainPanel(textOutput("citation"))
  ),
  # tab for clean data
  tabPanel("Dataset", 
           sidebarLayout(
             sidebarPanel(width = 2, 
                          # display instructions
                          textOutput("table_instruct"), 
                          # filter by coding decision
                          checkboxGroupInput("whichCode", "Filter by Coding Decision: ", 
                                        choices = c("Unambiguous" = "unambiguous", 
                                                    "Ambiguous" = "ambiguous", 
                                                    "Excluded (due to type)" = "exclude (type)", 
                                                    "Excluded (due to content)" = "exclude (content)", 
                                                    "Missing Abstract" = "abstract missing")), 
                          # filter by year
                          textOutput("year_instruct"), 
                          dropdown(
                            checkboxGroupInput("whichYear", "Years", 
                                               choices = years
                            )
                          ),
                          # filter by journal type 
                          textOutput("journal_type_instruct"), 
                          dropdown(
                            checkboxGroupInput("journType", "Gender Journal (Yes or No)", 
                                               choices = c("Yes" = 1, 
                                                           "No" = 0)
                            )
                          ),
                          # filter by journal name
                          textOutput("journal_name_instruct"), 
                          dropdown(
                          checkboxGroupInput("whichJournal", "Journal Names", 
                                             choices = journal)), 
                          # downloader
                          textOutput("download_instruct"), 
                          downloadButton("downloadData", "Download")),
             
             mainPanel(div(DT::DTOutput("clean_data"), 
                           style = "font-size: 75%; width: 125%"))
           )),
  # tab for article details
  tabPanel("Article Details", 
           mainPanel(
             fluidRow(textInput("title_search", "Search Article Title: ")), 
             fluidRow(htmlOutput("article_search_results")))), 
  
  # tab for summary data
  tabPanel("Summary Data", 
             mainPanel(div(DT::DTOutput("sum_data"), 
                           style = "font-size: 75%; width: 125%"))
           ),
  # tab for readme details
  # tabPanel("README", 
  #          "This application provides access to the data presented and discussed in Carolyn Barnett, Michael FitzGerald, Katie Krumbholz, and Manika Lamba, \"Gender Research in Political Science Journals: A Dataset,\"",
  #          em("PS: Political Science and Politics"), 
  #          ", 2022. The article and its accompanying online appendix discuss in detail the methodology used to collect and code the articles in the dataset. The full dataset is also available in .rds format, with additional documentation, on the Harvard Dataverse at ", 
  #          a(href = "https://doi.org/10.7910/DVN/UVWRTV", "https://doi.org/10.7910/DVN/UVWRTV"), 
  #          ". The authors thank John Kim for creating this Shiny app. Any questions should be directed to one of the study authors listed above. "
  #          #mainPanel(textOutput("citation"))
  #          )
)
)

# shiny app backend
server <- function(input, output, session) {
  # applies relevant filters to data
  filterChoices = reactive({  
    # checks to make sure a coding filter was supplied
    if(length(input$whichCode) != 0) {
      data_return = display_data %>%
        filter(code_choice %in% input$whichCode)
    }
    # return the original data otherwise
    else {
      data_return = display_data
    }
    
    # checks to make sure a journal filter was supplied
    if(length(input$whichJournal) != 0) {
      data_return = data_return %>%
        filter(SO %in% input$whichJournal)
    }
    
    # checks to make sure a journal type filter was supplied
    if(length(input$journType) != 0) {
      data_return = data_return %>%
        filter(gender_journ %in% input$journType)
    }
    
    # checks to make sure a publish year filter was supplied
    if(length(input$whichYear) != 0) {
      data_return = data_return %>%
        filter(PY %in% input$whichYear)
    }
    
    return(data_return)
    })
  
  # reactive version of title search
  titleSearchReactive = reactive({
    return(findByTitle(input$title_search, gender_research_search))
  })
  
  # display small data table
  output$clean_data = renderDT(filterChoices() %>% 
                                 rename("Title" = "TI",
                                        "Author(s)" = "AU", 
                                        "Publication Year" = "PY", 
                                        "Abstract" = "AB", 
                                        "Journal" = "SO",
                                        "Code Choice" = "code_choice") %>%
    DT::datatable(filter = "top",
                                    extensions = c("Buttons", "ColReorder"),
                                options = list(# autoWidth = TRUE,
                                               scrollX = TRUE,
                                               dom = "Bfrtip",
                                               buttons = c("copy", "csv", "pdf"),
                                columnDefs = list(list(width = "10px", targets = c(1, 2, 3, 5, 6))),
                                #                   list(width = "1200px", targets = 4)),
                                colReorder = TRUE, 
                                pageLength = 5)) %>%
      formatStyle(1, cursor = "pointer"))
  
  # creates interaction with clicking on the table
  observeEvent(input$clean_data_cell_clicked, {
    title_click = input$clean_data_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not the title column
    if (is.null(title_click$value) || title_click$col != 1) return()
    updateTabsetPanel(session, "journals_app", selected = "Article Details")
    updateTextInput(session, "title_search", value = title_click$value)
  })
  
  
  # display summary data table 
  output$sum_data = DT::datatable(summary_data, filter = "top", 
                                    extensions = c("Buttons", "ColReorder"), 
                                    options = list(autoWidth = TRUE,
                                                   scrollX = TRUE, 
                                                   dom = "rtip",
                                                   buttons = c("copy", "csv", "pdf"), 
                                                   colReorder = TRUE)) %>%
    DT::renderDT()
  
  # get search results and display them
  output$article_search_results = renderUI({
    article_data = titleSearchReactive()
    # failed search cases
    if (!is.list(article_data) & input$title_search == "") return("Search by title above, or click on a title entry in the Dataset tab.")
    if (!is.list(article_data)) return("No results were found. Search using the exact title (special characters are ignored). ")
    
    # 1 search result case
    if (nrow(article_data) == 1) {
      article_data = article_data %>%
        select(-"search") %>%
        rename("DOI" = "DI", "Author(s)" = "AU", "Publication Year" = "PY", 
               "Title" = "TI", "Journal" = "SO", "Volume" = "VL", "Issue" = "PN", 
               "Pages" = "PP", "Abstract" = "AB", "Auto Abstract" = "auto_abs", 
               "Code Choice" = "code_choice", "Gender Journal" = "gender_journ", 
               "References" = "CR", "No. References" = "NR", "WoS Citations" = "TC", 
               "Doc Type" = "DT", "Publisher" = "PU", "Short Ref" = "SR", "ISSN" = "SN", 
               "Journal (Abbr.)" = "JI", "Language" = "LA", "Author Address" = "C1", 
               "Reprint Address" = "RP", "Uni Affil (All)" = "AU_UN", 
               "Uni Affil" = "AU1_UN", "Database" = "DB", "Author Keywords" = "DE", 
               "Keyword Plus" = "ID") %>%
        relocate(c("Code Choice", "Title", "Author(s)", "Publication Year", "Abstract", 
                 "Journal", "Volume", "Issue", "Pages")) %>%
        t() 
      
      return(renderTable(article_data))
    }
    
    return("Search failed. Please try again later. ")
  })
  
  # download functionality
  output$downloadData = downloadHandler(
    filename = function() {
      paste(Sys.Date(), "Gender Research Data.csv")
    }, 
    content = function(file) {
      write.csv(filterChoices(), file, row.names = FALSE)
    }
  )
  
  ### Static tables in the app
  output$code_doc = renderTable(code_doc)
  output$articles_doc = renderTable(articles_doc)
  output$summ_doc = renderTable(summ_doc)
  
  ### All of the static text in the app follows!
  
  # data table instructions
  output$table_instruct = renderText({"Click on an article title to see full details."})
  
  # filters instructions
  output$journal_type_instruct = renderText({"Filter by Journal Type (click drop-down)"})
  output$journal_name_instruct = renderText({"Filter by Journal Name (click drop-down)"})
  output$year_instruct = renderText({"Filter by Year (click drop-down)"})
  
  # download instructions
  output$download_instruct = renderText({"Download a CSV of the data (with filters applied)"})
  
  # citation details
  output$citation = 
    renderText({"This application provides access to the data presented and discussed in Carolyn Barnett, Michael FitzGerald, Kate Krumbholz, and Manika Lambda, \"Gender Research in Political Science Journals: A Dataset,\"" })
}

# load shiny app
shinyApp(ui = ui, server = server)