# Load the required packages
library(shiny)
library(bslib)

# List of top 100 German words with Czech translations
flashcards <- data.frame(
    german = c("der", "und", "sein", "in", "zu", "haben", "ich", "werden", "sie", "von",
               "nicht", "mit", "es", "sich", "auf", "für", "er", "so", "dass", "können",
               "dies", "als", "aber", "wir", "sein", "bei", "oder", "ein", "sagen", "um",
               "müssen", "über", "machen", "kein", "durch", "wollen", "auch", "mir", "mehr",
               "wissen", "dann", "jetzt", "mein", "etwas", "gehen", "kommen", "lassen", "sein",
               "stehen", "finden", "bleiben", "nehmen", "bringen", "denken", "dürfen", "wissen",
               "vor", "gehen", "sehen", "geben", "kommen", "lassen", "sagen", "gehen", "stehen",
               "machen", "nehmen", "sollen", "tun", "finden", "halten", "müssen", "leben", "brauchen",
               "vidět", "najít", "žít", "zůstat", "přijít", "jít", "vidět", "dát", "přijít",
               "stát", "najít", "přinést", "myslet", "smět", "vědět", "před", "po", "u",
               "bez", "vždy", "možný"),
    czech = c("ten", "a", "být", "v", "k", "mít", "já", "stát se", "ona", "z",
              "ne", "s", "to", "se", "na", "pro", "on", "tak", "že", "moci",
              "toto", "jako", "ale", "my", "být", "u", "nebo", "jeden", "říkat", "aby",
              "muset", "o", "dělat", "žádný", "skrz", "chtít", "taky", "mně", "víc",
              "vědět", "pak", "teď", "můj", "něco", "jít", "přijít", "nechat", "on",
              "stát", "najít", "zůstat", "vzít", "přinést", "myslet", "smět", "vědět",
              "před", "jít", "vidět", "dát", "přijít", "nechat", "říkat", "jít", "stát",
              "dělat", "vzít", "mít", "dělat", "najít", "držet", "muset", "žít", "potřebovat",
              "vidět", "najít", "žít", "zůstat", "přijít", "jít", "vidět", "dát", "přijít",
              "stát", "najít", "přinést", "myslet", "smět", "vědět", "před", "po", "u",
              "bez", "vždy", "možný"),
    stringsAsFactors = FALSE
)

# Define the UI with bslib theme
ui <- fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    titlePanel("German-Czech Flashcards"),
    
    # Centered layout for flashcards
    fluidRow(
        column(
            width = 6, offset = 3,
            # Dropdown for selecting direction and practice set
            selectInput("direction", "Select direction:", 
                        choices = c("German to Czech", "Czech to German")),
            selectInput("practice_set", "Select practice set:", 
                        choices = c("All Words", "Flagged Words")),
            
            # Flashcard container with larger font and centered text
            div(
                style = "padding: 20px; font-size: 30px; text-align: center; 
                 border: 2px solid #5bc0de; border-radius: 10px; background-color: #f7f7f7;",
                textOutput("phrase"),
                textOutput("translation")
            ),
            br(),
            
            # Display remaining words
            div(
                style = "text-align: center; font-size: 20px; margin-top: 10px;",
                textOutput("remaining_words")
            ),
            
            # Button row for navigation, showing answer, and flagging/unflagging
            div(
                style = "text-align: center; margin-top: 20px;",
                actionButton("next_card", "Next Card", class = "btn-primary"),
                actionButton("show_answer", "Show Answer", class = "btn-info"),
                actionButton("toggle_flag", "Flag Word", class = "btn-warning")
            )
        )
    )
)

# Define the server
server <- function(input, output, session) {
    
    # Reactive values to store indices, flagged words, and states
    all_indices <- reactiveVal(sample(nrow(flashcards)))
    flagged_indices <- reactiveVal(c())
    card_index <- reactiveVal(1)
    show_translation <- reactiveVal(FALSE)
    
    # Update current flashcard set based on selected practice set
    current_indices <- reactive({
        if (input$practice_set == "Flagged Words") {
            flagged_indices()
        } else {
            all_indices()
        }
    })
    
    # Observe "Next Card" button click
    observeEvent(input$next_card, {
        # Move to the next card in the current set
        new_index <- card_index() + 1
        if (new_index > length(current_indices())) {
            # Reshuffle if at the end of the set
            if (input$practice_set == "All Words") {
                all_indices(sample(nrow(flashcards)))
            }
            new_index <- 1
        }
        card_index(new_index)
        show_translation(FALSE)  # Hide translation for new card
    })
    
    # Show translation on button click
    observeEvent(input$show_answer, {
        show_translation(TRUE)
    })
    
    # Flag or unflag the current word
    observeEvent(input$toggle_flag, {
        current_index <- current_indices()[card_index()]
        if (current_index %in% flagged_indices()) {
            # Unflag if already flagged
            flagged_indices(setdiff(flagged_indices(), current_index))
        } else {
            # Flag if not already flagged
            flagged_indices(c(flagged_indices(), current_index))
        }
    })
    
    # Update "Flag/Unflag Word" button text based on current word's flag status
    observe({
        current_index <- current_indices()[card_index()]
        if (current_index %in% flagged_indices()) {
            updateActionButton(session, "toggle_flag", label = "Unflag Word")
        } else {
            updateActionButton(session, "toggle_flag", label = "Flag Word")
        }
    })
    
    # Display the phrase based on the chosen direction
    output$phrase <- renderText({
        index <- current_indices()[card_index()]
        card <- flashcards[index, ]
        if (input$direction == "German to Czech") {
            paste("German:", card$german)
        } else {
            paste("Czech:", card$czech)
        }
    })
    
    # Display the translation, hidden initially
    output$translation <- renderText({
        if (show_translation()) {
            index <- current_indices()[card_index()]
            card <- flashcards[index, ]
            if (input$direction == "German to Czech") {
                paste("Czech:", card$czech)
            } else {
                paste("German:", card$german)
            }
        } else {
            ""
        }
    })
    
    # Display remaining words in the current set
    output$remaining_words <- renderText({
        remaining <- length(current_indices()) - card_index() + 1
        paste("Words remaining in this round:", remaining)
    })
}

# Run the app
shinyApp(ui = ui, server = server)
