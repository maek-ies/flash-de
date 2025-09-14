library(shiny)
library(bslib)
library(shinyjs)

# ---- Sample data ----
verbs <- data.frame(
    english = c("be","have","do","say","get","make","go","know","take","see",
                "come","think","give","find","tell","become","show","leave","feel","put",
                "bring","begin","keep","hold","write","stand","hear","let","mean","set",
                "meet","run","pay","sit","speak","lie","lead","read","grow","lose",
                "fall","send","build","understand","draw","break","spend","cut","rise","buy"),
    past_simple = c("was/were","had","did","said","got","made","went","knew","took","saw",
                    "came","thought","gave","found","told","became","showed","left","felt","put",
                    "brought","began","kept","held","wrote","stood","heard","let","meant","set",
                    "met","ran","paid","sat","spoke","lay","led","read","grew","lost",
                    "fell","sent","built","understood","drew","broke","spent","cut","rose","bought"),
    past_participle = c("been","had","done","said","got/gotten","made","gone","known","taken","seen",
                        "come","thought","given","found","told","become","shown","left","felt","put",
                        "brought","begun","kept","held","written","stood","heard","let","meant","set",
                        "met","run","paid","sat","spoken","lain","led","read","grown","lost",
                        "fallen","sent","built","understood","drawn","broken","spent","cut","risen","bought"),
    czech = c("být","mít","dělat","říct","dostat","udělat","jít","vědět","vzít","vidět",
              "přijít","myslet","dát","najít","říct","stát se","ukázat","odejít","cítit","dát",
              "donést","začít","udržet","držet","psát","stát","slyšet","dovolit","znamenat","nastavit",
              "potkat","běžet","zaplatit","sedět","mluvit","ležet","vést","číst","růst","ztratit",
              "padnout","poslat","stavět","rozumět","kreslit","zlomit","utrácet","řezat","stoupnout","koupit"),
    rank = 1:50,
    stringsAsFactors = FALSE
)

ui <- page_fillable(
    theme = bs_theme(bootswatch = "flatly"),
    useShinyjs(),
    tags$style(HTML("
    .flip-card {
      background-color: transparent;
      width: 350px;
      height: 250px;
      perspective: 1000px;
      margin: auto;
    }
    .flip-card-inner {
      position: relative;
      width: 100%;
      height: 100%;
      text-align: center;
      transition: transform 0.6s;
      transform-style: preserve-3d;
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      border-radius: 12px;
    }
    .flip-card.flipped .flip-card-inner {
      transform: rotateY(180deg);
    }
    .flip-card-front, .flip-card-back {
      position: absolute;
      width: 100%;
      height: 100%;
      backface-visibility: hidden;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      border-radius: 12px;
      padding: 10px;
      font-size: 1.2em;
    }
    .flip-card-front {
      background-color: #f8f9fa;
      color: #333;
    }
    .flip-card-back {
      background-color: #007bff;
      color: white;
      transform: rotateY(180deg);
      font-size: 1.1em;
      line-height: 1.6;
    }
    .answer-row { margin: 2px 0; }
    .answer-label { font-size: 0.9em; opacity: 0.7; }
    .answer-cz {
      margin-top: 8px;
      padding-top: 6px;
      border-top: 1px solid #eee;
      font-weight: bold;
    }
    .btn-row {
      margin-top: 10px;
      display: flex;
      justify-content: center;
      gap: 10px;
    }
    @media (max-width: 576px) {
      .flip-card { width: 90%; height: 220px; }
    }
  ")),
    tags$script(HTML("
    $(document).on('click', '.flip-card', function() {
      $(this).toggleClass('flipped');
    });
  ")),
    
    layout_sidebar(
        sidebar = sidebar(
            selectInput("direction", "Direction", 
                        c("English → Czech" = "en2cz", "Czech → English" = "cz2en")),
            selectInput("practice_set", "Practice set", 
                        c("Full set" = "full", "Flagged only" = "flagged")),
            selectInput("count", "Number of words", c(50, 100, 150), selected = 50)
        ),
        layout_columns(
            col_widths = c(12),
            uiOutput("progress_text"),
            div(
                class = "flip-card", id = "singleCard",
                div(class = "flip-card-inner",
                    div(class = "flip-card-front",
                        uiOutput("card_front")
                    ),
                    div(class = "flip-card-back",
                        uiOutput("card_back")
                    )
                )
            ),
            # --- Buttons below card in two rows ---
            div(class = "btn-row",
                actionButton("prev", "Previous"),
                actionButton("nextBtn", "Next")
            ),
            div(class = "btn-row",
                actionButton("shuffle", "Shuffle"),
                actionButton("flagBtn", "Flag / Unflag")
            )
        )
    )
)

server <- function(input, output, session) {
    pool <- reactiveVal(verbs)
    flagged <- reactiveVal(integer(0))
    cur <- reactiveVal(1)
    
    # Reset pool when count or practice_set changes
    observe({
        n <- as.numeric(input$count)
        df <- verbs[1:min(n, nrow(verbs)), ]
        if (input$practice_set == "flagged") {
            f <- flagged()
            if (length(f) > 0) df <- df[df$english %in% verbs$english[f], ]
            else df <- df[0, ]
        }
        pool(df)
        cur(1)   # reset index so counter works
    })
    
    get_current_pool <- reactive(pool())
    
    output$progress_text <- renderUI({
        df <- get_current_pool()
        if (nrow(df) == 0) return(NULL)
        div(class="text-center mb-2",
            paste("Card", cur(), "of", nrow(df),
                  "| Remaining:", nrow(df) - cur()))
    })
    
    output$card_front <- renderUI({
        df <- get_current_pool()
        i <- cur()
        if (nrow(df) == 0) return(NULL)
        if (input$direction == "en2cz") span(df$english[i]) else span(df$czech[i])
    })
    
    output$card_back <- renderUI({
        df <- get_current_pool()
        i <- cur()
        if (nrow(df) == 0) return(NULL)
        tagList(
            div(class="answer-row", span(class="answer-label","Base form: "), strong(df$english[i])),
            div(class="answer-row", span(class="answer-label","Past simple: "), strong(df$past_simple[i])),
            div(class="answer-row", span(class="answer-label","Past participle: "), strong(df$past_participle[i])),
            div(class="answer-cz", "Czech: ", df$czech[i])
        )
    })
    
    observeEvent(input$shuffle, {
        df <- get_current_pool()
        if (nrow(df) == 0) return()
        pool(df[sample(nrow(df)), ])
        cur(1)
        runjs("document.getElementById('singleCard').classList.remove('flipped')")
    })
    
    observeEvent(input$nextBtn, {
        p <- get_current_pool()
        if (nrow(p) == 0) return()
        i <- cur() + 1
        if (i > nrow(p)) i <- 1
        cur(i)
        runjs("document.getElementById('singleCard').classList.remove('flipped')")
    })
    
    observeEvent(input$prev, {
        p <- get_current_pool()
        if (nrow(p) == 0) return()
        i <- cur() - 1
        if (i < 1) i <- nrow(p)
        cur(i)
        runjs("document.getElementById('singleCard').classList.remove('flipped')")
    })
    
    observeEvent(input$flagBtn, {
        df <- get_current_pool()
        if (nrow(df) == 0) return()
        word <- df$english[cur()]
        idx <- which(verbs$english == word)
        f <- flagged()
        if (idx %in% f) {
            flagged(setdiff(f, idx))
        } else {
            flagged(c(f, idx))
        }
    })
}

shinyApp(ui, server)

