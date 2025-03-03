library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggsci)
library(survminer)
library(patchwork)
library(jsonlite)
library(shinyAce)  # Added for syntax highlighting
library(showtext)

# 启用中文字体支持
font_add_google("Noto Sans SC", "notosans")
showtext_auto()



# Define UI
ui <- page_sidebar(
  title = "R 可视化代码编辑器",
  sidebar = sidebar(
    position = "right",
    width = 300,
    card(
      card_header("已保存的代码"),
      downloadButton("download_json", "下载代码 (JSON)"),
      uiOutput("saved_code_cards")
    )
  ),

  tags$head(
    tags$style(
      HTML("
        .code-card {
          margin-bottom: 10px;
          padding: 8px;
        }
        .code-preview {
          max-height: 80px;
          overflow: hidden;
          font-size: 0.8rem;
          background-color: #f8f9fa;
          padding: 4px;
          border-radius: 3px;
          margin: 5px 0;
          white-space: pre-wrap;
          word-wrap: break-word;
        }
        .btn-group-sm {
          margin-top: 5px;
        }
        .sidebar {
          overflow-y: auto;
          max-height: 100vh;
        }
        pre.r-code {
          background-color: #f5f5f5;
          border-radius: 4px;
          padding: 10px;
          white-space: pre-wrap;
        }
        .warning-banner {
          background-color: #fff3cd;
          color: #856404;
          padding: 10px;
          margin-bottom: 15px;
          border-radius: 5px;
          border: 1px solid #ffeeba;
        }
      ")
    )
  ),

  layout_columns(
    col_widths = c(6, 6),

    # Left column - Code input with syntax highlighting
    card(
      card_header(
        tags$div(
          class = "d-flex justify-content-between align-items-center",
          tags$span("代码编辑器"),
          actionButton("save_code", "保存代码", class = "btn btn-primary btn-sm")
        )
      ),
      aceEditor(
        "code_editor",
        value =
          'library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  labs(title = "哈哈Car Weight vs MPG",
       x = "Weight (1000 lbs)",
       y = "Miles Per Gallon",
       color = "Cylinders") +
  theme_minimal() +
  scale_color_npg()',
        mode = "r",
        theme = "cobalt",
        height = "100%",
        fontSize = 14,
        tabSize = 2,
        showLineNumbers = TRUE,
        highlightActiveLine = TRUE,
        autoComplete = "enabled"
      )
    ),

    # Right column - Plot output
    card(
      card_header("图形输出"),
      plotOutput("plot_output", height = "100%")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Reactive value to store saved code snippets
  saved_codes <- reactiveVal(list())

  # Function to run code and show output
  run_code <- function() {
    output$plot_output <- renderPlot({
      # Create a new environment to evaluate the code
      env <- new.env()

      # Try to evaluate the code
      tryCatch({
        eval(parse(text = input$code_editor), envir = env)
      }, error = function(e) {
        # If there's an error, display error message
        plot.new()
        # Try to use Chinese font if available
        if(has_font_support) {
          par(family = "notosans")
        }
        text(0.5, 0.5, paste("错误:", e$message), col = "red", cex = 1.2)
      })
    }, res = 96)  # Higher resolution for better text rendering
  }

  # Auto-run code when ace editor changes
  observeEvent(input$code_editor, {
    invalidateLater(500)
    run_code()
  }, ignoreInit = FALSE)

  # Run the default code when the app starts
  observe({
    run_code()
  })

  # Function to truncate code for display
  truncateCode <- function(code, maxChars = 100) {
    # Try to extract title from labs() or ggtitle()
    title_match <- regexpr("title\\s*=\\s*[\"']([^\"']+)[\"']", code)

    if (title_match > 0) {
      # Extract the title from labs() function
      title_text <- regmatches(code, regexec("title\\s*=\\s*[\"']([^\"']+)[\"']", code))[[1]][2]
      return(paste0("", title_text))
    }  else {
      # If no title found, return first 10 characters
      if (nchar(code) > 10) {
        return(paste0(substr(code, 1, 10), "..."))
      } else {
        return(code)
      }
    }
  }

  # Save code to the list
  observeEvent(input$save_code, {
    if (nchar(trimws(input$code_editor)) > 0) {
      current_codes <- saved_codes()
      new_id <- length(current_codes) + 1
      current_codes[[new_id]] <- list(
        id = new_id,
        code = input$code_editor,
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      saved_codes(current_codes)
    }
  })

  # Render the saved code cards
  output$saved_code_cards <- renderUI({
    codes <- saved_codes()
    if (length(codes) == 0) {
      return(p("没有保存的代码。"))
    }

    cards <- lapply(codes, function(code_item) {
      card(
        class = "code-card",
        card_header(tags$small(code_item$timestamp)),
        truncateCode(code_item$code, 10),
        div(
          class = "btn-group btn-group-sm",
          style = "display: flex; justify-content: space-between;",
          actionButton(paste0("load_code_", code_item$id), "加载", class = "btn-sm btn-primary"),
          actionButton(paste0("delete_code_", code_item$id), "删除", class = "btn-sm btn-danger")
        )
      )
    })

    div(cards)
  })

  # Handle the output for each saved code card
  observe({
    codes <- saved_codes()
    for (code_item in codes) {
      local({
        local_id <- code_item$id
        local_code <- code_item$code

        # Load button handler
        load_id <- paste0("load_code_", local_id)
        observeEvent(input[[load_id]], {
          updateAceEditor(session, "code_editor", value = local_code)
        }, ignoreNULL = TRUE, ignoreInit = TRUE)

        # Delete button handler
        delete_id <- paste0("delete_code_", local_id)
        observeEvent(input[[delete_id]], {
          current_codes <- saved_codes()
          # Fixed the deletion code to properly filter the list
          current_codes <- current_codes[sapply(current_codes, function(x) x$id != local_id)]
          saved_codes(current_codes)
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }
  })

  # Fixed: Add input handlers for view buttons dynamically using session$userData
  # This replaces the problematic code that was trying to use 'i'
  observe({
    input_names <- names(input)
    view_patterns <- grep("^view_code_\\d+$", input_names, value = TRUE)

    for (view_pattern in view_patterns) {
      if (is.null(session$userData[[view_pattern]])) {
        session$userData[[view_pattern]] <- TRUE
        local({
          local_pattern <- view_pattern
          local_id <- as.numeric(gsub("view_code_", "", local_pattern))

          observeEvent(input[[local_pattern]], {
            codes <- saved_codes()
            code_item <- NULL

            # Find the correct code item by ID
            for (item in codes) {
              if (item$id == local_id) {
                code_item <- item
                break
              }
            }

            if (!is.null(code_item)) {
              showModal(modalDialog(
                title = paste("代码 #", code_item$id, " - ", code_item$timestamp),
                aceEditor(
                  outputId = paste0("modal_code_view_", code_item$id),
                  value = code_item$code,
                  mode = "r",
                  theme = "cobalt",
                  height = "300px",
                  readOnly = TRUE,
                  fontSize = 14
                ),
                size = "l",
                easyClose = TRUE,
                footer = modalButton("关闭")
              ))
            }
          }, ignoreNULL = TRUE, ignoreInit = TRUE)
        })
      }
    }
  })

  # Download all saved code as JSON
  output$download_json <- downloadHandler(
    filename = function() {
      paste("r_visualization_codes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json", sep = "")
    },
    content = function(file) {
      codes <- saved_codes()
      write_json(codes, file, pretty = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
