library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(rstudioapi)
library(dplyr)
library(stringr)
library(wordcloud2)
library(tidytext)
library(tidyr)
library(plotly)
library(arcdiagram)
library(circlize)
library(igraph)
library(networkD3)

movies_subset_df <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path), "/data_files/theSubset.csv", sep = ""))

movies_df <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path), "/data_files/theGoodDataset.csv", sep = ""))

unique_genres <- unique(unlist(strsplit(as.character(movies_df$genre), ", ")))

genre_groups <- lapply(split(unique_genres, substr(unique_genres, 1, 1)), function(x) setNames(x, x))

#### To actors

actor_freq <- movies_subset_df %>%
  dplyr::select(actors) %>%
  separate_rows(actors, sep = ",\\s*") %>%
  dplyr::count(actors, sort = TRUE)

top_10_actors <- actor_freq[1:10, ]

subset_actors <- movies_subset_df %>%
  separate_rows(actors, sep = ",\\s*") %>%
  dplyr::inner_join(top_10_actors, by = "actors")

top_5_actors <- subset_actors %>%
  dplyr::group_by(actors) %>%
  dplyr::summarise(median_success_norm = median(success_norm))

top_5_actors <- as.data.frame(top_5_actors)

top_5_actors <- top_5_actors[order(top_5_actors$median_success_norm, decreasing = TRUE), ]

top_5_actors <- unlist(lapply(list(top_5_actors[1:5, ]$actors), function(x) as.list(x)), recursive = FALSE)

unique_actors <- unique(unlist(strsplit(as.character(actor_freq[1:200, ]$actors), ", ")))

unique_actors <- setdiff(unique_actors, unlist(top_5_actors))

actors_groups <- lapply(split(unique_actors, substr(unique_actors, 1, 1)), function(x) setNames(x, x))

#### To directors

director_freq <- movies_subset_df %>%
  dplyr::select(director) %>%
  separate_rows(director, sep = ",\\s*") %>%
  dplyr::count(director, sort = TRUE)

top_10_director <- director_freq[1:10, ]

subset_director <- movies_subset_df %>%
  separate_rows(director, sep = ",\\s*") %>%
  dplyr::inner_join(top_10_director, by = "director")

top_5_director <- subset_director %>%
  dplyr::group_by(director) %>%
  dplyr::summarise(median_success_norm = median(success_norm))

top_5_director <- as.data.frame(top_5_director)

top_5_director <- top_5_director[order(top_5_director$median_success_norm, decreasing = TRUE), ]

top_5_director <- unlist(lapply(list(top_5_director[1:5, ]$director), function(x) as.list(x)), recursive = FALSE)

unique_director <- unique(unlist(strsplit(as.character(director_freq$director), ", ")))

unique_director <- setdiff(unique_director, unlist(top_5_director))

director_groups <- lapply(split(unique_director, substr(unique_director, 1, 1)), function(x) setNames(x, x))

ui <-  fluidPage(title = "Movies",
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "dataset_input", "Select the dataset",
        c("Initial" = "theDataset",
          "Successful" = "theSubset"),
      ),
      hr(),
      pickerInput(
        "genre_picker", "Filter to one or more genres:",
        choices = c(genre_groups),
        options =  list("max-options" = 3),
        multiple = TRUE
      ),
      hr(),
      pickerInput(
        "actors_picker", "Select one or more actors",
        c(list(`Top 5` = top_5_actors), actors_groups),
        options =  list("max-options" = 5),
        multiple = TRUE
      ),
      hr(),
      pickerInput(
        "directors_picker", "Select one director",
        c(list(`Top 5` = top_5_director), director_groups),
        options =  list("max-options" = 4),
        multiple = TRUE
      ),
      actionButton("go","Update View",icon("refresh"))
    ),
    
    mainPanel(
      tabsetPanel(id = "subject", 
        tabPanel("Genre"),
        tabPanel("Duration"),
        tabPanel("Crew"),
        tabPanel("$$$"),
        tabPanel("Trends"),
        #tabPanel("Synopsis")
      ),
      br(),
      conditionalPanel(
        'input.subject === "Genre"',
        fluidRow(column(12, DTOutput("choose"))),
        fluidRow(column(5, wordcloud2Output("workloud_genre")),
                 column(7, plotOutput("arc_diagram_genre"))),
        fluidRow(column(12, plotOutput("heatmap_genre")))
      ),
      conditionalPanel(
        'input.subject === "Crew"',
        fluidRow(column(12, plotOutput("genres_by_actor"))),
        fluidRow(column(12, plotOutput("genres_by_director"))),
        fluidRow(column(12, plotOutput("network_actors_director"))),
      ),
      conditionalPanel(
        'input.subject === "Duration"',
        fluidRow(column(12, plotOutput("duration_with_genre"))),
      ),
      conditionalPanel(
        'input.subject === "$$$"',
        fluidRow(column(12, plotOutput("financial_line"))),
        fluidRow(column(12, plotlyOutput("critical_financial_engagement"))),
      ),
      conditionalPanel(
        'input.subject === "Trends"',
        fluidRow(column(6, plotOutput("temporal_trend_avg_or_success")), 
                 column(6, plotOutput("temporal_trend_world_income"))),
        fluidRow(column(6, plotOutput("temporal_trend_budget")), 
                 column(6, plotOutput("temporal_trend_reviews"))),
      ),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rv <- reactiveValues( df = movies_df, df_name = "theDataset", ola = 1 )
  
  observeEvent(input$dataset_input, {
    if (rv$df_name == input$dataset_input) return()
    if (input$dataset_input == "theDataset") {
      rv$df <- movies_df
    } else {
      rv$df <- movies_subset_df
    }
    rv$df_name <- input$dataset_input
  })
  
  observeEvent(input$go, {
    heatmap_genre <- rv$df
    
    heatmap_genre$date_published <- as.Date(heatmap_genre$date_published)
    heatmap_genre$month <- format(heatmap_genre$date_published, "%b")
    
    month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    heatmap_genre$month <- factor(heatmap_genre$month, levels = month_order)
    
    check_genre_subgroup <- function(x) all(str_detect(x, input$genre_picker))
    check_actors_subgroup <- function(x) any(str_detect(x, input$actors_picker))
    check_directors_subgroup <- function(x) any(str_detect(x, input$directors_picker))
    breaks <- c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360)
    if (!is.null(input$genre_picker)) {
      
      genres_selected_df <- rv$df %>%
        dplyr::filter(rowSums(sapply(input$genre_picker, function(x) grepl(x, genre))) == length(input$genre_picker))
      
      text <- genres_selected_df$description
      
      words_list <- strsplit(text, "\\s+")
      
      words <- unlist(words_list)
      
      words <- words[!toupper(words) %in% toupper(stopwords::stopwords("en"))]
      output$workloud_genre <- renderWordcloud2({
        words_table <- table(words)
        if (max(words_table) > 5) {
          wordcloud2(data = data.frame(word = names(words_table[words_table > 5]), freq = as.numeric(words_table[words_table > 5])))
        } else {
          wordcloud2(data = data.frame(word = names(words_table[words_table > 1]), freq = as.numeric(words_table[words_table > 1])))
        }
      })
      
      genre_comb <- rv$df %>%
        dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
        separate_rows(genre, sep = ", ") %>%
        dplyr::group_by(original_title) %>%
        dplyr::filter(n() > 1) %>%
        dplyr::summarise(genre_combinations = list(combn(genre, 2, simplify = FALSE))) %>%
        unnest(genre_combinations)
      
      edgelist_df <- as.data.frame(do.call(rbind, genre_comb$genre_combinations))
      colnames(edgelist_df) <- c("genre1", "genre2")
      
      edgelist_df <- as.data.frame(table(edgelist_df))
      
      edgelist_df$genre1 <- as.character(edgelist_df$genre1)
      edgelist_df$genre2 <- as.character(edgelist_df$genre2)
      
      edgelist_df <- subset(edgelist_df, genre1 != genre2)
      
      genre_counts <- rv$df %>%
        dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
        separate_rows(genre, sep = ", ") %>%
        dplyr::group_by(genre) %>%
        dplyr::summarise(frequency = n()) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(frequency))
      
      ordered_genres <- genre_counts$genre
      
      output$arc_diagram_genre <- renderPlot({
        arcplot(as.matrix(edgelist_df[, c("genre1", "genre2")]), 
                labels=ordered_genres, cex.labels=0.8,
                show.nodes=TRUE, 
                cex.nodes = log1p(genre_counts$frequency)/2, pch.nodes=21,
                lwd.nodes = 2, 
                col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * log1p(edgelist_df$Freq))
      })
      
      ##### Genre by Actor plot
      
        movie_data <- rv$df %>%
          dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
          dplyr::mutate(
            genre = strsplit(genre, ", "),
            actors = strsplit(actors, ", "),
            director = strsplit(director, ", ")
          )
      
      
      for (i in 1:nrow(movie_data)) {
        movie_data$first_actor[i] <- movie_data$actors[[i]][1]
      }
      
      movie_data <- movie_data %>%
        unnest_longer(genre)
      if (!is.null(input$actors_picker)) {
        movie_data <- movie_data %>%
          dplyr::filter(sapply(first_actor, check_actors_subgroup))
      }
      if (rv$df_name == "theDataset") {
        output$critical_financial_engagement <- renderPlotly({
          NULL
        })
        duration_hist <- rv$df %>%
          dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
          dplyr::mutate(duration_bin = cut(duration, breaks = breaks, include.lowest = TRUE)) %>%
          dplyr::group_by(duration_bin) %>%
          dplyr::summarise(freq = n())
        
        output$duration_with_genre <- renderPlot({
          ggplot(duration_hist, aes(x = duration_bin, y = freq)) +
            geom_bar(stat = "identity", fill = "lightblue", color = "black", size = 0.2) +
            labs(title= "Frequency in function of the Movie Duration",x = "Duration", y = "Frequency") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })
        
        financial_data_normalized <- rv$df %>%
          dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
          dplyr::select(budget, worlwide_gross_income, profit, votes, reviews_from_users, avg_vote) %>%
          dplyr::mutate(across(everything(), ~scales::rescale(.))) %>%
          dplyr::mutate(index = row_number())
        
        financial_long <- financial_data_normalized %>%
          pivot_longer(cols = c(budget, worlwide_gross_income, profit, votes, reviews_from_users, avg_vote), 
                       names_to = "variable", values_to = "value") %>%
          dplyr::mutate(variable = factor(variable, levels = c("budget", "worlwide_gross_income", "profit", "votes", "reviews_from_users", "avg_vote")))
        
        success_values <- financial_long %>%
          dplyr::filter(variable == "avg_vote") %>%
          dplyr::select(index, success_value = value)
        
        financial_long <- financial_long %>%
          left_join(success_values, by = "index")
        
        success_75 <- quantile(financial_data_normalized$avg_vote, 0.75, na.rm = TRUE)
        
        financial_long$variable <- factor(financial_long$variable, 
                                          levels = c("budget", "worlwide_gross_income", "profit", "votes", "reviews_from_users", "avg_vote"),
                                          labels = c("Budget", "Worldwide Gross Income", "Profit", "Votes", "Number of Reviews", "Average Rating"))
        
        output$financial_line <- renderPlot({
          ggplot(financial_long, aes(x = variable, y = value, group = index, color = success_value)) +
            geom_line(data = subset(financial_long, success_value >= success_75)) +
            scale_color_gradient(low = "lightyellow", high = "red4") +
            theme_minimal() + 
            labs(x="", y="", color = "Avg Rating")
        })
        
        heatmap_genre <- heatmap_genre %>%
          dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
          separate_rows(genre, sep = ",\\s*") %>%
          dplyr::group_by(genre, month) %>%
          dplyr::summarise(frequency = n())
        
        output$heatmap_genre <- renderPlot({
          ggplot(heatmap_genre, aes(x = month, y = genre, fill = frequency)) +
            geom_tile() +
            scale_fill_gradient(low = "lightyellow1", high = "darkred") +
            labs(title = "Frequency-Genre-Time Heatmap",
                 x = "Month",
                 y = "Genre",
                 fill = "Frequency") +
            theme_void() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme(axis.text.y = element_text(angle = 0, hjust = 1))
        })
        
        result_by_genre_actor <- movie_data %>%
          dplyr::group_by(genre, first_actor) %>%
          dplyr::summarise(freq = n())
        
        if (max(result_by_genre_actor$freq) > 5) {
          result_by_genre_actor <- subset(result_by_genre_actor, freq > 5)
        }
        
        output$genres_by_actor <- renderPlot({
          ggplot(result_by_genre_actor, aes(x = first_actor, y = freq, fill = genre)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Frequency by Genre and Actor", x = "Actor", y = "Frequency") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        result_by_genre_director <- movie_data %>%
          dplyr::group_by(genre, director) %>%
          dplyr::summarise(freq = n()) %>%
          unnest_longer(director)
        
        if (!is.null(input$directors_picker)) {
          result_by_genre_director <- result_by_genre_director %>%
            dplyr::filter(sapply(director, check_directors_subgroup))
        }
        
        if(max(result_by_genre_director$freq) > 5) {
          result_by_genre_director <- subset(result_by_genre_director, freq > 5)
        }
        
        output$genres_by_director <- renderPlot({
          ggplot(result_by_genre_director, aes(x = director, y = freq, fill = genre)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Frequency by Genre and Director", x = "Director", y = "Frequency") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        result <- rv$df$genre %>%
          sapply(function(x) all(str_detect(x, input$genre_picker))) %>%
          rv$df$genre[.]
        unique_counts_df <- as.data.frame(table(result))
        sorted_data <- unique_counts_df[order(-unique_counts_df$Freq), ]
        
        colnames(sorted_data) <- c("Groups", "Frequency")
        
        output$choose <- renderDT({
          datatable(sorted_data, options = list(pageLength = 5, lengthMenu = c(5, 10, 15), searching = FALSE), rownames = FALSE)
        })
        
      } else {
        
        arco_da_velha_graph <- rv$df %>%
          dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
          dplyr::mutate(
            profit_readable = profit / 1000000, 
            hover_info = paste(original_title, "<br> Average vote rating:", avg_vote, "<br> Number of votes:", votes) 
          ) 
        
        gg <- ggplot(arco_da_velha_graph, aes(x = success_norm, y = profit_readable, size = votes, color = avg_vote)) +
          geom_point(shape = 16, aes(text = hover_info)) + 
          labs(x = "Success (normalized)", y = "Profit (in millions of US dollars)", size = "Number of Votes", color = "Average rating") +
          ggtitle("Critical acclamation, Financial return and Engagement") +
          scale_color_gradient(low = "turquoise", high = "darkblue") +
          theme_minimal() +
          scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 1500)) +
          scale_size_continuous(breaks = c(500000, 1000000, 1500000, 2000000), labels = c("500K", "1M", "1.5M", "2M"))

        output$critical_financial_engagement <- renderPlotly({
          ggplotly(gg, tooltip = c("text"))
        })
        
        duration_hist <- rv$df %>%
          dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
          dplyr::mutate(duration_bin = cut(duration, breaks = breaks, include.lowest = TRUE)) %>%
          dplyr::group_by(duration_bin) %>%
          dplyr::summarise(median_success_norm = median(success_norm, na.rm = TRUE))
        
        output$duration_with_genre <- renderPlot({
          ggplot(duration_hist, aes(x = duration_bin, y = median_success_norm)) +
            geom_bar(stat = "identity", fill = "lightblue", color = "black", size = 0.2) +
            labs(title= "Success in function of the Movie Duration",x = "Duration", y = "Success Normalized") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })
        
        financial_data_normalized <- rv$df %>%
          dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
          dplyr::select(budget, worlwide_gross_income, profit, votes, reviews_from_users, avg_vote, success) %>%
          dplyr::mutate(across(everything(), ~scales::rescale(.))) %>%
          dplyr::mutate(index = row_number())
        
        financial_long <- financial_data_normalized %>%
          pivot_longer(cols = c(budget, worlwide_gross_income, profit, votes, reviews_from_users, avg_vote, success), 
                       names_to = "variable", values_to = "value") %>%
          dplyr::mutate(variable = factor(variable, levels = c("budget", "worlwide_gross_income", "profit", "votes", "reviews_from_users", "success", "avg_vote")))
        
        success_values <- financial_long %>%
          dplyr::filter(variable == "success") %>%
          dplyr::select(index, success_value = value)
        
        financial_long <- financial_long %>%
          left_join(success_values, by = "index")
        
        success_75 <- quantile(financial_data_normalized$success, 0.75, na.rm = TRUE)
        
        financial_long$variable <- factor(financial_long$variable, 
                                          levels = c("budget", "worlwide_gross_income", "profit", "votes", "reviews_from_users", "avg_vote", "success"),
                                          labels = c("Budget", "Worldwide Gross Income", "Profit", "Votes", "Number of Reviews", "Average Rating", "Success"))
        
        output$financial_line <- renderPlot({
          ggplot(financial_long, aes(x = variable, y = value, group = index, color = success_value)) +
            geom_line(data = subset(financial_long, success_value >= success_75)) +
            scale_color_gradient(low = "lightyellow", high = "red4") +
            theme_minimal() + 
            labs(x="", y="", color = "Success")
        })
        
        heatmap_genre <- heatmap_genre %>%
          dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
          separate_rows(genre, sep = ",\\s*")
        
        output$heatmap_genre <- renderPlot({
          ggplot(heatmap_genre, aes(x = month, y = genre, fill = success_norm)) +
            geom_tile() +
            scale_fill_gradient(low = "lightyellow1", high = "darkred") +
            labs(title = "Genre-Time-Success Heatmap",
                 x = "Month",
                 y = "Genre",
                 fill = "Success Norm") +
            theme_void() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme(axis.text.y = element_text(angle = 0, hjust = 1))
        })
        
        if(max(movie_data$success_norm) > 0.45) {
          movie_data <- subset(movie_data, success_norm > 0.45)  
        }
        
        # Calculate the average success for each genre-actor combination
        avg_success_by_genre_actor <- movie_data %>%
          dplyr::group_by(genre, first_actor) %>%
          dplyr::summarise(Avg_Success = mean(success_norm, na.rm = TRUE))
        
        output$genres_by_actor <- renderPlot({
          ggplot(avg_success_by_genre_actor, aes(x = first_actor, y = Avg_Success, fill = genre)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Average Success by Genre and Actor", x = "Actor", y = "Average Success") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        movie_data <- rv$df %>%
          dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
          dplyr::mutate(
            genre = strsplit(genre, ", "),
            director = strsplit(director, ", ")
          )
        
        movie_data <- movie_data %>%
          unnest_longer(director) %>%
          unnest_longer(genre)
        
        result_by_genre_director <- movie_data %>%
          dplyr::group_by(genre, director) %>%
          dplyr::summarise(Avg_Success = mean(success_norm, na.rm = TRUE))
        
        if (!is.null(input$directors_picker)) {
          result_by_genre_director <- result_by_genre_director %>%
            dplyr::filter(sapply(director, check_directors_subgroup))
        }
        
        if(max(result_by_genre_director$Avg_Success) > 0.45) {
          result_by_genre_director <- subset(result_by_genre_director, Avg_Success > 0.45)
        }

        output$genres_by_director <- renderPlot({
          ggplot(result_by_genre_director, aes(x = director, y = Avg_Success, fill = genre)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Average Success by Genre and Director", x = "Director", y = "Average Success") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        # Filter data based on the subgroup condition
        filtered_data <- rv$df %>%
          dplyr::filter(sapply(genre, check_genre_subgroup))
        
        # Calculate the average success for each unique group of movie genres
        result <- filtered_data %>%
          dplyr::group_by(genre) %>%
          dplyr::summarise(avg_success = round(mean(success_norm),3))
        
        sorted_data <- result[order(-result$avg_success), ]
        
        colnames(sorted_data) <- c("Groups", "Avg Success")
        
        output$choose <- renderDT({
          datatable(sorted_data, options = list(pageLength = 5, lengthMenu = c(5, 10, 15), searching = FALSE), rownames = FALSE)
        })
      }
      
      trend_df <- rv$df %>%
        dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
        dplyr::mutate(
          genre = strsplit(genre, ", "),
          year = as.integer(format(as.Date( date_published), "%Y"))
        )
      
      if (rv$df_name == "theDataset") {
        yearly <- trend_df %>%
          dplyr::group_by(year ) %>%
          dplyr::summarise(mean_avg_vote = mean(avg_vote, na.rm = TRUE), 
                    mean_worlwide_gross_income = mean(worlwide_gross_income, na.rm = TRUE),
                    mean_reviews_from_users = mean(reviews_from_users, na.rm = TRUE),
                    mean_budget = mean(budget, na.rm = TRUE))
        
        future_years <- data.frame(year = 2021:2024)
        
        lm_model <- lm(mean_avg_vote ~ year, data = yearly)
        
        future_data <- data.frame(year = future_years$year)
        future_data$mean_avg_vote <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_worlwide_gross_income ~ year, data = yearly)
        future_data$mean_worlwide_gross_income <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_budget ~ year, data = yearly)
        future_data$mean_budget <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_reviews_from_users ~ year, data = yearly)
        future_data$mean_reviews_from_users <- predict(lm_model, newdata = future_data)
        
        extended_rating <- rbind(yearly, future_data)
        
        output$temporal_trend_avg_or_success <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_avg_vote)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Average Rating") +
            ggtitle("Temporal Trends in Mean Average Rating") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_world_income <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_worlwide_gross_income)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Worlwide gross Income") +
            ggtitle("Temporal Trends in Mean Worlwide gross Income") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_budget <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_budget)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Budget") +
            ggtitle("Temporal Trends in Mean Budget") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_reviews <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_reviews_from_users)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Reviews from users") +
            ggtitle("Temporal Trends in Reviews from users") +
            theme_minimal() +
            scale_color_identity()
        })
      } else {
        yearly <- trend_df %>%
          dplyr::group_by(year ) %>%
          dplyr::summarise(mean_success_norm = mean(success_norm, na.rm = TRUE), 
                    mean_worlwide_gross_income = mean(worlwide_gross_income, na.rm = TRUE),
                    mean_reviews_from_users = mean(reviews_from_users, na.rm = TRUE),
                    mean_budget = mean(budget, na.rm = TRUE))
        
        future_years <- data.frame(year = 2021:2024)
        
        lm_model <- lm(mean_success_norm ~ year, data = yearly)
        
        future_data <- data.frame(year = future_years$year)
        future_data$mean_success_norm <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_worlwide_gross_income ~ year, data = yearly)
        future_data$mean_worlwide_gross_income <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_budget ~ year, data = yearly)
        future_data$mean_budget <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_reviews_from_users ~ year, data = yearly)
        future_data$mean_reviews_from_users <- predict(lm_model, newdata = future_data)
        
        extended_rating <- rbind(yearly, future_data)
        
        output$temporal_trend_avg_or_success <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_success_norm)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Success") +
            ggtitle("Temporal Trends in Success") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_world_income <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_worlwide_gross_income)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Worlwide gross Income") +
            ggtitle("Temporal Trends in Mean Worlwide gross Income") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_budget <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_budget)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Budget") +
            ggtitle("Temporal Trends in Mean Budget") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_reviews <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_reviews_from_users)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Reviews from users") +
            ggtitle("Temporal Trends in Reviews from users") +
            theme_minimal() +
            scale_color_identity()
        })
      }
      
      
    } else {
      text <- rv$df$description
      
      words_list <- strsplit(text, "\\s+")
      
      words <- unlist(words_list)
      
      words <- words[!toupper(words) %in% toupper(stopwords::stopwords("en"))]
      output$workloud_genre <- renderWordcloud2({
        words_table <- table(words)
        if (max(words_table) > 5) {
          wordcloud2(data = data.frame(word = names(words_table[words_table > 5]), freq = as.numeric(words_table[words_table > 5])))
        } else {
          wordcloud2(data = data.frame(word = names(words_table[words_table > 1]), freq = as.numeric(words_table[words_table > 1])))
        }
      })
      
      genre_comb <- rv$df %>%
        separate_rows(genre, sep = ", ") %>%
        dplyr::group_by(original_title) %>%
        dplyr::filter(n() > 1) %>%
        dplyr::summarise(genre_combinations = list(combn(genre, 2, simplify = FALSE))) %>%
        unnest(genre_combinations)
      
      edgelist_df <- as.data.frame(do.call(rbind, genre_comb$genre_combinations))
      colnames(edgelist_df) <- c("genre1", "genre2")
      
      edgelist_df <- as.data.frame(table(edgelist_df))
      
      edgelist_df$genre1 <- as.character(edgelist_df$genre1)
      edgelist_df$genre2 <- as.character(edgelist_df$genre2)
      
      edgelist_df <- subset(edgelist_df, genre1 != genre2)
      
      genre_counts <- rv$df %>%
        separate_rows(genre, sep = ", ") %>%
        dplyr::group_by(genre) %>%
        dplyr::summarise(frequency = n()) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(frequency))
      
      ordered_genres <- genre_counts$genre
      
      output$arc_diagram_genre <- renderPlot({
        arcplot(as.matrix(edgelist_df[, c("genre1", "genre2")]), 
                labels=ordered_genres, cex.labels=0.8,
                show.nodes=TRUE, 
                cex.nodes = log1p(genre_counts$frequency)/2, pch.nodes=21,
                lwd.nodes = 2, 
                col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * log1p(edgelist_df$Freq))
      })
      
      ##### Genre by Actor plot
      
        
        movie_data <- rv$df %>%
          dplyr::mutate(
            genre = strsplit(genre, ", "),
            actors = strsplit(actors, ", ")
          )
      
      for (i in 1:nrow(movie_data)) {
        movie_data$first_actor[i] <- movie_data$actors[[i]][1]
      }
      
      movie_data <- movie_data %>%
        unnest_longer(genre)
      if (!is.null(input$actors_picker)) {
        movie_data <- movie_data %>%
          dplyr::filter(sapply(first_actor, check_actors_subgroup))
      }
      if (rv$df_name == "theDataset") {
        output$critical_financial_engagement <- renderPlotly({
          NULL
        })
        duration_hist <- rv$df %>%
          dplyr::mutate(duration_bin = cut(duration, breaks = breaks, include.lowest = TRUE)) %>%
          dplyr::group_by(duration_bin) %>%
          dplyr::summarise(freq = n())
        
        output$duration_with_genre <- renderPlot({
          ggplot(duration_hist, aes(x = duration_bin, y = freq)) +
            geom_bar(stat = "identity", fill = "lightblue", color = "black", size = 0.2) +
            labs(title= "Frequency in function of the Movie Duration",x = "Duration", y = "Frequency") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })
        
        financial_data_normalized <- rv$df %>%
          dplyr::select(budget, worlwide_gross_income, profit, votes, reviews_from_users, avg_vote) %>%
          dplyr::mutate(across(everything(), ~scales::rescale(.))) %>%
          dplyr::mutate(index = row_number())
        
        financial_long <- financial_data_normalized %>%
          pivot_longer(cols = c(budget, worlwide_gross_income, profit, votes, reviews_from_users, avg_vote), 
                       names_to = "variable", values_to = "value") %>%
          dplyr::mutate(variable = factor(variable, levels = c("budget", "worlwide_gross_income", "profit", "votes", "reviews_from_users", "avg_vote")))
        
        success_values <- financial_long %>%
          dplyr::filter(variable == "avg_vote") %>%
          dplyr::select(index, success_value = value)
        
        financial_long <- financial_long %>%
          left_join(success_values, by = "index")
        
        success_75 <- quantile(financial_data_normalized$avg_vote, 0.75, na.rm = TRUE)
        
        financial_long$variable <- factor(financial_long$variable, 
                                          levels = c("budget", "worlwide_gross_income", "profit", "votes", "reviews_from_users", "avg_vote"),
                                          labels = c("Budget", "Worldwide Gross Income", "Profit", "Votes", "Number of Reviews", "Average Rating"))
        
        output$financial_line <- renderPlot({
          ggplot(financial_long, aes(x = variable, y = value, group = index, color = success_value)) +
            geom_line(data = subset(financial_long, success_value >= success_75)) +
            scale_color_gradient(low = "lightyellow", high = "red4") +
            theme_minimal() + 
            labs(x="", y="", color = "Avg Rating")
        })
        
        heatmap_genre <- heatmap_genre %>%
          separate_rows(genre, sep = ",\\s*") %>%
          dplyr::group_by(genre, month) %>%
          dplyr::summarise(frequency = n())
          
        output$heatmap_genre <- renderPlot({
          ggplot(heatmap_genre, aes(x = month, y = genre, fill = frequency)) +
            geom_tile() +
            scale_fill_gradient(low = "lightyellow1", high = "darkred") +
            labs(title = "Frequency-Genre-Time Heatmap",
                 x = "Month",
                 y = "Genre",
                 fill = "Frequency") +
            theme_void() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme(axis.text.y = element_text(angle = 0, hjust = 1))
        })
        
        result_by_genre_actor <- movie_data %>%
          dplyr::group_by(genre, first_actor) %>%
          dplyr::summarise(freq = n())
        
        if(max(result_by_genre_actor$freq) > 5) {
          result_by_genre_actor <- subset(result_by_genre_actor, freq > 5)
        }
        
        output$genres_by_actor <- renderPlot({
          ggplot(result_by_genre_actor, aes(x = first_actor, y = freq, fill = genre)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Frequency by Genre and Actor", x = "Actor", y = "Frequency") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        movie_data <- rv$df %>%
          dplyr::mutate(
            genre = strsplit(genre, ", "),
            director = strsplit(director, ", ")
          )
        
        movie_data <- movie_data %>%
          unnest_longer(director) %>%
          unnest_longer(genre)
        
        result_by_genre_director <- movie_data %>%
          dplyr::group_by(genre, director) %>%
          dplyr::summarise(freq = n()) %>%
          unnest_longer(director)
        
        if (!is.null(input$directors_picker)) {
          result_by_genre_director <- result_by_genre_director %>%
            dplyr::filter(sapply(director, check_directors_subgroup))
        }
        
        if(max(result_by_genre_director$freq) > 5) {
          result_by_genre_director <- subset(result_by_genre_director, freq > 5)
        }
        
        output$genres_by_director <- renderPlot({
          ggplot(result_by_genre_director, aes(x = director, y = freq, fill = genre)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Frequency by Genre and Director", x = "Director", y = "Frequency") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        unique_counts_df <- as.data.frame(table(rv$df$genre))
        sorted_data <- unique_counts_df[order(-unique_counts_df$Freq), ]
        
        colnames(sorted_data) <- c("Groups", "Frequency")
        
        output$choose <- renderDT({
          datatable(sorted_data, options = list(pageLength = 5, lengthMenu = c(5, 10, 15), searching = FALSE), rownames = FALSE)
        })
      } else {
        arco_da_velha_graph <- rv$df %>%
          dplyr::mutate(
            profit_readable = profit / 1000000, 
            hover_info = paste(original_title, "<br> Average vote rating:", avg_vote, "<br> Number of votes:", votes) 
          ) 
        
        gg <- ggplot(arco_da_velha_graph, aes(x = success_norm, y = profit_readable, size = votes, color = avg_vote)) +
          geom_point(shape = 16, aes(text = hover_info)) + 
          labs(x = "Success (normalized)", y = "Profit (in millions of US dollars)", size = "Number of Votes", color = "Average rating") +
          ggtitle("Critical acclamation, Financial return and Engagement") +
          scale_color_gradient(low = "turquoise", high = "darkblue") +
          theme_minimal() +
          scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 1500)) +
          scale_size_continuous(breaks = c(500000, 1000000, 1500000, 2000000), labels = c("500K", "1M", "1.5M", "2M"))

        output$critical_financial_engagement <- renderPlotly({
          ggplotly(gg, tooltip = c("text"))
        })
        
        duration_hist <- rv$df %>%
          dplyr::mutate(duration_bin = cut(duration, breaks = breaks, include.lowest = TRUE)) %>%
          dplyr::group_by(duration_bin) %>%
          dplyr::summarise(median_success_norm = median(success_norm, na.rm = TRUE))
        
        output$duration_with_genre <- renderPlot({
          ggplot(duration_hist, aes(x = duration_bin, y = median_success_norm)) +
            geom_bar(stat = "identity", fill = "lightblue", color = "black", size = 0.2) +
            labs(title= "Success in function of the Movie Duration",x = "Duration", y = "Success Normalized") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })
        
        financial_data_normalized <- rv$df %>%
          dplyr::select(budget, worlwide_gross_income, profit, votes, reviews_from_users, avg_vote, success) %>%
          dplyr::mutate(across(everything(), ~scales::rescale(.))) %>%
          dplyr::mutate(index = row_number())
        
        financial_long <- financial_data_normalized %>%
          pivot_longer(cols = c(budget, worlwide_gross_income, profit, votes, reviews_from_users, avg_vote, success), 
                       names_to = "variable", values_to = "value") %>%
          dplyr::mutate(variable = factor(variable, levels = c("budget", "worlwide_gross_income", "profit", "votes", "reviews_from_users", "success", "avg_vote")))
        
        success_values <- financial_long %>%
          dplyr::filter(variable == "success") %>%
          dplyr::select(index, success_value = value)
        
        financial_long <- financial_long %>%
          left_join(success_values, by = "index")
        
        success_75 <- quantile(financial_data_normalized$success, 0.75, na.rm = TRUE)
        
        financial_long$variable <- factor(financial_long$variable, 
                                          levels = c("budget", "worlwide_gross_income", "profit", "votes", "reviews_from_users", "avg_vote", "success"),
                                          labels = c("Budget", "Worldwide Gross Income", "Profit", "Votes", "Number of Reviews", "Average Rating", "Success"))
        
        output$financial_line <- renderPlot({
          ggplot(financial_long, aes(x = variable, y = value, group = index, color = success_value)) +
            geom_line(data = subset(financial_long, success_value >= success_75)) +
            scale_color_gradient(low = "lightyellow", high = "red4") +
            theme_minimal() + 
            labs(x="", y="", color = "Success")
        })
        
        heatmap_genre <- heatmap_genre %>%
          separate_rows(genre, sep = ",\\s*")
        
        output$heatmap_genre <- renderPlot({
          ggplot(heatmap_genre, aes(x = month, y = genre, fill = success_norm)) +
            geom_tile() +
            scale_fill_gradient(low = "lightyellow1", high = "darkred") +
            labs(title = "Genre-Time-Success Heatmap",
                 x = "Month",
                 y = "Genre",
                 fill = "Success Norm") +
            theme_void() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme(axis.text.y = element_text(angle = 0, hjust = 1))
        })
        
        if(max(movie_data$success_norm) > 0.45) {
          movie_data <- subset(movie_data, success_norm > 0.45)  
        }
        
        # Calculate the average success for each genre-actor combination
        avg_success_by_genre_actor <- movie_data %>%
          dplyr::group_by(genre, first_actor) %>%
          dplyr::summarise(Avg_Success = mean(success_norm, na.rm = TRUE))
        
        output$genres_by_actor <- renderPlot({
          ggplot(avg_success_by_genre_actor, aes(x = first_actor, y = Avg_Success, fill = genre)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Average Success by Genre and Actor", x = "Actor", y = "Average Success") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        movie_data <- rv$df %>%
          dplyr::mutate(
            genre = strsplit(genre, ", "),
            director = strsplit(director, ", ")
          )
        
        movie_data <- movie_data %>%
          unnest_longer(director) %>%
          unnest_longer(genre)
        
        result_by_genre_director <- movie_data %>%
          dplyr::group_by(genre, director) %>%
          summarise(Avg_Success = mean(success_norm, na.rm = TRUE)) %>%
          unnest_longer(director)
        
        if (!is.null(input$directors_picker)) {
          result_by_genre_director <- result_by_genre_director %>%
            dplyr::filter(sapply(director, check_directors_subgroup))
        }
        
        if(max(result_by_genre_director$Avg_Success) > 0.45) {
          result_by_genre_director <- subset(result_by_genre_director, Avg_Success > .45)
        }
        
        output$genres_by_director <- renderPlot({
          ggplot(result_by_genre_director, aes(x = director, y = Avg_Success, fill = genre)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Average Success by Genre and Director", x = "Director", y = "Average Success") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
        result <- rv$df %>%
          dplyr::group_by(genre) %>%
          dplyr::summarise(avg_success = round(mean(success_norm),3))
        
        sorted_data <- result[order(-result$avg_success), ]
        
        colnames(sorted_data) <- c("Groups", "Avg Success")
        
        output$choose <- renderDT({
          datatable(sorted_data, options = list(pageLength = 5, lengthMenu = c(5, 10, 15), searching = FALSE), rownames = FALSE)
        })
      }
      
      trend_df <- rv$df %>%
        dplyr::mutate(
          genre = strsplit(genre, ", "),
          year = as.integer(format(as.Date( date_published), "%Y"))
        )
      
      if (rv$df_name == "theDataset") {
        yearly <- trend_df %>%
          dplyr::group_by(year ) %>%
          dplyr::summarise(mean_avg_vote = mean(avg_vote, na.rm = TRUE), 
                    mean_worlwide_gross_income = mean(worlwide_gross_income, na.rm = TRUE),
                    mean_reviews_from_users = mean(reviews_from_users, na.rm = TRUE),
                    mean_budget = mean(budget, na.rm = TRUE))
        
        future_years <- data.frame(year = 2021:2024)
        
        lm_model <- lm(mean_avg_vote ~ year, data = yearly)
        
        future_data <- data.frame(year = future_years$year)
        future_data$mean_avg_vote <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_worlwide_gross_income ~ year, data = yearly)
        future_data$mean_worlwide_gross_income <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_budget ~ year, data = yearly)
        future_data$mean_budget <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_reviews_from_users ~ year, data = yearly)
        future_data$mean_reviews_from_users <- predict(lm_model, newdata = future_data)
        
        extended_rating <- rbind(yearly, future_data)
        
        output$temporal_trend_avg_or_success <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_avg_vote)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Average Rating") +
            ggtitle("Temporal Trends in Mean Average Rating") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_world_income <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_worlwide_gross_income)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Worlwide gross Income") +
            ggtitle("Temporal Trends in Mean Worlwide gross Income") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_budget <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_budget)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Budget") +
            ggtitle("Temporal Trends in Mean Budget") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_reviews <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_reviews_from_users)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Reviews from users") +
            ggtitle("Temporal Trends in Reviews from users") +
            theme_minimal() +
            scale_color_identity()
        })
      } else {
        yearly <- trend_df %>%
          dplyr::group_by(year ) %>%
          dplyr::summarise(mean_success_norm = mean(success_norm, na.rm = TRUE), 
                    mean_worlwide_gross_income = mean(worlwide_gross_income, na.rm = TRUE),
                    mean_reviews_from_users = mean(reviews_from_users, na.rm = TRUE),
                    mean_budget = mean(budget, na.rm = TRUE))
        
        future_years <- data.frame(year = 2021:2024)
        
        lm_model <- lm(mean_success_norm ~ year, data = yearly)
        
        future_data <- data.frame(year = future_years$year)
        future_data$mean_success_norm <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_worlwide_gross_income ~ year, data = yearly)
        future_data$mean_worlwide_gross_income <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_budget ~ year, data = yearly)
        future_data$mean_budget <- predict(lm_model, newdata = future_data)
        lm_model <- lm(mean_reviews_from_users ~ year, data = yearly)
        future_data$mean_reviews_from_users <- predict(lm_model, newdata = future_data)
        
        extended_rating <- rbind(yearly, future_data)
        
        output$temporal_trend_avg_or_success <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_success_norm)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Success") +
            ggtitle("Temporal Trends in Success") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_world_income <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_worlwide_gross_income)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Worlwide gross Income") +
            ggtitle("Temporal Trends in Mean Worlwide gross Income") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_budget <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_budget)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Budget") +
            ggtitle("Temporal Trends in Mean Budget") +
            theme_minimal() +
            scale_color_identity()
        })
        
        output$temporal_trend_reviews <- renderPlot({
          ggplot(extended_rating, aes(x = year, y = mean_reviews_from_users)) +
            geom_point(aes(color = ifelse(year <= 2020, "dodgerblue3", "white"))) +
            geom_smooth(level = 0.95, color = "orange") +  
            labs(x = "Year", y = "Mean Reviews from users") +
            ggtitle("Temporal Trends in Reviews from users") +
            theme_minimal() +
            scale_color_identity()
        })
      }
      
    }
    
    if(!is.null(input$genre_picker)) {
      movie_data <- rv$df %>%
        dplyr::filter(sapply(genre, check_genre_subgroup)) %>%
        dplyr::mutate(
          director = strsplit(director, ", "),
          actors = strsplit(actors, ", ")
        )
    } else {
      movie_data <- rv$df %>%
        dplyr::mutate(
          director = strsplit(director, ", "),
          actors = strsplit(actors, ", ")
        )
    }
    
    for (i in 1:nrow(movie_data)) {
      movie_data$first_actor[i] <- movie_data$actors[[i]][1]
    }
  
    if (!is.null(input$actors_picker)) {
      if (rv$df_name == "theDataset") {
        grouped <- movie_data %>%
          dplyr::group_by(first_actor, director) %>%
          dplyr::summarise(freq = n()) %>%
          unnest_longer(director)
      } else {
        grouped <- movie_data %>%
          dplyr::group_by(first_actor, director) %>%
          dplyr::summarise(avg_success = round(mean(success_norm),3)) %>%
          unnest_longer(director)
      }
      if (!is.null(input$directors_picker)) {
        grouped <- grouped %>%
          dplyr::filter(sapply(first_actor, check_actors_subgroup)) %>%
          dplyr::filter(sapply(director, check_directors_subgroup))
      } else {
        grouped <- grouped %>%
          dplyr::filter(sapply(first_actor, check_actors_subgroup))
      }
    } else {
      if (rv$df_name == "theDataset") {
        grouped <- movie_data %>%
          dplyr::group_by(first_actor, director) %>%
          dplyr::summarise(freq = n()) %>%
          unnest_longer(director)
      } else {
        grouped <- movie_data %>%
          dplyr::group_by(first_actor, director) %>%
          dplyr::summarise(avg_success = round(mean(success_norm),3)) %>%
          unnest_longer(director)
          
      }
      if (!is.null(input$directors_picker)) {
        grouped <- grouped %>%
          dplyr::filter(sapply(director, check_directors_subgroup))
      }
    }
    if (rv$df_name == "theDataset") {
      if (mean(grouped$freq) >= 3) {
        grouped <- grouped %>%
          dplyr::filter(freq >= 3)
      }
      grouped <- head(grouped, 10)
    } else {
      grouped <- grouped %>%
        dplyr::filter(avg_success >= median(avg_success)) %>%
        dplyr::arrange(desc(avg_success))
      grouped <- head(grouped, 10)
    }
    
    graph_data <- graph_from_data_frame(grouped, directed = TRUE)
    
    V(graph_data)$color <- ifelse(V(graph_data)$name %in% grouped$first_actor, "lightblue", "lightgreen")
    
    output$network_actors_director <- renderPlot({
      plot(
        graph_data,
        edge.arrow.size = 0.5,
        edge.width = scales::rescale(E(graph_data)$freq, to = c(1, 8)),
        vertex.label.cex = 0.8,
        vertex.label.dist = 1.5,
        vertex.label.color = "black",
        vertex.label.family = "sans",
        vertex.label.font = 1.5,
        vertex.size = 12,
        layout = layout_nicely(graph_data),
        margin = 0.1
      )
      
      legend("topright", legend = c("Actor", "Director"), fill = c("lightblue", "lightgreen"))
    })
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
