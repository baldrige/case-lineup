library(shiny)
library(gt)
library(bslib)
library(tidyverse)
library(lubridate)

load_justices <- function() {
  justicebio <- read_csv("justicebio.csv", col_types = "fccDDc")
  return(justicebio)
}

df <- read_csv("scdb.csv", col_types = "cccccfcccciffccccffffffffffffffffffffffffffffffcfffffffffffff")

case_lineup <- function(citation) {
  cite <- str_split_fixed(citation, " U.S. ", 2)
  vol <- str_pad(cite[1], width=3, pad="0")
  page <- cite[2]
  cite_url <- paste0("https://tile.loc.gov/storage-services/service/ll/usrep/usrep", vol, "/usrep", vol, page, "/usrep", vol, page, ".pdf")
  jus <- load_justices()
  df <- df |> 
    filter(usCite == citation) |> 
    select(justice, justiceName, caseName, dateDecision, vote, opinion, majority)
  caption <- df$caseName[1]  
  year <- year(df$dateDecision[1])
  df <- df |> left_join(jus, by = c("justice", "justiceName"))
  df |> select(-justice, -justiceName, -dateDecision, -Start, -End, -caseName) |>
    select(image, Name, majority, vote, opinion) |> 
    arrange(majority, vote, desc(opinion)) |>
    mutate(majority = case_match(majority, "1" ~ "Dissent", "2" ~ "Majority")) |>
    mutate(vote = case_match(vote, 
      "1" ~ "Voted with Majority or Plurality", 
      "2" ~ "Dissent", 
      "3" ~ "Regular Concurrence",
      "4" ~ "Special Concurence",
      "5" ~ "Judgment of the Court",
      "6" ~ "Dissental",
      "7" ~ "Jurisdictional Dissent",
      "8" ~ "Equally Divided Vote"
    )) |>
    mutate(opinion = case_match(opinion,
      "1" ~ "No opinion",
      "2" ~ "Authored opinion",
      "3" ~ "Co-authored Opinion"
    )) |> 
    group_by(majority) |> 
    gt(row_group_as_column = TRUE) |> 
    tab_header(title = caption, subtitle = md(paste0('[', citation, " ", "(", year, ")", '](', cite_url, ')'))) |>
    fmt_image(columns = "image") |>
    cols_label(image = "", majority = "Vote", vote = "Vote Type", opinion = "Opinion?") |>
    data_color(columns = c(vote)) |>
    tab_style(style = cell_text(weight = "bold", v_align = "middle"), locations = cells_row_groups())
}

ui <- page_sidebar(
  title = "Supreme Court Case Lineup Tool",
  sidebar = sidebar(
    textInput(inputId = "citation", label = "Enter U.S. Reports citation", value = "5 U.S. 137"),
    actionButton("refresh", "Generate Lineup")
  ),
  
  card(
    gt::gt_output("table")
  )
)

server <- function(input, output, session) {
  lineup_data <- eventReactive(input$refresh, {
    case_lineup(input$citation)
  })
  
  output$table <- gt::render_gt({
    lineup_data()
  })
}

shinyApp(ui, server)