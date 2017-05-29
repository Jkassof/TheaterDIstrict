library(rvest)
library(stringr)
library(tibble)
theaters <- read_html("https://www.ibdb.com/theatres")
links <- theaters %>%
  html_nodes("#nyc a") %>%
  html_attr("href")

stub <- "https://www.ibdb.com"
detail_pages <- paste0(stub, links)
theater_data <- tibble::tibble(name = character(),
                               seats = character(),
                               established = character(),
                               history = list(),
                               productions = list()
)

for (i in seq_along(1:length(detail_pages))) {
  page <- read_html(detail_pages[i])
  
  name <- page %>% 
    html_node("b") %>% 
    html_text()
  
  description <- page %>%
    html_node(".production-page") %>%
    html_text() %>%
    str_replace_all("[\r\n]", "") 
  
  seat_count <- description %>%
    str_extract("Seats.*?\\d{3,5}")
  
  built <- description %>%
    str_extract("Built: \\d{4}")
  
  name_history <- page %>%
    html_node("table.venu-listing") %>%
    html_table()
  
  productions <- page %>%
    html_nodes("table.venu-listing1.venu-page") %>%
    html_table() %>%
    dplyr::bind_rows()
  
  theater_data <- add_row(theater_data,
                          name = name,
                          seats = seat_count,
                          established = built,
                          history = list(name_history),
                          productions = list(productions))
  
}

theater_data$name <- trimws(str_replace(theater_data$name, "Theatre", ""))

theater_district <- readLines("bwaylayout.csv")

district_layout <- paste(theater_district, collapse = "\n") 
custom_layout <- str_replace(district_layout, "'", "")

custom_labels <- vector(mode = "list", length = nrow(theater_data))

names(custom_labels) <- theater_data$name

for (i in 1:nrow(theater_data)) {
  custom_labels[[i]] <- list(full = theater_data$name[i],
                           short = theater_data$name[i])
}


library(rsquaire)

rsquaire(theater_data[,1:3],
         layout = district_layout,
         labels = custom_labels,
         index = "established",
         indexType = "string",
         categories = theater_data$established,
         labelStyle = "full",
         layout_key = "name",
         width = "400",
         height = "600")


