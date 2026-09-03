# setup ----
pacman::p_load(
    readr,
    stringr,
    lubridate,
    scales,
    dplyr,
    googlesheets4,
    pander,
    kableExtra
)

gs4_deauth()


# functions ----

## gscholar ----
gscholar_stats = function(gscholar_id) {
    stats = read_csv("data/scholar_stats.csv", show_col_types = FALSE)
    hy = "\u2010"  # true Unicode hyphen
    return(
        paste0(
            'citations: ', stats$citations, ' • ',
            'h', hy, 'index: ', stats$hindex, ' • ',
            'i10', hy, 'index: ', stats$i10index
        )
    )
}

gscholar_cites = function(gscholar_id) {
    pubs = read_csv("data/scholar_pubs.csv", show_col_types = FALSE) |>
        select(
            id_scholar,
            cites
        )
    return(pubs)
}


## google ----
get_cv_sheet = function(sheet) {
    gurl = 'https://docs.google.com/spreadsheets/d/1zH08x-6om6SzIyDuByLtm-zYK9Fl_QS57CRAckKTu4o/edit?usp=sharing'
    return(
        read_sheet(
            ss = gurl,
            sheet = sheet
        )
    )
}


## pandoc ----
make_ordered_list = function(x) {
    return(
        pandoc.list(
            x, style = 'ordered', loose = TRUE
        )
    )
}

make_bullet_list = function(x) {
    return(
        pandoc.list(
            x, style = 'bullet', loose = TRUE
        )
    )
}

make_ordered_list_filtered = function(df, cat) {
    #TODO
  return(
      df |>
          filter(category == {{cat}}) |>
        # mutate(
        #     citation = str_replace_all(
        #         citation,
        #         "\\\\\\*(\\w+),",
        #         "\\\\*\\\\underline{\\1},"
        #     )
        # ) |>
        pull(citation) |>
          make_ordered_list()
  )
}

na_to_space = function(x) {
    return(
        ifelse(is.na(x), ' ', x)
    )
}

enquote <- function(x) {
    return(
        paste0('"', x, '"')
    )
}

markdown_url <- function(url) {
    return(
        paste0('[', url, '](', url,')')
    )
}

make_grants_list <- function(df) {
    return(
        df |>
            mutate(
                title = if_else(
                    is.na(url),
                    title,
                    paste0('[', title, ']', '(', url, ')')
                ),
                citation = paste(
                    sponsor, title, colabs, credit, period, sep = '. '
                )
            ) |>
            pull(citation) |>
            make_ordered_list()
    )
}

label_euro <- scales::label_currency(
    prefix = "€",
    suffix = "",
    big.mark = ",",
    decimal.mark = "."
)

specdec = function(x, k) trimws(format(round(x, k), nsmall=k))
