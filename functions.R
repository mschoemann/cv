library(groundhog)

pkgs = c(
    "dplyr",
    "stringr",
    "lubridate",
    "scales",
    "googlesheets4",
    "httr",
    "xml2",
    "rvest",
    "pander",
    "kableExtra",
    "scholar"
)

groundhog.library(
    pkgs,
    "2025-07-01"
)

gs4_deauth()

gscholar_stats <- function(url) {
  cites <- get_stats(url)
  return(paste(
    'Citations:', cites$citations, '•',
    'h-index:',   cites$hindex, '•',
    'i10-index:', cites$i10index
  ))
}

get_stats <- function(url, max_tries = 3) {
    for (i in 1:max_tries) {
        tryCatch({
            # Add delay to avoid rate limiting
            if (i > 1) Sys.sleep(5)

            # Fetch with proper headers (Linux Mint 22.2)
            response <- GET(
                url,
                user_agent("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"),
                timeout(30)
            )

            # Check status
            stop_for_status(response)

            # Parse HTML
            html <- read_html(content(response, as = "text", encoding = "UTF-8"))
            node <- html_nodes(html, xpath = '//*[@id="gsc_rsb_st"]')

            if (length(node) == 0) {
                stop("Could not find statistics table")
            }

            cites_df <- html_table(node)[[1]]
            cites <- data.frame(t(as.data.frame(cites_df)[, 2]))
            names(cites) <- c('citations', 'hindex', 'i10index')

            return(cites)

        }, error = function(e) {
            if (i == max_tries) {
                stop(paste("Failed after", max_tries, "attempts:", e$message))
            }
            message(paste("Attempt", i, "failed, retrying..."))
        })
    }
}

gscholar_cites <- function(gscholar_id) {
    res = get_publications(gscholar_id) |>
        select(
            id_scholar = pubid,
            cites
        )
    return(res)
}

get_cv_sheet <- function(sheet) {
    return(read_sheet(
        ss = 'https://docs.google.com/spreadsheets/d/1zH08x-6om6SzIyDuByLtm-zYK9Fl_QS57CRAckKTu4o/edit?usp=sharing',
        sheet = sheet
    ))
}

make_ordered_list <- function(x) {
    return(pandoc.list(x, style = 'ordered'))
}

make_bullet_list <- function(x) {
  return(pandoc.list(x, style = 'bullet'))
}

make_ordered_list_filtered <- function(df, cat) {
  return(df |>
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

na_to_space <- function(x) {
    return(ifelse(is.na(x), '', x))
}

enquote <- function(x) {
    return(paste0('"', x, '"'))
}

markdown_url <- function(url) {
    return(paste0('[', url, '](', url,')'))
}

make_grants_list <- function(df) {
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
}

label_euro <- scales::label_currency(
    prefix = "€",
    suffix = "",
    big.mark = ",",
    decimal.mark = "."
)
