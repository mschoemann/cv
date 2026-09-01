pacman::p_load(
    readr,
    stringr,
    lubridate,
    scales,
    dplyr,
    googlesheets4,
    httr,
    xml2,
    rvest,
    pander,
    kableExtra,
    scholar
)

gs4_deauth()

gscholar_stats <- function(gscholar_id) {
  cites = get_stats(gscholar_id)
  hy = "\u2010"   # true Unicode hyphen
  return(paste0(
      'citations: ', cites$citations, ' • ',
      'h', hy, 'index: ',   cites$hindex, ' • ',
      'i10', hy, 'index: ', cites$i10index
  ))
}

get_cites <- function(url) {
    html <- tryCatch(xml2::read_html(url), error = function(e) NULL)
    if (is.null(html)) {
        message("Could not fetch Scholar page (possibly blocked).")
        return(NULL)
    }

    node <- rvest::html_nodes(html, xpath = '//*[@id="gsc_rsb_st"]')
    if (length(node) == 0) {
        message("Summary table not found - Scholar may have served a block page.")
        return(NULL)
    }

    cites_df <- rvest::html_table(node)[[1]]
    cites <- data.frame(t(as.data.frame(cites_df)[, 2]))
    names(cites) <- c('citations', 'hindex', 'i10index')
    return(cites)
}

get_stats <- function(gscholar_id) {
    profile = get_profile(gscholar_id)
    return(
        tibble(
            citations = profile$total_cites,
            hindex = profile$h_index,
            i10index = profile$i10_index
        )
    )
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
    return(pandoc.list(x, style = 'ordered', loose = TRUE))
}

make_bullet_list <- function(x) {
  return(pandoc.list(x, style = 'bullet', loose = TRUE))
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

specdec = function(x, k) trimws(format(round(x, k), nsmall=k))
