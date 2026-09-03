# setup ----
pacman::p_load(
    scholar
)

# functions ----
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

get_cites <- function(gscholar_id) {
    res = get_publications(gscholar_id) |>
        select(
            id_scholar = pubid,
            year, title, cites,
        )
    return(res)
}
