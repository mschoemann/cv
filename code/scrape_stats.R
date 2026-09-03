# setup ----
pacman::p_load(
    readr,
    dplyr
)

source("code/get_ids.R")
source("code/functions_scrape-stats.R")


# scrape ----
stats = get_stats(gscholar_id)
pubs = get_cites(gscholar_id) |>
    arrange(desc(year))

write_csv(stats, "data/scholar_stats.csv")
write_csv(pubs, "data/scholar_pubs.csv")


# log scrape ----
log_path = "data/scrape_log.csv"

log_entry = tibble(
    timestamp    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stats_ok     = !is.null(stats),
    pubs_ok      = !is.null(pubs),
    n_pubs       = if (!is.null(pubs)) nrow(pubs) else NA_integer_,
    citations    = if (!is.null(stats)) stats$citations else NA_integer_,
    hindex       = if (!is.null(stats)) stats$hindex else NA_integer_,
    i10index     = if (!is.null(stats)) stats$i10index else NA_integer_
)

# Append if the log exists, otherwise create it with a header
if (file.exists(log_path)) {
    write_csv(log_entry, log_path)
} else {
    write_csv(log_entry, log_path)   # writes header on first run
}
