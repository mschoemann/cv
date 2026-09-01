source("code/functions.R")

gscholar_id = 'EdZjQtsAAAAJ'
stats = get_stats(gscholar_id)
pubs = get_publications(gscholar_id) |>
    select(year, title, pubid, cites) |>
    arrange(desc(year))


write_csv(stats, "data/scholar_stats.csv")
write_csv(pubs,  "data/scholar_pubs.csv")
