## code to prepare `MCUmovie` dataset

library(rvest)

mcu.meta <- read.csv("data-raw/MCUmovie-raw/mcu-data.csv")
mcu.url <- mcu.meta$URL
mcu.url.nums <- sapply(mcu.url, \(s) substr(s, gregexpr('/', s)[[1]][4], gregexpr('/', s)[[1]][5]))
mcu.url.cast <- paste0("https://www.imdb.com/title", mcu.url.nums, "fullcredits?ref_=tt_cl_sm")
mcu.cast <- vector("list",length=length(mcu.url))

mcu.worldwidegross <- vector(length=length(mcu.url))
for(i in seq_along(mcu.url)){
  # cat(mcu.meta$Title[i])
  read_html(mcu.url[i]) |>
    html_nodes(xpath='//*[@data-testid="title-boxoffice-section"]/ul/li[4]/div/ul/li') |>
    html_text(trim=TRUE) -> mcu.worldwidegross[i]
}
gsub(",","",substr(mcu.worldwidegross,2,nchar(mcu.worldwidegross))) |> as.numeric() -> mcu.worldwidegross

for(i in seq_along(mcu.url)){
  which("Rest of cast listed alphabetically:" == read_html(mcu.url.cast[i]) |>
          html_nodes(xpath='//*[@id="fullcredits_content"]/table[3]/*/td[1]') |>
          html_text(trim=TRUE)) - 2 -> cast.index
  read_html(mcu.url.cast[i]) |>
    html_nodes(xpath='//*[@id="fullcredits_content"]/table[3]/*/td[2]') |>
    html_text(trim=TRUE) -> cast.full
  mcu.cast[[i]] <- cast.full[1:cast.index]
}

weight <- matrix(0, nrow=32, ncol=32)
rownames(weight) <- colnames(weight) <- mcu.meta$Title
for(i in 1:31){
  for(j in (i+1):32){
    union.length <- c(mcu.cast[[i]],mcu.cast[[j]]) |> unique() |> length()
    weight[i,j] <- (length(c(mcu.cast[[i]],mcu.cast[[j]]))-union.length)/union.length
  }
}
weight <- weight + t(weight)

# Adj matrix
weight.dist <- 1/weight
weight.dist[weight.dist == Inf] <- 0

MCUmovie <- igraph::graph_from_adjacency_matrix(weight.dist, mode = "undirected", weighted = TRUE)
V(MCUmovie)$worldwidegross <- mcu.worldwidegross
V(MCUmovie)$year <- mcu.meta$Year

usethis::use_data(MCUmovie, overwrite = TRUE)
