suppressPackageStartupMessages({
    library(dplyr)
    library(jsonlite)
    library(httr)
    library(lubridate)
    library(purrr)
})

options(stringsAsFactors = FALSE)

#url <- "www.cbcmusic.ca/Component/Playlog/GetPlaylog?stationId=99&date="
url <- "https://www.cbcmusic.ca/Component/Playlog/GetPlaylog"


#pl <- GET(url, query = list(stationId = 99, date = "2017-08-04"))

get_day_log <-
    function(day){
        GET(url, query = list(stationId = 99, date = day))
    }

extract_signal <-
    function(get_req){
        tryCatch({
            progs <- content(get_req)$programs
            signal <- Filter(function(prog) prog$Title == "The Signal"
                           , progs)
            signal[[1]]
        }, error = function(e) return(NA))
    }


days <- seq(ymd("2015-01-01"), ymd("2017-08-04"), by = "day")

signal_data <-
    lapply(days,
           function(day)
               extract_signal(get_day_log(day)))

has_signal <-
    sapply(signal_data, function(x) !identical(x, NA))

signal_days <- days[has_signal]
signal_logs <- signal_data[has_signal]

signal_logs_dated <-
    mapply(
        function(log, day){
            log$Date <- day
            log
        }
      , log = signal_logs
      , day = signal_days
      , SIMPLIFY = FALSE)


signal_frame <-
    lapply(signal_logs_dated, function(log){
        lapply(log$Tracks, function(track){
            with(track
               , data_frame(Title = Title
                          , Artist = Artists[[1]]
                          , Time = Date
                          , Date = log[["Date"]]
                          , Album = Album
                          , Composers =
                                `if`(length(Composers) == 0
                                   , NA
                                   , Composers[[1]])))
        }) %>% bind_rows
    }) %>% bind_rows

saveRDS(signal_frame, "signal_playlogs.rds")

auth <- read.csv("oauth/signal_scraping_auth", header = FALSE)

id <- auth[1,2]
secret <- auth[2,2]


spotify_oep <- oauth_endpoint(authorize = "https://accounts.spotify.com/authorize"
                            , access = "https://accounts.spotify.com/api/token")

app_auth <- oauth_app("signal_scraping", key = id, secret = secret)

token <- oauth2.0_token(spotify_oep, app_auth, use_basic_auth=TRUE)

track <- 
    GET("https://api.spotify.com/v1/search"
      , query = list(q = "artist:regina+spektor track:eet"
                   , track = "eet"
                   , type = "track"
                   , limit = 1)
      , config(token = token))


format_query <-
    function(q)
        strsplit(gsub(" ", "+", q), ",")[[1]][1]

find_track <- function(artist, track){
    empty_track <- data_frame(Artist = artist
                              , Title = track
                              , url = NA
                              , spotify_id = NA
                              , spotify_uri = NA)
    tryCatch({
        resp <- 
            GET("https://api.spotify.com/v1/search"
              , query = list(q = sprintf("artist:%s track:%s"
                                       , format_query(artist)
                                       , format_query(track))
                           , type = "track"
                           , limit = 1)
              , config(token = token))

        track_obj <- content(resp)$tracks$items[[1]]
        
        data_frame(Artist = artist
                   , Title = track
                   , url = track_obj$external_urls$spotify
                   , spotify_id = track_obj$id
                   , spotify_uri = track_obj$uri)
        }, error = function(e){cat(artist, ":", track, "\n"); return(empty_track)})
    }


unique_tracks <-
    signal_frame %>%
    mutate(ta = paste0(Artist, " : ", Title)) %>%
    filter(!duplicated(ta))

spotify_results <- 
    transpose(unique_tracks) %>%
    lapply(function(t) find_track(t$Artist, t$Title)) %>%
    bind_rows

signal_frame_spotify <-
    inner_join(signal_frame, spotify_results
             , by = c("Artist", "Title"))

saveRDS(signal_frame_spotify, "signal_frame_spotify.rds")

write.table(signal_frame_spotify, "signal_frame_full.csv"
          , row.names = FALSE
          , sep = "\t")


signal_frame_spotify %>%
    mutate(ta = paste0(Artist, ":", Title)) %>%
    group_by(ta) %>%
    summarize(url = url[1], n = n(), artist = Artist[1], track = Title[1]) %>%
    arrange(desc(n)) %>%
    slice(11:20) %>%
    .$url %>%
    .[8] %>%
    BROWSE
