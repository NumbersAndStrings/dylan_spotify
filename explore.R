#' # Analyzing Artist Songs and Popularity from Spotify
#'
#' First, I installed the R-package spotifyr and obtain and set up a Spotify 
#' cliend id and secret [according to the 
#' directions](https://www.rcharlie.com/spotifyr).
#' 
#' We can find the artist ID by using the `search_spotify` function:
#'
#+ search, echo=TRUE
library(tidyverse)
library(spotifyr)
library(jsonlite)
artist_ids <- search_spotify('Bob Dylan', type="artist") 

pull(artist_ids, name)
#' The first entry is the one we want (though Blind Boy Grunt should probably be 
#' in play as well).

bob_id <- artist_ids %>% slice(1) %>% pull(id)

#' First, let's take a look at track popularity. As well known as Bob's album 
#' sales may be over time, we know little about individual tracks (beyond, say 
#' single sales and and radio play data which may be available). First, I need 
#' to get a list of all the albums. For this I'll use `get_artist_albums`.  
#' There's a limit of 50 albums, which I'm guessing, with live albums, 
#' compilations, and bootleg series, etc stuff, will not get all of them. I 
#' don't think there's a way to get a count of the number of albums, so I'll 
#' have to do something like this:

albums <- tibble()
offset <- 0
these_albums <- get_artist_albums(bob_id, limit=50, offset=offset)
while (length(these_albums) > 0) {
  albums <- bind_rows(albums, these_albums)
  offset <- offset + 50
  these_albums <- get_artist_albums(bob_id, limit=50, offset=offset)
}

nrow(albums)

#' As I suspected, there's actually `nrow(albums)`.
#' Now that we have a list of albums (and album ids), we can get play counts for 
#' individual tracks. But we have to do that an album at a time. In doing so, 
#' we'll employ 
library(httr)

get_playcount <- function(album_id, 
                  server_url='https://t4ils.dev:4433/api/beta/albumPlayCount') {
  count_response <- GET(server_url, query=list(albumid=album_id))
  playcount <- fromJSON(content(count_response, as="text"))$data
  playcount$track_id <- sub('spotify:track:', '', playcount$uri)
    
  playcount
}

playcounts <- map_dfr(albums$id[1:3], get_playcount)

get_track_info <- function(track_id) {
  c(get_tracks(track_id),
    get_track_audio_analysis(track_id),
    get_track_audio_features(track_id))
}

track_info <- map_dfr(playcounts$track_id, get_track_info)

