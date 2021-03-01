library(dplyr)

anime <- read.csv('anime.csv')

count(anime)

head(anime)

tv_anime <- filter(anime, type=='TV')

tv_anime <- select(tv_anime, anime_id, name, genre, type, episodes, rating)

head(tv_anime)

count(tv_anime)

ratings <- read.csv('rating.csv')

head(ratings)

new_ratings <- filter(ratings, rating!=-1)

new_ratings

tv_anime_user_ratings <- filter(new_ratings, anime_id %in% tv_anime$anime_id)

count(tv_anime_user_ratings)

head(tv_anime_user_ratings)

write.csv(tv_anime, "C:\\Users\\Tarun Luthra\\Desktop\\Anime recommendation system\\Tidy data\\TV Animes.csv",row.names = FALSE)

write.csv(tv_anime_user_ratings, "C:\\Users\\Tarun Luthra\\Desktop\\Anime recommendation system\\Tidy data\\TV User Ratings.csv", row.names = FALSE)


