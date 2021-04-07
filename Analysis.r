library(needs)
## Load `package:needs` in an interactive session to set auto-load flag

needs(tidyverse,
      plotly,
      formattable,
      lme4)

anime <- read_csv('./anime.csv')

anime

anime %>% 
  head(10)

rating <- read_csv("./rating.csv")

rating %>% 
  head(10)

rating %>% 
  group_by(rating) %>% 
  summarise(ratings = n(), people = n_distinct(user_id))

rating <- rating %>% mutate(rating = ifelse(rating == -1, NA, rating))
rating %>% 
  count(rating)

g <- anime %>% 
  filter(!is.na(rating)) %>% 
  ggplot(aes(rating, group = type)) +  geom_density(aes(fill = type), alpha = .4) +   xlim(0, 10)

ggplotly(g)

m1 = lmer(rating ~ (1 + log(members) | type), data = anime)
coef(m1)

anime %>% 
  group_by(type) %>% 
  summarise(
    average.people = mean(members),
    sd.people = sd(members),
    average.rating = mean(rating, na.rm = T),
    sd.rating = sd(rating, na.rm = T)
  ) %>% 
  formattable()

g <- rating %>% 
  count(user_id) %>% 
  filter(n < 500) %>% 
  ggplot(aes(n)) +
  geom_density()

ggplotly(g)

anime %>% 
  mutate(genre = map(genre, ~ strsplit(.x, ", ") %>% unlist())) %>% 
  unnest(genre) ->
  shows_by_genre

g <- shows_by_genre %>% 
  ggplot(aes(rating, group = genre)) +
  geom_density(aes(fill = genre), alpha = .4)

ggplotly(g)

g <- rating %>% 
  group_by(user_id) %>% 
  summarise(sd = sd(rating)) %>% 
  ggplot(aes(sd)) +
  geom_density()

ggplotly(g)

g <- rating %>% 
  group_by(user_id) %>% 
  summarise(m = mean(rating)) %>% 
  ggplot(aes(m)) +
  geom_density()

ggplotly(g)

anime %>% 
  mutate(episodes = as.numeric(episodes)) %>% 
  filter(!is.na(episodes)) %>% 
  group_by(type) %>% 
  summarise(
    average.episodes = mean(episodes),
    sd.episodes = sd(episodes),
    average.rating = mean(rating, na.rm = T),
    sd.rating = sd(rating, na.rm = T)
  ) %>% 
  formattable()

m2 = lmer(rating ~ (1 + as.numeric(episodes) | type), data = anime)

coef(m2)

g <- anime %>% 
  filter(!is.na(type)) %>% 
  mutate(episodes = as.numeric(episodes)) %>% 
  ggplot(aes(episodes, rating)) +
  geom_point(aes(color = type)) + 
  facet_wrap(~ type, scales = "free_x")

ggplotly(g)

m3 <- lm(log(members) ~ log(as.numeric(episodes)), data = anime)

summary(m3)

g <- anime %>% 
  filter(!is.na(type)) %>% 
  ggplot(aes(log(members), log1p(as.numeric(episodes)))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ type, scales = "free_y")

ggplotly(g)

m4 <- lmer(log(as.numeric(episodes)) ~ (1 + log(as.numeric(members)) | type), data = anime)

coef(m4)


