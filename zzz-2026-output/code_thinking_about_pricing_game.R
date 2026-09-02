library(tidyverse)
setwd("~/Downloads/")

profits <- read_csv("pricing_game/all_profits.csv") |>
  janitor::clean_names()

profits |> 
  select(starts_with("student_1"), game) |>
  rename(profit = student_1_total_profit,
         student = student_1) |>
  mutate(profit = str_remove_all(profit, "\\$"),
         profit = str_remove_all(profit, ",") |> as.numeric(),
         game = case_when(
           game == "Bertrand" ~ "Bertrand",
           game == "Hoteling 1" ~ "Hoteling (high)",
           TRUE ~ "Hoteling (low)"
         )) |>
  pivot_wider(names_from = game, values_from = profit) |>
  select(student, Bertrand, everything()) |>
  mutate(Total = Bertrand + `Hoteling (high)` + `Hoteling (low)`) |>
  arrange(desc(Total))
  

profits |> 
  pivot_longer(student_1_total_profit:total_profit, values_to = "profit") |>
  mutate(profit = str_remove_all(profit, "\\$"),
         profit = str_remove_all(profit, ",") |> as.numeric(),
         game = case_when(
           game == "Bertrand" ~ "Bertrand",
           game == "Hoteling 1" ~ "Hoteling (high)",
           TRUE ~ "Hoteling (low)"
         )) |>
  pivot_wider(values_from = profit) |>
  arrange(total_profit) |>
  group_by(total_profit, game) |>
  slice(1) |>
  split(~game) 
 

df <- list.files("pricing_game/", pattern = "\\pricing.*.csv$", full.names = TRUE) |>
  map(read_csv, show_col_types = FALSE) |>
  list_rbind() |>
  janitor::clean_names()


simulate_market <- function(p1,p2, t = 1, v = 250) {
  x <- 0:100
  u1 <- map_dbl(x, \(x) v - p1 - t*x) 
  u2 <- map_dbl(x, \(x) v - p2 - t*(100 - x))
  tibble(p1 = p1, p2 = p2, 
         d1 = sum(u1 > u2 & u1 > 0), 
         d2 = sum(u2 > u1 & u2 > 0),
         r1 = p1 * d1,
         r2 = p2 * d2)
}

simulate_many_markets <- function(p1_range, p2_range) {
  d <- expand_grid(p1 = p1_range, p2 = p2_range) 
  
  map2_dfr(d$p1, d$p2, \(x, y) simulate_market(x, y))
    
}

markets <- simulate_many_markets(seq(0, 250, 10), seq(0, 250, 10))
monopoly <- simulate_many_markets(1:250, 500)
monopoly |> 
  ggplot(aes(y = p1, x = d1, color = r1)) + 
  geom_point() + 
  scale_color_viridis_c()



br <- simulate_many_markets(1:250, c(100, 150, 200))
br |> 
  ggplot(aes(y = p1, x = d1, color = r1)) + 
  geom_point() + 
  scale_color_viridis_c() +
  theme_minimal() + 
  facet_wrap(~p2)



## Slide 4

df |> 
  filter(game_type == "Bertrand") |>
  ggplot(aes(x = round, y = price, color = name)) + 
  geom_line() +
  geom_point() + 
  facet_wrap(~market_id) + theme_minimal()



## Monopoly price in Bertrand # Slide 5

p <- 1:100
profit = case_when(p<50 ~ (100 - p) * p, 
                   p==50 ~ (100 - p) * p/2,
                   p > 50 ~ 0)
ggplot(data = NULL, aes(x = p, y = profit)) + 
  geom_line() + 
  geom_point(aes(x = 10, y = 900), size=3, color = "forestgreen") +
  geom_point(aes(x = 50, y = 1250)) +
  geom_vline(aes(xintercept = 49), color = "red", linetype="dashed") +
  annotate("text", x = 20, y = 900, label = "Actual: $10", 
           vjust = -1, color = "forestgreen", size = 4.5) +
  annotate("text", x = 49, y = 2400, label = "Best Response: $49", 
           hjust = -0.1, vjust = -0.5, color = "red", size = 4.5) +
  theme_minimal() +
  labs(title = "Bertrand: Steve's Choice Round 3",
       )



## Hoteling plot 1

df |> 
  filter(game_type == "hoteling_1") |>
  ggplot(aes(x = round, y = price, color = name)) + 
  geom_line() +
  geom_hline(aes(yintercept = 100), color = "red", linetype="dashed") + 
annotate("text", x = 8, y = 80, label = "NE: $100", 
         hjust = -0.1, vjust = -0.5, color = "red", size = 3) +
geom_hline(aes(yintercept = 200), color = "darkgreen", linetype="dashed") + 
  annotate("text", x = 5, y = 205, label = "Monopoly: $200", 
           hjust = -0.1, vjust = -0.5, color = "darkgreen", size = 3) +
  geom_point() + coord_cartesian(ylim = c(0, 215)) +
  facet_wrap(~market_id) + 
  theme_minimal()


# Best Response Hoteling 1
p <- 1:200
br <- function(p) 50 + p/2
p1 <- 105
p2 <- 200
profit1 <- p * (50 + (p2 - p)/2) # player 2 plays 200
plot1 <- ggplot(data = NULL, aes(x = p, y = profit1)) + 
  geom_line() + 
  theme_minimal() +
  geom_point(aes(x = 105, y = profit1[[105]]), size = 3, color = "forestgreen") + 
  geom_vline(aes(xintercept = br(p2)), color = "red", linetype = "dotted") +
  
  labs(title =  "Wilson response to Habib (Round 8)") 


plot1

profit2 <- p * (50 + (p1 - p)/2) # player 2 plays 200
plot2 <- ggplot(data = NULL, aes(x = p, y = profit2)) + 
  geom_line() + 
  theme_minimal() +
  geom_point(aes(x = p2, y = profit[[p2]]), size = 3, color = "forestgreen") + 
  geom_vline(aes(xintercept = br(p1)), color = "red", linetype = "dotted") +
  
  labs(title  = "Habib response to Wilson (Round 8)") 

plot1 / plot2

## Hoteling plot 2

df |> 
  filter(game_type == "hoteling_5") |>
  ggplot(aes(x = round, y = price, color = name)) + 
  geom_line() +
  geom_hline(aes(yintercept = 100), color = "red", linetype="dashed") + 
  annotate("text", x = 8, y = 80, label = "NE: $100", 
           hjust = -0.1, vjust = -0.5, color = "red", size = 3) +
  geom_hline(aes(yintercept = 225), color = "darkgreen", linetype="dashed") + 
  annotate("text", x = 5, y = 230, label = "Monopoly: $225", 
           hjust = -0.1, vjust = -0.5, color = "darkgreen", size = 3) +
  geom_point() + coord_cartesian(ylim = c(0, 245)) +
  facet_wrap(~market_id) + 
  theme_minimal()


# Best Response Hoteling .5
p <- 1:200
p2 <- 100
br <- function(p2) 50 + p2/2
profit <- p * (100 + (p2 - p)) # player 2 plays 200
ggplot(data = NULL, aes(x = p, y = profit)) + 
  geom_line() + 
  theme_minimal() +
  geom_point(aes(x = 105, y = profit[[105]]), size = 3, color = "forestgreen") + 
  geom_vline(aes(xintercept = br(p2)), color = "red", linetype = "dotted") +
  
  labs(title = glue::glue("Profit for player 1 if player 2 charges ${p2}"),
       subtitle = "Wilson response to Habib (Round 8)") 

