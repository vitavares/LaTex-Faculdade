g_south = ggplot(murders[murders$regiao == "South", ], aes(y = populacao, x = assassinatos)) +
  geom_point() +
  labs(subtitle = "South",
       y = "População", x = "Assassinatos",
       caption = "Fonte: https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state")

g_west = ggplot(murders[murders$regiao == "West", ], aes(y = populacao, x = assassinatos)) +
  geom_point() +
  labs(subtitle = "West",
       y = "População", x = "Assassinatos",
       caption = "Fonte: https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state")

g_northeast = ggplot(murders[murders$regiao == "Northeast", ], aes(y = populacao, x = assassinatos)) +
  geom_point() +
  labs(subtitle = "Northeast",
       y = "População", x = "Assassinatos",
       caption = "Fonte: https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state")

g_northCentral = ggplot(murders[murders$regiao == "North Central", ], aes(y = populacao, x = assassinatos)) +
  geom_point() +
  labs(subtitle = "North Central",
       y = "População", x = "Assassinatos",
       caption = "Fonte: https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state")

grid.arrange(g_south, g_west, g_northeast, g_northCentral, nrow = 2, ncol = 2)
