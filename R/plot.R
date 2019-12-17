# Plots -------------------------------------------------------------------



plot_genres_2019 <- top_genres %>%
  filter(year_read == 2019) %>%
  ggplot(
    aes(
      x = reorder(shelf,books),
      y = books,
      color = shelf,
      fill = shelf,
      label = books
    )
  ) +
  geom_col() +
  geom_text(hjust = 1.5, color = 'black', alpha = .7, size = 3) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.title = element_blank(),
    axis.text.x = element_blank()
  ) +
  coord_flip() +
  labs(title = 'Top Genre Tags: 2019', subtitle = 'By Count of Books') 


plot_genres_2018 <- top_genres %>%
  filter(year_read == 2018) %>%
  ggplot(
    aes(
      x = reorder(shelf,books),
      y = books,
      color = shelf,
      fill = shelf,
      label = books
    )
  ) +
  geom_col() +
  geom_text(hjust = 1.5, color = 'black', alpha = .7, size = 3) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.title = element_blank(),
    axis.text.x = element_blank()
  ) +
  coord_flip() +
  labs(title = 'Top Genre Tags: 2018', subtitle = 'By Count of Books') 




