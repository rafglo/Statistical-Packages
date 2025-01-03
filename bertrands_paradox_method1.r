library(ggplot2)

# Parametry 
r <- 1  # Promień okręgu
n_chords <- 1000  # Liczba losowanych cięciw
a <- sqrt(3)  # Długość boku trójkąta równobocznego wpisanego w okrąg

# Ustalanie punktu początkowego P na obwodzie
alpha <- 2 * pi * runif(1)
x0 <- r * cos(alpha)
y0 <- r * sin(alpha)

triangle_alpha <- c(alpha, alpha + 2 * pi / 3, alpha - 2 * pi / 3)  # Uwzględnienie właściwej rotacji wierzchołków

triangle_points <- data.frame(
  x = r * cos(triangle_alpha),  # Pozycje x wierzchołków
  y = r * sin(triangle_alpha)   # Pozycje y wierzchołków
)

# Zamknięcie trójkąta (powrót do pierwszego wierzchołka)
triangle_points <- rbind(triangle_points, triangle_points[1, ])

count_longer <- 0

# Dane do wizualizacji
points <- data.frame(x = numeric(), y = numeric(), chord_color = character())
chords <- data.frame(x_start = numeric(), y_start = numeric(),
                     x_end = numeric(), y_end = numeric(), chord_color = character())

# Generowanie cięciw
for (i in 1:n_chords) {
  alpha <- 2 * pi * runif(1)
  x <- r * cos(alpha)
  y <- r * sin(alpha)
  chord_length <- sqrt((x0 - x)^2 + (y0 - y)^2)
  
  # Sprawdzenie długości cięciwy
  if (chord_length > a) {
    chord_color <- "green"
    count_longer <- count_longer + 1
  } else {
    chord_color <- "red"
  }
  
  # Dodawanie danych do wizualizacji
  points <- rbind(points, data.frame(x = x, y = y, chord_color = chord_color))
  chords <- rbind(chords, data.frame(x_start = x0, y_start = y0, 
                                     x_end = x, y_end = y, chord_color = chord_color))
}

# Okrąg
circle_points <- data.frame(
  x = r * cos(seq(0, 2 * pi, length.out = 100)),
  y = r * sin(seq(0, 2 * pi, length.out = 100))
)

ggplot() +
  geom_path(data = circle_points, aes(x = x, y = y), color = "black") +  # Okrąg
  geom_segment(data = chords, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = chord_color), size = 0.5) +  # Cięciwy
  geom_point(data = points, aes(x = x, y = y, color = chord_color), size = 1) +  # Punkty na obwodzie
  geom_polygon(data = triangle_points, aes(x = x, y = y), fill = NA, color = "blue", linetype = "dashed", size = 1) +  # Trójkąt
  scale_color_manual(values = c("green" = "green", "red" = "red")) +
  theme_minimal() +
  coord_equal() +
  labs(title = "Paradoks Bertranda - metoda 1", x = "X", y = "Y") +
  theme(legend.position = "none")

ggsave("C:/Users/rafal/OneDrive/Dokumente/GitHub/pakiety-statystyczne/metoda1.png")

# Obliczenie prawdopodobieństwa
p1 <- count_longer / n_chords
cat(sprintf("Prawdopodobieństwo: %.3f\n", p1))



