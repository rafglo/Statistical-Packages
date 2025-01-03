library(ggplot2)
set.seed(8)

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

beta <- abs(alpha - pi/2)

x_mid <- x0/2
y_mid <- y0/2
dist_mid <- sqrt((x_mid)^2 + (y_mid)^2)

chords <- data.frame(x_start = numeric(), y_start = numeric(),
                     x_end = numeric(), y_end = numeric(), chord_color = character())

count_longer <- 0

for (i in 1:n_chords) {
  m <- r * runif(1)
  x <- m * cos(alpha)
  y <- m * sin(alpha)
  dist <- sqrt((x)^2 + (y)^2)
  
  chord_length = sqrt(r^2 - dist^2)
  x1 <- x + chord_length * cos(beta)
  y1 <- y + chord_length * sin(beta)
  x2 <- x - chord_length * cos(beta)
  y2 <- y - chord_length * sin(beta)
  # Sprawdzenie długości cięciwy
  if (dist_mid < dist) {
    chord_color <- "red"
  }
  else {
    chord_color <- "green"
    count_longer <- count_longer + 1
  }
  chords <- rbind(chords, data.frame(x_start = x1, y_start = y1, 
                                     x_end = x2, y_end = y2, chord_color = chord_color))
}

circle_points <- data.frame(
  x = r * cos(seq(0, 2 * pi, length.out = 100)),
  y = r * sin(seq(0, 2 * pi, length.out = 100))
)

radius <- data.frame(
  x_start = 0,
  y_start = 0,
  x_end = r * cos(alpha),
  y_end = r * sin(alpha)
)

ggplot() +
  geom_path(data = circle_points, aes(x = x, y = y), color = "black") +  # Okrąg
  geom_segment(data = chords, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = chord_color), size = 0.5) +  # Cięciwy
  geom_polygon(data = triangle_points, aes(x = x, y = y), fill = NA, color = "blue", linetype = "dashed", size = 1) +  # Trójkąt
  scale_color_manual(values = c("green" = "green", "red" = "red")) +
  geom_segment(data = radius, aes(x = x_start, y = y_start, xend = x_end, yend = y_end), color = "black", size = 1.2) +
  theme_minimal() +
  coord_equal() +
  labs(title = "Paradoks Bertranda - metoda 2", x = "X", y = "Y") +
  theme(legend.position = "none")

p2 <- count_longer / n_chords
cat(sprintf("Prawdopodobieństwo: %.3f\n", p2))

