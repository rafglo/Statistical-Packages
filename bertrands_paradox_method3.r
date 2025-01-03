library(ggplot2)
set.seed(10)

r = 1
n_chords = 1000
a = sqrt(3)

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

#Małe kółko 
small_circle_points <- data.frame(
  x = r/2 * cos(seq(0, 2 * pi, length.out = 100)),
  y = r/2 * sin(seq(0, 2 * pi, length.out = 100))
)

# Dane do wizualizacji
points <- data.frame(x = numeric(), y = numeric(), chord_color = character())

count_longer <- 0
for (i in 1:n_chords) {
  beta <- 2 * pi * runif(1)
  m <- sqrt(runif(1))
  x <- m * r * cos(beta)
  y <- m * r * sin(beta)
  dist <- sqrt((x)^2 + (y)^2)
  
  if (dist > r / 2) {
    chord_color <- "red"
  }
  else {
    chord_color <- "green"
    count_longer <- count_longer + 1
  }
  points <- rbind(points, data.frame(x = x, y = y, chord_color = chord_color))
}

circle_points <- data.frame(
  x = r * cos(seq(0, 2 * pi, length.out = 100)),
  y = r * sin(seq(0, 2 * pi, length.out = 100))
)

ggplot() +
  geom_path(data = circle_points, aes(x = x, y = y), color = "black") +  # Okrąg
  geom_path(data = small_circle_points, aes(x = x, y = y), color = "black") +  # Okrąg
  geom_point(data = points, aes(x = x, y = y, color = chord_color), size = 1) +
  geom_polygon(data = triangle_points, aes(x = x, y = y), fill = NA, color = "blue", linetype = "dashed", size = 1) +  # Trójkąt
  scale_color_manual(values = c("green" = "green", "red" = "red")) +
  theme_minimal() +
  coord_equal() +
  labs(title = "Paradoks Bertranda - metoda 3", x = "X", y = "Y") +
  theme(legend.position = "none")
  
ggsave("C:/Users/rafal/OneDrive/Dokumente/GitHub/pakiety-statystyczne/metoda3.png")

p3 <- count_longer / n_chords
cat(sprintf("Prawdopodobieństwo: %.3f\n", p3))

