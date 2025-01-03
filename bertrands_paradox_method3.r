library(ggplot2)
set.seed(9)

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

chords <- data.frame(x_start = numeric(), y_start = numeric(),
                     x_end = numeric(), y_end = numeric(), chord_color = character())

count_longer <- 0