library(ggplot2)

# Parametry 
r <- 1  # Promień okręgu
n_chords <- 1000  # Liczba losowanych cięciw
a <- sqrt(3)  # Długość boku trójkąta równobocznego wpisanego w okrąg

# Ustalanie punktu początkowego P na obwodzie
alpha <- 2 * pi * runif(1)
x0 <- r * cos(alpha)
y0 <- r * sin(alpha)

beta <- abs(alpha - pi/2)

x_mid <- x0/2
y_mid <- y0/2
dist_mid <- sqrt((x_mid)^2 + (y_mid)^2)

count_longer <- 0

for (i in 1:n_chords) {
  m <- r * runif(1)
  x <- m * cos(alpha)
  y <- m * sin(alpha)
  dist <- sqrt((x)^2 + (y)^2)
  
  # Sprawdzenie długości cięciwy
  if (dist_mid < dist) {
    count_longer <- count_longer + 1
  }
}
p2 <- count_longer / n_chords
cat(sprintf("Prawdopodobieństwo: %.3f\n", p2))

