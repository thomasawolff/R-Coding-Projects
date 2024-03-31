
rangeP <- seq(0, 1, length.out = 100)

plot(rangeP, dbinom(x = 8, prob = rangeP, size = 10),
     type = "l", xlab = "P(Black)", ylab = "Density")


lines(rangeP, dnorm(x = rangeP, mean = .5, sd = .1) / 15,
      col = "red")


lik <- dbinom(x = 8, prob = rangeP, size = 10)
prior <- dnorm(x = rangeP, mean = .5, sd = .1)
lines(rangeP, lik * prior, col = "green")


unstdPost <- lik * prior
stdPost <- unstdPost / sum(unstdPost)
lines(rangeP, stdPost, col = "blue")
legend("topleft", legend = c("Lik", "Prior", "Unstd Post", "Post"),
       text.col = 1:4, bty = "n")