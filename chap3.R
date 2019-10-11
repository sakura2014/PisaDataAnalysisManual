# table 3.2

x <- seq(160, 178, 2)
w <- rep(10, 10)

# p.50
sd(x)
sqrt(sum(w * (x - weighted.mean(x))^2) / length(x)) # (i)
sqrt(sum(w * (x - weighted.mean(x))^2) / (length(x) - 1)) # (ii)
sqrt(sum(w * (x - weighted.mean(x))^2) / sum(w)) # (iii)
sqrt(sum(w * (x - weighted.mean(x))^2) / (sum(w) - 1)) # (iv)
