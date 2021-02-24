# part 2
x <- rnorm(100, 50, 5)
hist(x)

#part 3
pnorm(50, 50, 5)

#part 4
x <- rnorm(100, 50, 10)
hist(x)

x <- rnorm(100, 50, 100)
hist(x)

## the spread (domain/range) grows with standard deviation; with sd = 5, the range was 35:65, sd = 10, the
## range was 30:75, with sd = 100, the range was -200:400. You can expect (approximately) a 3 times the standard
## deviation spread to either side (example with sd = 5, 3*5 = 15 --> lower bound is 50-15 = 35 and an upper
## bound of 50+15 = 65)

#part 5
x <- rnorm(100, 50, 5)
y <- dnorm(x, 50, 5)

#part 6
plot(x,y)

#part 7
z <- pnorm(x, 50, 5)
plot(x,z)

#part 8
abline(h=0.5, v=50)

## The point outline in the grid is (50, 0.5). Since this is a plot of the CDF, this corresponds to a cumulative
## probability density of 50% (or 0.5) to the left of x=50 on the PDF plot.



