#
#  Q1.9 from B&D
#
library("readr")
deaths <- read.table("deaths.txt")
names(deaths) <- c("Month", "Year", "Counts")
deaths <- data.frame(deaths, Index = 1:dim(deaths)[1])

# Q1.9 - (a) - plot
plot(deaths$Index, deaths$Counts, type = "b", xlab = "Index, Months", ylab = "Counts")

# Q1.9 - (b) - fit seasonality ($\hat{s}_t$)
fitA <- lm(Counts ~ as.factor(Month) - 1, data = deaths)
summary(fitA)
s_t <- predict(fitA)
lines(deaths$Index, s_t, col = "red")

# Q1.9 - (c) - plot residuals
deaths <- data.frame(deaths, Resid_after_st = deaths$Counts - s_t)
plot(deaths$Index, deaths$Resid_after_st, type = "l", xlab = "Index, Months",
     ylab = "Residual Counts")

# Q1.9 - (d) - fit $m_t$ as a quadratic
deaths <- data.frame(deaths, X = deaths$Index, X2 = deaths$Index^2)
fitB <- lm(Resid_after_st ~ X + X2, data = deaths)

plot(deaths$Index, deaths$Resid_after_st, type = "l", xlab = "Index, Months",
     ylab = "Residual Counts")
lines(deaths$Index, predict(fitB), col = "red")

# Q1.9 - (e) - plot residuals again
deaths <- data.frame(deaths, Resid_after_mt = deaths$Resid_after_st - predict(fitB))

plot(deaths$Index, deaths$Resid_after_mt, type = "b", xlab = "Index",
     ylab = "Residual Counts after s_t and m_t")

plot(deaths$Index, deaths$Counts, type = "b")
lines(deaths$Index, deaths$Counts - deaths$Resid_after_mt, col = "red")
lines(deaths$Index, deaths$Resid_after_mt + mean(deaths$Counts), col = "blue")

# Q1.9 - (f) - compute ACF
deaths <- data.frame(deaths, YtHat = deaths$Resid_after_mt)
fitC <- acf(deaths$YtHat, lag.max = 20)

# Q1.9 - (g) - predict X_t at t = 73, 74, ..., 84

# basic prediction: just use s_t and m_t - NOISE PREDICTION ZERO
extrap <- predict(fitA, newdata = data.frame(Index = 73:84, Month = 1:12), data = deaths) + 
          predict(fitB, newdata = data.frame(X = 73:84, X2 = (73:84)^2), data = deaths)

plot(c(deaths$Index, 73:84), c(deaths$Counts, rep(NA, 12)), type = "b", xlab = "Index, Months",
     ylab = "Original Data and Fits", main = "Prediction and Fit Plot")
lines(deaths$Index, deaths$Counts - deaths$YtHat, col = "red")
lines(73:84, extrap, col = "blue")
lines(73:84, predict(fitA, newdata = data.frame(Index = 73:84, Month = 1:12), data = deaths),
      col = "green")
lines(73:84, fitA$coefficients, col = "yellow")

plot(-10:100, predict(fitB, newdata = data.frame(X = -10:100, X2 = (-10:100)^2)), 
     type = "l", ylim = c(-1500, 2500))
lines(1:72, deaths$Counts - mean(deaths$Counts))

# advanced prediction: add ACF-governed noise. NOT YET, LATER CHAPTER.

# random notes
# seq(73, 84, 1)  ======   73:84
# for(i in seq(a, b, step))
# for(i in c(1, 2, 7, 8))
# for(i in 1:12)
# for(j in my_vector)

# more advanced: use the ACF as well