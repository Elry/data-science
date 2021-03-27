install.packages("forecast", repo = "https://lib.ugent.be/CRAN/");
library(forecast);

set.seed(123456);

ylim <- c(min(Nile), max(Nile));
plot(Nile);

plot(ma(Nile, 3), ylim=ylim);
plot(ma(Nile, 8), ylim=ylim);
