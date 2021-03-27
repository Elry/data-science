# local regression / locally weighted polynomial regression

data(AirPassengers);
aps <- AirPassengers;

# multiplicative to additive
laps <- log(aps);
plot(laps, ylab="log AirPassengers");

fit <- stl(laps, s.window="period");
plot(fit);

fit$time.series;

# more decomposition
mothplot(aps);
