library("forecast");

data("AirPassengers");
aps <- AirPassengers;

class(aps);
start(aps);
end(aps);
frequency(aps);
summary(aps);
plot(aps);
abline(reg=lm(aps ~ time(aps)));
text(1948.7, -65, "", xpd=NA, cex=0.8);
cycle(aps);

# new frequency with mean by year
aggregate(aps, FUN=mean);

# new frequency with sum by year
aggregate(aps, FUN=sum);

# mean by month
aggregate(aps ~ cycle(aps), FUN=mean);

# median by month
aggregate(aps ~ cycle(aps), FUN=median);

# variance by month
aggregate(aps ~ cycle(aps), FUN=var);


plot(aggregate(aps ~ cycle(aps), FUN=var), type="o", lty=2, ylab="Passengers variance");


boxplot(aps ~ cycle(aps));
