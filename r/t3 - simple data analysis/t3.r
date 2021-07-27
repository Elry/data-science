# libraries
library(dplyr);
library(ggplot2);

# setting data
data(diamonds);
dds <- diamonds;

# 1. Qual é a estrutura do conjunto de dados “diamantes”?
str(dds);

# 2. Explore a parte inicial e a final do conjunto de dados.
head(dds);
tail(dds);

# 3. Faça alguns sumários estatísticos para entender melhor a base de dados.
summary(dds);

# 4. A saída da função summary() está de acordo com a descrição mostrada anteriormente?
print('Sim, todos os dados estão de acordo com a função summary() e as anteriores')

# 5. Explore a variável price, seguindo o modelo de exploração.
head(dds$price);
tail(dds$price);
mean(dds$price);
summary(dds$price);

# 6. Veja a distribuição da variável (histograma); observe a faixa de valores da variável e também.
priceCount <- dds %>% count(price);
priceCountGraph <- ggplot(dds, aes(price)) +
  geom_histogram(color="grey", fill="green", alpha=.5, bins=100L) +
  labs(title="Price by amount", subtitles="Price Analysis", x="Price", y="Amount") +
  geom_vline(aes(xintercept=mean(price)), color="blue", linetype="dashed", size=1);

# 7. Explore também as variáveis carat, cut, color, clarity, x, y, z, depth e table, seguindo o modelo de exploração.
# carat
head(dds$carat);
tail(dds$carat);
mean(dds$carat);
summary(dds$carat);
caratHist <- ggplot(dds, aes(carat)) +
  geom_histogram(color="black", fill="grey", alpha=.5) +  
  geom_vline(aes(xintercept=mean(carat)), color="blue", linetype="dashed", size=1) +
  labs(title="Carat Amount", subtitles="Descritive Analysis", x="Carat", y="Amount");

caratHist;

# cut
head(dds$cut);
tail(dds$cut);
summary(dds$cut);
cutCount <- dds %>% count(cut);
cutCountGraph <- ggplot(dds, aes(x=cut)) +
  coord_flip() +  
  stat_count(width=1, colour="green", geom="bar") +
  labs(title="Cut count", subtitles="Descritive Analysis", x="Cut", y="Amount") +
  geom_text(aes(y=..count.., label=..count..), stat="count", color="white", hjust=1.0, size=4);

cutCount;
cutCountGraph;

# color
head(dds$color);
tail(dds$color);
summary(dds$color);
colorCount <- dds %>% count(color);
colorCountGraph <- ggplot(dds, aes(x=color)) +
  coord_flip() +
  stat_count(width=1, colour="green", geom="bar") +
  labs(title="Color count", subtitles="Descritive Analysis", x="Cut", y="Amount") +
  geom_text(aes(y=..count.., label=..count..), stat="count", color="white", hjust=1.0, size=4);

colorCount;
colorCountGraph;

# clarity
head(dds$clarity);
tail(dds$clarity);
summary(dds$clarity);
clarityCount <- dds %>% count(clarity);
clarityCountGraph <- ggplot(dds, aes(clarity)) +
  coord_flip() +
  stat_count(width=1, colour="green", geom="bar") +
  labs(title="Clarity count", subtitles="Descritive Analysis", x="Cut", y="Amount") +  
  geom_text(aes(y=..count.., label=..count..), stat="count", color="white", hjust=1.0, size=4);

clarityCount;
clarityCountGraph;

# x
head(dds$x);
tail(dds$x);
mean(dds$x);
summary(dds$x);
xGraph <- ggplot(dds, aes(x)) +
  geom_histogram(color="black", fill="blue", alpha=.5) +
  labs(title="x Amount", subtitles="Descritive Analysis", x="x", y="Amount") +
  geom_vline(aes(xintercept=mean(x)), color="red", linetype="dashed", size=1);

xGraph;

# y
head(dds$y);
tail(dds$y);
mean(dds$y);
summary(dds$y);
yGraph <- ggplot(dds, aes(y)) +
  geom_histogram(color="black", fill="blue", alpha=.5) +
  labs(title="Y Amount", subtitles="Descritive Analysis", x="Y", y="Amount") +
  geom_vline(aes(xintercept=mean(y)), color="blue", linetype="dashed", size=1);

yGraph;

# z
head(dds$z);
tail(dds$z);
mean(dds$z);
summary(dds$z);
zGraph <- ggplot(dds, aes(z)) +
  geom_histogram(color="black", fill="blue", alpha=.5) +
  labs(title="Z Amount", subtitles="Descritive Analysis", x="Z", y="Amount") +
  geom_vline(aes(xintercept=mean(z)), color="blue", linetype="dashed", size=1);

zGraph;

# depth
head(dds$depth);
tail(dds$depth);
mean(dds$depth);
summary(dds$depth);
depthCount <- dds %>% count(depth);
depthHisto <- ggplot(dds, aes(depth)) +
  geom_histogram(color="black", fill="blue", alpha=.5) +
  geom_vline(aes(xintercept=mean(depth)), color="blue", linetype="dashed", size=1) +
  labs(title="Depth Amount", subtitles="Descritive Analysis", x="Depth", y="Amount");
depthCountGraph <- ggplot(dds, aes(depth)) +
  stat_count(width=1, colour="green", geom="bar") +
  labs(title="Depth count", subtitles="Descritive Analysis", x="Depth", y="Amount") +
  geom_text(aes(y=..count.., label=..count..), stat="count", color="white", hjust=1.0, size=1);

depthCount;
depthHisto;
depthCountGraph;

# table
head(dds$table);
tail(dds$table);
mean(dds$table);
summary(dds$table);
tableGraph <- ggplot(dds, aes(table)) +
  geom_histogram(color="black", fill="blue", alpha=.5) +  
  geom_vline(aes(xintercept=mean(table)), color="blue", linetype="dashed", size=1) +
  labs(title="Table Amount", subtitles="Descritive Analysis", x="Table", y="Amount");

tableGraph;

# 8. Crie boxplots para as variáveis numéricas; veja se existem dados anormais (outliers).
# x boxplot + outliers
ggplot(dds, aes(x)) +
  coord_flip() +
  scale_color_brewer(palette="Dark2") +
  labs(title="Numeric Analysis", subtitles="X", x="X", y="") +
  geom_boxplot(outlier.colour="blue", outlier.size=2, fill='grey', color='black')

# y boxplot + outliers
ggplot(dds, aes(y)) +
  coord_flip() +
  scale_color_brewer(palette="Dark2") +
  labs(title="Numeric Analysis", subtitles="Y", x="Y", y="") +
  geom_boxplot(outlier.colour="blue", outlier.size=2, fill='grey', color='black')

# z boxplot + outliers  
ggplot(dds, aes(z)) +
  coord_flip() +
  scale_color_brewer(palette="Dark2") +
  labs(title="Numeric Analysis", subtitles="Z", x="Z", y="") +
  geom_boxplot(outlier.colour="blue", outlier.size=2, fill='grey', color='black')

# carat boxplot + outliers
ggplot(dds, aes(carat)) +
  coord_flip() +
  scale_color_brewer(palette="Dark2") +
  labs(title="Numeric Analysis", subtitles="Carat", x="Carat", y="") +
  geom_boxplot(outlier.colour="blue", outlier.size=2, fill='grey', color='black')

# price boxplot + outliers
ggplot(dds, aes(price)) +
  coord_flip() +
  scale_color_brewer(palette="Dark2") +
  labs(title="Numeric Analysis", subtitles="Price", x="Price", y="") +
  geom_boxplot(outlier.colour="blue", outlier.size=2, fill='grey', color='black')

# depth boxplot + outliers  
ggplot(dds, aes(depth)) +
  coord_flip() +
  scale_color_brewer(palette="Dark2") +
  labs(title="Numeric Analysis", subtitles="Depth", x="Depth", y="") +
  geom_boxplot(outlier.colour="blue", outlier.size=2, fill='grey', color='black')

# table boxplot + outliers
ggplot(dds, aes(table)) +
  coord_flip() +
  scale_color_brewer(palette="Dark2") +
  labs(title="Numeric Analysis", subtitles="Table", x="Table", y="") +
  geom_boxplot(outlier.colour="blue", outlier.size=2, fill='grey', color='black')

# 9. Utilize as variáveis categóricas para fazer o facetamento dos dados, mostrando alguns gráficos com 2 ou mais variáveis contínuas lado a lado
# Cut x Depth x Color
cutDepthColor <- ggplot(dds, aes(cut, depth, fill=color)) +
  theme_minimal() +
  facet_wrap(~ color, scale="free") +
  geom_boxplot(color="grey", notch=TRUE) +
  stat_summary(fun="mean", geom="point", shape=1, size=2, color="black") +
  labs(title="Categorical analysis", subtitles="Cut x Depth x Color", x="Cut", y="Depth")

# Cut x Depth x Clarity
cutDepthClarity <- ggplot(dds, aes(cut, depth, fill=clarity)) +
  theme_minimal() +
  geom_boxplot(color="grey") +
  facet_wrap(~ clarity, scale="free") +
  stat_summary(fun="mean", geom="point", shape=1, size=2, color="black") +
  labs(title="Categorical analysis", subtitles="Cut x Depth x Clarity", x="Cut", y="Depth")

# Clarity x Color
clarityColor <- boxplot(clarity ~ color, data=dds, xlab='Color', ylab='Clarity', main='Clarity by color', col='yellow');

# Price x Cut x Clarity
priceCutClarity <- ggplot(dds, aes(cut, price, fill=clarity)) +
  theme_minimal() +
  facet_wrap(~ clarity, scale="free") +
  geom_boxplot(color="black", notch=TRUE) +
  stat_summary(fun="mean", geom="point", shape=1, size=2, color="red") + 
  labs(title="Price Analysis", subtitles="Price x Cut x Clarity", x="Cut", y="Price")

# Price x Cut x Color
priceCutColor <- ggplot(dds, aes(cut, price, fill=color)) +
  theme_minimal() +  
  facet_wrap(~ color, scale="free") +
  geom_boxplot(color="black", notch=TRUE) +
  stat_summary(fun="mean", geom="point", shape=1, size=2, color="white") +
  labs(title="Price Analysis", subtitles="Price x Cut x Color", x="Cut", y="Price")

# Price x Color x Clarity
priceColor <- ggplot(dds, aes(color, price, fill=clarity)) +
  theme_minimal() +  
  facet_wrap(~ clarity, scale="free") +
  geom_boxplot(color="black", notch=TRUE) +
  stat_summary(fun="mean", geom="point", shape=1, size=2, color="white") +
  labs(title="Price Analysis", subtitles="Price x Color x Clarity", x="Color", y="Price")