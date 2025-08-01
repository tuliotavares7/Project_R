## Brazilian Ecommerce


### Introdução
Neste projeto, utilizo o banco de dados Olist, que contém oito tabelas principais, algumas a serem usadas para responder às perguntas propostas. Abaixo, segue a descrição de cada uma:

olist customers contém informações sobre os clientes e sua localização. Permite identificar clientes únicos no conjunto de pedidos e localizar os destinos das entregas. Cada pedido tem um customer ID exclusivo, mas o customer unique ID permite reconhecer clientes que fizeram múltiplas compras.
olist_geolocation: reúne dados sobre códigos postais brasileiros e suas coordenadas de latitude e longitude. Utilizado para mapear locais e calcular distâncias entre vendedores e clientes.
olist_order_items: contém dados dos itens comprados em cada pedido.
olist_order_payments: inclui informações sobre as formas de pagamento usadas em cada pedido.
olist_order_reviews: traz avaliações e comentários dos clientes, enviados após a entrega, para registrar a satisfação com a compra.
olist_orders: é o conjunto de dados principal, onde cada pedido pode ser associado a todas as outras informações das tabelas relacionadas.
olist_products: reúne dados sobre os produtos vendidos na plataforma.
olist_sellers: contém informações sobre os vendedores que atenderam aos pedidos, incluindo sua localização e associação a cada produto vendido.

FOTO 

### Analisando os dados

Neste projeto, explorei o conjunto de dados do e-commerce Olist com o objetivo de realizar análises descritivas, segmentação de clientes e modelagem preditiva com foco na experiência do consumidor.
Inicialmente, carreguei os principais pacotes do R para manipulação, visualização e modelagem, como dplyr, ggplot2, randomForest, tidyverse e outros. 

```
library(writexl)
library(DescTools)
library(e1071)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(lubridate)
library(randomForest)
```

Importeis datasets do Olist foram utilizados, incluindo informações sobre pedidos, produtos, pagamentos, clientes, vendedores e avaliações. Para organização e reuso, exportei todas essas tabelas em um único arquivo Excel com múltiplas abas.

```
olist_customers_dataset <- read.csv("C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/olist_customers_dataset.csv")

olist_geolocation_dataset <- read.csv("C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/olist_geolocation_dataset.csv")


olist_order_items_dataset <- read.csv("C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/olist_order_items_dataset.csv")

olist_order_payments_dataset <- read.csv("C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/olist_order_payments_dataset.csv")

olist_order_reviews_dataset <- read.csv("C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/olist_order_reviews_dataset.csv")

olist_orders_dataset <- read.csv("C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/olist_orders_dataset.csv")

olist_products_dataset <- read.csv("C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/olist_products_dataset.csv")

olist_sellers_dataset <- read.csv("C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/olist_sellers_dataset.csv")

product_category_name_translation <- read.csv("C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/product_category_name_translation.csv")

write_xlsx(
  list(
    customers = olist_customers_dataset,
    geolocation = olist_geolocation_dataset,
    order_items = olist_order_items_dataset,
    payments = olist_order_payments_dataset,
    reviews = olist_order_reviews_dataset,
    orders = olist_orders_dataset,
    products = olist_products_dataset,
    sellers = olist_sellers_dataset,
    translation = product_category_name_translation
  ),
  path = "C:/Users/tulio/Documents/portfolio/Brazilian ecommerce/archive/olist_datasets_completo.xlsx"
)
```

A análise descritiva teve como foco o comportamento de compra dos clientes. Calculei o total gasto por cliente, identifiquei a categoria de produto mais vendida e analisei a distribuição dos preços. Os resultados indicaram forte assimetria positiva nos preços, com muitos itens de baixo valor e alguns poucos de alto valor. 

```
precos <- olist_order_items_dataset$price

sd(precos, na.rm = TRUE)
var(precos, na.rm = TRUE)
range(precos, na.rm = TRUE)
IQR(precos, na.rm = TRUE)
```

Também foi observado que o meio de pagamento mais utilizado foi o cartão de crédito, o que reforça o perfil digital dos consumidores.

```
table(olist_order_payments_dataset$payment_type)
prop.table(table(olist_order_payments_dataset$payment_type))
```

Para enriquecer a análise, construí visualizações como histogramas, boxplots, curvas de densidade e gráficos de barras. 

```
# histograma

ggplot(df, aes(x = price)) +
  geom_histogram(binwidth = 10, fill = "#A6CEE3", color = "white") +  
  labs(title = "Distribuição dos preços", x = "Preço", y = "Frequência") +
  coord_cartesian(xlim = c(0, 500)) +  # Limita o eixo X até 500
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# boxplot

top10 <- olist_products_dataset %>%
  count(product_category_name, sort = TRUE) %>%
  slice_head(n = 10)

olist_order_items_dataset %>%
  left_join(olist_products_dataset, by = "product_id") %>%
  filter(product_category_name %in% top10$product_category_name) %>%
  ggplot(aes(x = reorder(product_category_name, price), y = price)) +
  geom_boxplot(fill = "#A6CEE3", color = "black") +  # azul pastel + contorno preto
  coord_flip() +
  labs(
    title = "Boxplot de preços por categoria (Top 10)",
    x = "Categoria",
    y = "Preço"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# densidade

ggplot(olist_order_items_dataset, aes(x = price)) +
  geom_density(fill = "#4682B4", alpha = 0.6) +  
  labs(
    title = "Densidade dos preços dos produtos",
    x = "Preço",
    y = "Densidade"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# grafico de barras 

ggplot(olist_order_payments_dataset, aes(x = payment_type)) +
  geom_bar(fill = "#1f78b4") +  # azul mais forte
  labs(
    title = "Frequência dos tipos de pagamento",
    x = "Tipo de Pagamento",
    y = "Contagem"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
```

Um mapa interativo com o pacote leaflet permitiu observar a distribuição geográfica dos clientes, com maior concentração nas regiões Sudeste e Sul.

```
clientes_geo <- olist_geolocation_dataset %>%
  filter(!is.na(geolocation_lat), !is.na(geolocation_lng)) 

clientes_geo %>%
  slice_sample(n = 1000) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~geolocation_lng, lat = ~geolocation_lat, radius = 2,
                   color = "blue", opacity = 0.6, label = ~geolocation_zip_code_prefix)
```

A segmentação de clientes foi feita por meio do algoritmo K-means. Para isso, calculei a frequência de compras e o total gasto por cliente. Após padronização das variáveis, determinei o número ideal de clusters com o método do cotovelo (elbow method), que sugeriu três agrupamentos. Os clusters revelaram perfis distintos: consumidores de baixo gasto e pouca frequência, consumidores intermediários, e consumidores frequentes e de alto valor.

```
# clientes_gasto unico

clientes_gasto <- olist_order_payments_dataset %>%
  inner_join(olist_orders_dataset, by = "order_id") %>%
  inner_join(olist_customers_dataset, by = "customer_id") %>%
  group_by(customer_unique_id) %>%
  summarise(total_gasto = sum(payment_value, na.rm = TRUE), .groups = "drop")

# clientes_compras único

clientes_compras <- olist_orders_dataset %>%
  inner_join(olist_customers_dataset, by = "customer_id") %>%
  group_by(customer_unique_id) %>%
  summarise(qtd_compras = n(), .groups = "drop")

# juntar sem warnings

clientes_cluster <- clientes_gasto %>%
  left_join(clientes_compras, by = "customer_unique_id") %>%
  select(qtd_compras, total_gasto) %>%
  na.omit() %>%
  scale()

# clustering com k-means 

set.seed(123)
kmeans_result <- kmeans(clientes_cluster, centers = 3)

plot(clientes_cluster, col = kmeans_result$cluster, pch = 20)

# visualizar o perfil dos cluters 

kmeans_result$centers
```

A última etapa consistiu na construção de modelos preditivos com machine learning. O objetivo foi prever a probabilidade de um cliente fazer uma avaliação negativa (nota 1 ou 2). Para isso, foram usadas as variáveis: tempo de entrega, valor do frete e preço do produto. A modelagem foi realizada com o algoritmo Random Forest. Após preparação dos dados e transformação da variável resposta em fator, dividi os dados em treino e teste (70/30). O modelo apresentou uma acurácia de aproximadamente 88% no conjunto de teste. A matriz de confusão revelou bom desempenho para prever avaliações positivas, porém com menor capacidade de detectar avaliações negativas, reflexo de um desbalanceamento nas classes. A análise de importância das variáveis mostrou que o tempo de entrega foi o fator mais relevante na predição de avaliações negativas, seguido pelo valor do frete e pelo preço.

```
order_items_unico <- olist_order_items_dataset %>%
  distinct(order_id, .keep_all = TRUE)

reviews_unicos <- olist_order_reviews_dataset %>%
  distinct(order_id, .keep_all = TRUE)

dados_modelo <- reviews_unicos %>%
  inner_join(olist_orders_dataset, by = "order_id") %>%
  inner_join(order_items_unico, by = "order_id") %>%
  mutate(
    order_purchase_timestamp = ymd_hms(order_purchase_timestamp),
    order_delivered_customer_date = ymd_hms(order_delivered_customer_date),
    delivery_time = as.numeric(difftime(order_delivered_customer_date, order_purchase_timestamp, units = "days")),
    review_negativa = ifelse(review_score <= 2, 1, 0)
  ) %>%
  select(review_negativa, delivery_time, freight_value, price) %>%
  na.omit()

set.seed(123)

dados_modelo$review_negativa <- as.factor(dados_modelo$review_negativa)

modelo_class <- randomForest(
  review_negativa ~ delivery_time + freight_value + price,
  data = dados_modelo,
  ntree = 100,
  importance = TRUE
)

print(modelo_class)
importance(modelo_class, type = 2)
```

Este projeto demonstra a aplicabilidade de técnicas de análise de dados e machine learning no e-commerce, com foco em melhorar a experiência do cliente e apoiar decisões estratégicas como retenção de consumidores, logística e precificação.
Este projeto demonstra minha capacidade de aplicar técnicas de ciência de dados de ponta a ponta: desde o tratamento e análise exploratória até a modelagem preditiva, sempre com foco em resolver problemas reais de negócio. A partir de dados do e-commerce Olist, consegui gerar insights relevantes sobre comportamento do consumidor, segmentar perfis de clientes e prever avaliações negativas com base em fatores logísticos e comerciais.
O uso de algoritmos como K-means e Random Forest mostrou-se eficaz para apoiar estratégias de marketing, retenção de clientes e melhoria na experiência do usuário. O destaque do tempo de entrega como principal fator de insatisfação, por exemplo, oferece um caminho direto para ações logísticas mais assertivas.
Essa abordagem evidencia não apenas domínio técnico em R e aprendizado atual em machine learning, mas também uma visão orientada a negócios, essencial para transformar dados em valor estratégico.
Limitações da análise
Apesar dos resultados positivos, este estudo apresenta algumas limitações importantes. Primeiramente, o modelo preditivo sofre com o desbalanceamento das classes, já que avaliações negativas representam uma parcela pequena do total. Isso afeta diretamente a sensibilidade do modelo para detectar clientes insatisfeitos. Além disso, variáveis potencialmente relevantes para a experiência do consumidor, como tipo de produto, desempenho do vendedor, ou atendimento ao cliente, não foram consideradas nesta versão da análise. Outra limitação é que os dados refletem apenas comportamentos passados e não capturam mudanças recentes no mercado ou preferências do consumidor. Por fim, os modelos foram avaliados apenas em dados históricos do próprio conjunto, sem validação externa ou acompanhamento temporal, o que pode limitar sua generalização.
Conclusão
A partir de dados do e-commerce Olist, consegui gerar insights relevantes sobre comportamento do consumidor, segmentar perfis de clientes e prever avaliações negativas com base em fatores logísticos e comerciais.
O uso de algoritmos como K-means e Random Forest mostrou-se eficaz para apoiar estratégias de marketing, retenção de clientes e melhoria na experiência do usuário. O destaque do tempo de entrega como principal fator de insatisfação, por exemplo, oferece um caminho direto para ações logísticas mais assertivas.
Essa abordagem evidencia não apenas domínio técnico em R e machine learning, mas também uma visão orientada a negócios, essencial para transformar dados em valor estratégico.
Agradeço por acompanhar este projeto e espero que ele tenha demonstrado meu raciocínio analítico, assim como minha capacidade de aplicar R na solução de problemas reais de negócio.
Obrigado!

skewness(precos, na.rm = TRUE)
kurtosis(precos, na.rm = TRUE)
