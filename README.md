
## Brazilian Ecommerce


### Introdução
Neste projeto, utilizo o banco de dados Olist, que contém oito tabelas principais, algumas a serem usadas para responder às perguntas propostas. Abaixo, segue a descrição de cada uma:

- olist customers contém informações sobre os clientes e sua localização. Permite identificar clientes únicos no conjunto de pedidos e localizar os destinos das entregas. Cada pedido tem um customer ID exclusivo, mas o customer unique ID permite reconhecer clientes que fizeram múltiplas compras.
- olist_geolocation: reúne dados sobre códigos postais brasileiros e suas coordenadas de latitude e longitude. Utilizado para mapear locais e calcular distâncias entre vendedores e clientes.
- olist_order_items: contém dados dos itens comprados em cada pedido.
- olist_order_payments: inclui informações sobre as formas de pagamento usadas em cada pedido.
- olist_order_reviews: traz avaliações e comentários dos clientes, enviados após a entrega, para registrar a satisfação com a compra.
- olist_orders: é o conjunto de dados principal, onde cada pedido pode ser associado a todas as outras informações das tabelas relacionadas.
- olist_products: reúne dados sobre os produtos vendidos na plataforma.
- olist_sellers: contém informações sobre os vendedores que atenderam aos pedidos, incluindo sua localização e associação a cada produto vendido.

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

skewness(precos, na.rm = TRUE)
kurtosis(precos, na.rm = TRUE)
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

O uso de algoritmos como K-means e Random Forest mostrou-se eficaz para apoiar estratégias de marketing, retenção de clientes e melhoria na experiência do usuário. O destaque do tempo de entrega como principal fator de insatisfação, por exemplo, oferece um caminho direto para ações logísticas mais assertivas.

Essa abordagem evidencia não apenas domínio técnico em R e aprendizado atual em machine learning, mas também uma visão orientada a negócios, essencial para transformar dados em valor estratégico.

#### Limitações da análise

Apesar dos resultados positivos, este estudo apresenta algumas limitações importantes. Primeiramente, o modelo preditivo sofre com o desbalanceamento das classes, já que avaliações negativas representam uma parcela pequena do total. Isso afeta diretamente a sensibilidade do modelo para detectar clientes insatisfeitos. Além disso, variáveis potencialmente relevantes para a experiência do consumidor, como tipo de produto, desempenho do vendedor, ou atendimento ao cliente, não foram consideradas nesta versão da análise. Outra limitação é que os dados refletem apenas comportamentos passados e não capturam mudanças recentes no mercado ou preferências do consumidor. Por fim, os modelos foram avaliados apenas em dados históricos do próprio conjunto, sem validação externa ou acompanhamento temporal, o que pode limitar sua generalização.

#### Conclusão

A partir de dados do e-commerce Olist, consegui gerar insights relevantes sobre comportamento do consumidor, segmentar perfis de clientes e prever avaliações negativas com base em fatores logísticos e comerciais.
O uso de algoritmos como K-means e Random Forest mostrou-se eficaz para apoiar estratégias de marketing, retenção de clientes e melhoria na experiência do usuário. O destaque do tempo de entrega como principal fator de insatisfação, por exemplo, oferece um caminho direto para ações logísticas mais assertivas.
Essa abordagem evidencia não apenas domínio técnico em R e machine learning, mas também uma visão orientada a negócios, essencial para transformar dados em valor estratégico.
Agradeço por acompanhar este projeto e espero que ele tenha demonstrado meu raciocínio analítico, assim como minha capacidade de aplicar R na solução de problemas reais de negócio.
Obrigado!



## Delivery Center

### Introdução

Neste projeto, utilizo o banco de dados Delivery Center: Food & Goods orders in Brazil, que contém sete tabelas principais, utilizadas para responder às perguntas propostas. Abaixo, segue a descrição de cada uma:
- channels: contém informações sobre os canais de venda (marketplaces) utilizados pelos lojistas para vender alimentos (food) e produtos (goods).
- deliveries: reúne dados sobre as entregas realizadas por entregadores parceiros.
- drivers: apresenta informações sobre os entregadores parceiros, que atuam a partir dos hubs para entregar os pedidos aos consumidores.
- hubs: contém dados sobre os centros de distribuição (hubs) de onde partem as entregas.
- orders: reúne informações sobre as vendas processadas pela plataforma do Delivery Center.
- payments: apresenta dados sobre os pagamentos feitos ao Delivery Center.
- stores: traz informações sobre os lojistas que utilizam a plataforma para vender seus produtos nos marketplaces.

### Analisando os dados

Neste projeto, realizei uma análise exploratória utilizando a base de dados "Delivery Center: Food & Goods Orders in Brazil", que reúne informações sobre pedidos, entregadores, lojas, hubs logísticos, canais de venda e pagamentos. O objetivo foi compreender melhor o comportamento das entregas, os modais utilizados, os valores envolvidos nos pedidos e a prática de descontos por estado. Todo o processamento e análise foi feito em R, com auxílio de pacotes como dplyr, ggplot2, readr e writexl.

```
library(writexl)
library(DescTools)
library(e1071)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(readr)

caminho <- "C:/Users/tulio/Documents/portfolio/delivery_center/archive"

channels   <- read_csv(file.path(caminho, "channels.csv"))
deliveries <- read_csv(file.path(caminho, "deliveries.csv"))
drivers    <- read_csv(file.path(caminho, "drivers.csv"))
hubs       <- read_csv(file.path(caminho, "hubs.csv"))
orders     <- read_csv(file.path(caminho, "orders.csv"))
payments   <- read_csv(file.path(caminho, "payments.csv"))
stores     <- read_csv(file.path(caminho, "stores.csv"))
```

Inicialmente, carreguei os arquivos da base, que inclui tabelas com os entregadores (drivers), pedidos (orders), entregas (deliveries), pagamentos (payments), hubs logísticos (hubs) e lojas (stores). Em seguida, comecei a explorar os dados de entrega em conjunto com o tipo de entregador e o modal utilizado (moto ou bicicleta). Agrupei os dados por tipo e modal para calcular a quantidade total de entregas, a taxa de sucesso (considerando apenas entregas com status "DELIVERED") e a distância média percorrida.

```
entregas_stats <- deliveries %>%
  left_join(drivers, by = "driver_id") %>%
  group_by(driver_type, driver_modal) %>%
  summarise(
    total_entregas = n(),
    entregas_sucesso = sum(delivery_status == "DELIVERED", na.rm = TRUE),
    taxa_sucesso_percentual = round(100 * entregas_sucesso / total_entregas, 2),
    distancia_media_metros = round(mean(delivery_distance_meters, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(entregas_stats)
```

Essa análise revelou que os motoboys são responsáveis pelo maior volume de entregas, com distâncias médias superiores às dos bikers. Já os ciclistas (modal "BIKER") tendem a operar em faixas de curta distância, com menor variabilidade nas entregas. Foi possível observar ainda que entregas sem identificação de entregador são relativamente raras, o que reforça a confiabilidade dos cruzamentos feitos a seguir.

```
entregas_por_faixa <- deliveries %>%
  filter(!is.na(driver_id)) %>%
  left_join(drivers, by = "driver_id") %>%
  left_join(orders, by = c("delivery_order_id" = "order_id")) %>%
  mutate(
    faixa_distancia = case_when(
      delivery_distance_meters <= 1000 ~ "0-1 km",
      delivery_distance_meters <= 3000 ~ "1-3 km",
      delivery_distance_meters <= 5000 ~ "3-5 km",
      TRUE ~ ">5 km"
    )0
  ) %>%
  group_by(driver_modal, faixa_distancia) %>%
  summarise(
    qtd_entregas = n(),
    .groups = "drop"
  ) %>%
  arrange(driver_modal, faixa_distancia)

print(entregas_por_faixa)
```

Em uma segunda etapa, analisei o valor médio dos pedidos, cruzando os dados de entrega com os dados dos pedidos e criando faixas de distância (0–1 km, 1–3 km, 3–5 km e acima de 5 km). Essa segmentação mostrou que o valor dos pedidos cresce com a distância, especialmente quando o modal é motoboy. 

Avançando na análise, passei a investigar o comportamento dos pedidos em diferentes estados brasileiros, utilizando como referência o estado do hub logístico vinculado a cada pedido. O objetivo foi entender a distribuição do volume de pedidos, da receita bruta e líquida, e do desconto médio percentual aplicado. Para isso, filtrei apenas os pedidos com status "FINISHED" e pagamentos efetivados com status "PAID".

A análise revelou que o volume de pedidos varia significativamente entre os estados, com algumas unidades da federação concentrando grande parte da operação logística. 

```
ggplot(desconto_estado, aes(x = reorder(state, -total_pedidos), y = total_pedidos)) +
  geom_col(fill = "#1f78b4") +
  labs(
    title = "Total de Pedidos por Estado",
    x = "Estado",
    y = "Número de Pedidos"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
Em termos de descontos, alguns estados apresentaram valores médios superiores a 10%, o que pode indicar estratégias de precificação ou ambientes de maior competitividade. 
ggplot(desconto_estado, aes(x = reorder(state, -desconto_medio_percentual), y = desconto_medio_percentual, fill = desconto_medio_percentual)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Desconto Médio Percentual por Estado",
    x = "Estado",
    y = "Desconto Médio (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
```

Além disso, calculei estatísticas descritivas como média, mediana, desvio padrão, coeficiente de variação e quartis dos descontos médios por estado. Esses indicadores forneceram uma visão mais aprofundada da variabilidade nos descontos, reforçando que há regiões com políticas comerciais bastante distintas.

Por fim, utilizei uma análise de variância (ANOVA) para verificar se o valor dos pedidos é influenciado pelo tipo de entregador e pelo modal utilizado. Os resultados da ANOVA fatorial indicaram que tanto o tipo quanto o modal têm efeito significativo sobre o valor médio dos pedidos. 

```
anova_result <- aov(order_amount ~ driver_type * driver_modal, data = dados_completos)
summary(anova_result)

TukeyHSD(anova_result)
```

Uma análise post-hoc com o teste de Tukey confirmou que há diferenças estatísticas relevantes entre os grupos, sugerindo que, por exemplo, entregadores autônomos em motos podem operar em nichos diferentes dos entregadores contratados em bicicletas.


#### Limitações do Estudo

Como toda análise baseada em dados secundários, este estudo apresenta algumas limitações importantes. A primeira diz respeito à cobertura da base de dados: embora rica em informações, ela reflete apenas o universo operacional do Delivery Center durante um determinado período, podendo não capturar sazonalidades, mudanças estratégicas ou eventos externos que afetem o comportamento logístico.

Além disso, algumas variáveis possuem registros ausentes ou incompletos, como entregas sem identificação de entregador (driver_id ausente) ou pagamentos não conciliados. Embora essas observações tenham sido filtradas nas análises, sua presença pode indicar problemas de qualidade no registro dos dados.

Também vale destacar que as análises se limitaram a medidas descritivas e inferenciais simples. Modelos preditivos ou multivariados mais robustos poderiam ser explorados em estudos futuros para prever valores de pedidos, estimar tempos de entrega ou segmentar perfis de entregadores com base em desempenho.

#### Conclusão
A análise exploratória do banco de dados do Delivery Center permitiu identificar padrões relevantes no comportamento logístico e comercial da plataforma, como a predominância dos motoboys em distâncias maiores, variações no valor dos pedidos conforme modal e tipo de entregador, além de diferenças regionais em volume de vendas e aplicação de descontos.

Apesar das limitações da base, como registros ausentes e escopo temporal restrito, os resultados oferecem insights úteis para estratégias logísticas e comerciais. O estudo também abre caminho para análises mais robustas no futuro, com uso de modelos preditivos e segmentações mais refinadas.

Agradeço por acompanhar este projeto e espero que ele tenha demonstrado meu raciocínio analítico, assim como minha capacidade de aplicar R na solução de problemas reais de negócio.

Obrigado!
