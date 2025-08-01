
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




## Gestão Hospitalar

### Introdução 

Neste projeto, utilizo o banco de dados Hospital Management Dataset, que contém cinco tabelas principais, utilizadas para responder às perguntas propostas. Abaixo, segue a descrição de cada uma:

- atients.csv - Dados demográficos, contatos, informações de registro e seguro dos pacientes;
- doctors.csv - Perfis dos médicos, especializações, experiência e contatos;
- appointments.csv - Datas, horários, motivos das visitas e status dos agendamentos;
- treatments.csv - Tipos de tratamento, descrições, datas e custos associados;
- billing.csv - Valores cobrados, formas de pagamento e status das transações.

### Analisando os dados

Carregando os pacotes necessários

Antes de iniciar a análise, carregamos os principais pacotes que darão suporte às etapas de leitura, transformação, visualização e modelagem:

```
library(writexl)
library(DescTools)
library(e1071)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(readr)
library(lubridate)
# Importanto os dados
# Os dados foram importados diretamente de um diretório local contendo os arquivos em CSV:
caminho <- "C:/Users/tulio/Documents/portfolio/gestao_hospitalar/archive (1)"

appointments <- read.csv(file.path(caminho, "appointments.csv"))
billing <- read.csv(file.path(caminho, "billing.csv"))
doctors <- read.csv(file.path(caminho, "doctors.csv"))
patients <- read.csv(file.path(caminho, "patients.csv"))
treatments <- read.csv(file.path(caminho, "treatments.csv"))
```

#### Verificação Estrutural dos Dados

Utilizamos str() e head() para inspecionar os primeiros registros e entender a estrutura de cada tabela. Essa etapa é essencial para planejar os tipos de análises possíveis e identificar colunas relevantes.

```
str(appointments)
head(appointments)

str(billing)
head(billing)

str(doctors)
head(doctors)

str(patients)
head(patients)

str(treatments)
head(treatments)
```

#### Conversão de Datas

As colunas relacionadas a datas e horários foram convertidas para os formatos adequados:

```
# Appointments
appointments$appointment_date <- as.Date(appointments$appointment_date)
appointments$appointment_time <- format(strptime(appointments$appointment_time, "%H:%M:%S"), "%H:%M:%S")

# Billing
billing$bill_date <- as.Date(billing$bill_date)

# Patients
patients$date_of_birth <- as.Date(patients$date_of_birth)
patients$registration_date <- as.Date(patients$registration_date)

# Treatments
treatments$treatment_date <- as.Date(treatments$treatment_date)
Estatísticas Descritivas de Faturamento
Calculamos métricas estatísticas básicas sobre os valores faturados:

billing_summary <- billing %>%
  summarise(
    total_faturado = sum(amount, na.rm = TRUE),
    media_faturamento = mean(amount, na.rm = TRUE),
    mediana_faturamento = median(amount, na.rm = TRUE),
    desvio_padrao = sd(amount, na.rm = TRUE),
    minimo = min(amount, na.rm = TRUE),
    maximo = max(amount, na.rm = TRUE),
    n = n()
  )
print(billing_summary)
```

Essas estatísticas oferecem um panorama geral do faturamento hospitalar, permitindo visualizar a média de cobrança por tratamento, além de desvios que podem indicar outliers. Nessa imagem, o faturamento apresenta alta dispersão, o que sugere a necessidade de analisar os fatores que influenciam o valor cobrado, como tipo de tratamento ou método de pagamento. Além disso, a diferença entre média e mediana indica que existem valores mais baixos puxando a média para baixo, o que pode influenciar a precificação ou as estratégias comerciais da clínica.

#### Faturamento por Tipo de Tratamento

Relacionamos os dados de faturamento com os tipos de tratamento para entender quais serviços geram maior receita:

```
billing_treatments <- billing %>%
  left_join(treatments, by = "treatment_id") %>%
  group_by(treatment_type) %>%
  summarise(
    media_valor = mean(amount, na.rm = TRUE),
    total_valor = sum(amount, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_valor))
print(billing_treatments)
```

Isso revela, por exemplo, quais tratamentos são mais lucrativos e quais têm maior volume. A análise dos tratamentos revela que o faturamento total está mais associado ao volume de atendimentos do que ao valor médio por consulta. A quimioterapia, embora não tenha o maior preço unitário, gerou a maior receita total devido ao maior número de sessões realizadas. Já a ressonância magnética apresentou o maior valor médio por consulta, mas o menor volume reduziu seu impacto no total faturado.

Os demais tratamentos, como ECG e raio-X, destacam-se pelos menores valores médios e também menor contribuição no faturamento total, reforçando a importância de entender tanto o preço quanto a frequência de utilização dos serviços para decisões estratégicas.

#### Consultas por Status

Aqui analisamos a distribuição das consultas segundo o status ("Completed", "Cancelled", "No-show"):

```
appointments_status <- appointments %>%
  count(status)
print(appointments_status)
```

Esse tipo de informação é crucial para medir absenteísmo e problemas operacionais.

#### Consultas por Especialidade Médica

Utilizamos left_join() para cruzar dados de consultas com a especialidade dos médicos:

```
appointments_by_specialty <- appointments %>%
  left_join(doctors, by = "doctor_id") %>%
  count(specialization, sort = TRUE)
print(appointments_by_specialty)
```

Com isso, conseguimos ver quais especialidades concentram maior número de atendimentos.

A visualização facilita a identificação de tratamentos com maior ticket médio:

```
ggplot(billing_treatments, aes(x = reorder(treatment_type, -media_valor), y = media_valor)) +
  geom_col(fill = "#1f78b4", width = 0.6) +  # barra um pouco mais fina
  labs(
    title = "Faturamento Médio por Tipo de Tratamento",
    x = "Tipo de Tratamento",
    y = "Valor Médio (R$)"
  ) +
  theme_minimal(base_size = 12) +  # tamanho menor do texto geral, incluindo título
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14)  # título um pouco menor que o base_size padrão
  ) +
  coord_flip()
```

#### Evolução de Consultas por Mês

```
ggplot(appointments_monthly, aes(x = mes, y = n)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue") +
  labs(
    title = "Número de Consultas por Mês",
    x = "Mês",
    y = "Quantidade de Consultas"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
```

O gráfico mostra como o volume de consultas evoluiu ao longo do tempo.

#### Faturamento Total e Médio por Mês

Analisamos o valor total e médio faturado a cada mês:

```
billing_monthly <- billing %>%
  mutate(mes = floor_date(bill_date, "month")) %>%
  group_by(mes) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")

ggplot(billing_monthly, aes(x = mes, y = total)) +
  geom_line(color = "#1f78b4", size = 1.2) +
  geom_point(color = "#1f78b4") +
  labs(
    title = "Faturamento Total por Mês",
    x = "Mês",
    y = "Valor Faturado (R$)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
billing_avg_monthly <- billing %>%
  mutate(mes = floor_date(bill_date, "month")) %>%
  group_by(mes) %>%
  summarise(media = mean(amount, na.rm = TRUE), .groups = "drop")

ggplot(billing_avg_monthly, aes(x = mes, y = media)) +
  geom_line(color = "#1f78b4", size = 1.2) +
  geom_point(color = "#1f78b4") +
  labs(
    title = "Faturamento Médio por Mês",
    x = "Mês",
    y = "Valor Médio (R$)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
```

Isso ajuda a avaliar sazonalidade e a saúde financeira do hospital ao longo do tempo.

#### Cancelamentos e Faltas

```
appointments_temporal_status <- appointments %>%
  mutate(mes = floor_date(appointment_date, "month")) %>%
  filter(status %in% c("Cancelled", "No-show")) %>%
  count(mes, status)

ggplot(appointments_temporal_status, aes(x = mes, y = n, color = status)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Evolução de Cancelamentos e Faltas",
    x = "Mês",
    y = "Quantidade de Ocorrências",
    color = "Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
```

A visualização mostra se cancelamentos e ausências estão aumentando, ajudando na tomada de decisões para mitigar essas ocorrências.

#### Regressão Linear

Analisamos se o tipo de tratamento e o método de pagamento influenciam no valor cobrado:

```
# quais variaveis influenciam o valor faturado 
modelo <- lm(amount ~ treatment_type + payment_method, data = billing %>%
               left_join(treatments, by = "treatment_id"))
summary(modelo)
```

A regressão mostra quais variáveis têm impacto significativo no faturamento. A regressão linear indicou que o tipo de tratamento ressonância magnética (MRI) está associado a um aumento estatisticamente significativo no valor faturado, em comparação a outros tratamentos. No entanto, o modelo como um todo não é significativo, e as variáveis utilizadas explicam menos de 4% da variação nos valores cobrados. Isso sugere que outras variáveis clínicas ou operacionais não incluídas no banco de dados têm maior influência sobre o custo final. Além disso, a forma de pagamento não afeta o valor faturado, o que pode indicar uma política de cobrança padronizada no hospital.

#### Análise de Variância (ANOVA)

Verificamos se o valor faturado difere significativamente entre os tipos de tratamento:

```
# o valor faturado muda significativamente entre diferentes tipos de tratamento?
anova_model <- aov(amount ~ treatment_type, data = billing %>%
                     left_join(treatments, by = "treatment_id"))
summary(anova_model)
```

Se o valor-p da ANOVA for menor que 0.05, concluímos que há diferenças significativas entre os grupos. Com base na ANOVA, não há diferença estatisticamente significativa no valor médio faturado entre os diferentes tipos de tratamento registrados nesse hospital. Isso pode indicar uma padronização nos custos praticados, ou que outras variáveis (como o tempo de tratamento, complexidade individual ou método de pagamento) têm influência maior sobre os valores cobrados.

#### Clustering de Pacientes
Usamos o algoritmo de k-means para agrupar pacientes com características semelhantes:
```
# agrupar pacientes com caracteristicas semelhantes como idade  

# Pré-processamento: idade, total de consultas, total gasto
pacientes_cluster <- billing %>%
  left_join(patients, by = "patient_id") %>%
  mutate(
    idade = as.integer((Sys.Date() - as.Date(date_of_birth)) / 365.25)
  ) %>%
  group_by(patient_id, idade) %>%
  summarise(
    consultas = n(),
    total_gasto = sum(amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  na.omit()

# Clustering com k-means (3 grupos)
set.seed(123)
kmeans_result <- kmeans(pacientes_cluster[, c("idade", "consultas", "total_gasto")], centers = 3)

pacientes_cluster$grupo <- as.factor(kmeans_result$cluster)

# Visualizar
ggplot(pacientes_cluster, aes(x = idade, y = total_gasto, color = grupo)) +
  geom_point(size = 3) +
  labs(title = "Clusters de Pacientes por Idade e Gasto Total") +
  theme_minimal()
```

A visualização mostra diferentes perfis de pacientes, por exemplo, idosos com alto gasto ou jovens com poucas consultas.

#### Tempo de Espera entre Cadastro e Primeira Consulta

```
# quanto tempo esperam entre o cadastro e a primeira consulta

tempo_espera <- appointments %>%
  left_join(patients, by = "patient_id") %>%
  group_by(patient_id) %>%
  summarise(
    registro = min(as.Date(registration_date)),
    primeira_consulta = min(appointment_date),
    dias_espera = as.numeric(primeira_consulta - registro)
  )

summary(tempo_espera$dias_espera)
```

Com essa análise, é possível identificar gargalos no atendimento inicial ao paciente, algo crucial para medir eficiência e satisfação. Esse resultado levanta um sinal de alerta: a demora para o primeiro atendimento é alta, o que pode impactar diretamente na qualidade do cuidado oferecido. Além disso, a presença de valores negativos indica que há dados inconsistentes no sistema, que precisam ser verificados ou limpos para evitar interpretações incorretas.

Esse tipo de análise é valioso para gestores hospitalares, pois permite identificar gargalos no processo de triagem e agendamento de consultas iniciais, além de orientar melhorias no fluxo de atendimento e na confiabilidade dos registros.

#### CONCLUSÃO

Ao explorar o Hospital Management Dataset com R, evidenciamos que o faturamento total é mais sensível ao volume de atendimentos do que ao preço unitário. A quimioterapia lidera a receita pelo número de sessões, enquanto a ressonância magnética (MRI) apresenta maior ticket médio sem dominar o faturamento por ter menor volume. Em paralelo, ECG e raio‑X contribuem menos por combinarem menor valor médio e menor demanda.

Do ponto de vista estatístico, a ANOVA não detectou diferenças significativas entre os tipos de tratamento no valor médio faturado, sugerindo padronização de preços ou influência de fatores não observados. A regressão indicou efeito positivo do MRI no valor, mas o modelo global não foi significativo (R² baixo), reforçando que variáveis clínicas e operacionais ausentes (complexidade do caso, tempo de permanência, materiais, convênio específico, urgência) provavelmente explicam a maior parte da variação.

Nas frentes operacionais, a análise temporal de cancelamentos e no‑shows e o tempo até a primeira consulta expuseram gargalos relevantes. A presença de tempos negativos entre cadastro e primeira consulta aponta inconsistências cadastrais que precisam ser corrigidas para evitar decisões enviesadas. O clustering de pacientes mostrou perfis distintos (ex.: idosos com alto gasto vs. jovens de baixa frequência), abrindo espaço para estratégias segmentadas de cuidado, agenda e comunicação.

Agradeço por acompanhar este projeto e espero que ele tenha demonstrado meu raciocínio analítico, assim como minha capacidade de aplicar R na solução de problemas reais de negócio.
Obrigado!


