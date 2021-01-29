#install.packages("esquisse")
library(dplyr)
library(ggplot2)
library(psych)
library(corrplot)
library(caret)
library(NbClust)
library(cluster)
library(factoextra)
library(esquisse)

#O DATASET ORIGINAL SERA TRATADO COMO "BD" NO DECORRER DO CODIGO
BD <- read.table("C:/Users/Thiago Silva/Downloads/BASE_TITULOS.txt", header = TRUE, dec = ".", na.strings = "", stringsAsFactors = FALSE, sep="\t")
head(BD)

#TRANSFORMANDO TIPOS DE DADOS EM DATA E FATORES
str(BD)
BD$STATUS_LIQ_TITULO <- as.factor(BD$STATUS_LIQ_TITULO)
BD$DATA_PAGAMENTO <- as.Date(BD[,c("DATA_PAGAMENTO")])
BD$DATA_EMISSAO <- as.Date(BD[,c("DATA_EMISSAO")])
BD$VENCIMENTO <- as.Date(BD[,c("VENCIMENTO")])


#CRIANDO DATA FRAME COM AS REGIOES DO BRASIL E UNINDO-O COM A BASE ORIGINAL, AFIM DE SEGMENTAR VISOES POR REGIAO
ESTADO <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
REGIAO_EST_CEDENTE <- c("Norte","Nordeste","Norte","Norte","Nordeste","Nordeste","Centro-Oeste","Sudeste","Centro-Oeste","Nordeste","Centro-Oeste","Centro-Oeste","Sudeste","Norte","Nordeste","Sul","Nordeste","Nordeste","Sudeste","Nordeste","Sul","Norte","Norte","Sul","Sudeste","Nordeste","Norte")
bdregiao <- data.frame(ESTADO, REGIAO_EST_CEDENTE)
BD <- left_join(BD, bdregiao, by=c("ESTADO_CEDENTE"="ESTADO"))
head(BD)

#CALCULANDO PROPORCAO DE CEDENTES POR REGIOES DO BRASIL E PLOTANDO GRAFICO DE BARRAS
REG_ESTCED <- table(BD$REGIAO_EST_CEDENTE)
pct_REG_ESTCED <- prop.table(REG_ESTCED)*100
pct_REG_ESTCED <- round(pct_REG_ESTCED, digits = 1)
pct_REG_ESTCED
barplot(height = pct_REG_ESTCED, main = "Cedentes por regi?o", col= c("#353436"), ylab = "% de cedentes")

#Visualizando distribui??o de cedentes pelos estados do Brasil
ESTCED <- table(BD$ESTADO_CEDENTE)
pct_ESTCED <- prop.table(ESTCED)*100
pct_ESTCED <- round(pct_ESTCED, digits = 1)
pct_ESTCED
barplot(height = pct_ESTCED, main = "Cedentes por estado", col= c("#353436"), ylab = "% de cedentes")

#ENCONTRANDO CEDENTES COM MAIS LIMITE DE CREDITO E VISUALIZANDO-OS DE FORMA ORDENADA POR TABELA
cedentes_maislimite<- arrange(unique(BD[,c(4,6)]), desc(LIMITE_CEDENTE))
cedentes_maislimite

#EXPLORANDO PRINCIPAIS ESTATISTICAS DESCRITIVAS DA VARIAVEL DE TAXA MENSAL POR SEGMENTACOES DE CEDENTES, REGIAO E ESTADO
aggregate(TAXA_MENSAL ~ REGIAO_EST_CEDENTE, FUN= summary, data = BD)
aggregate(TAXA_MENSAL ~ ESTADO_CEDENTE, FUN= summary, data = BD)
aggregate(TAXA_MENSAL ~ CEDENTE, FUN= summary, data = BD)

#CALCULANDO RECEITA POR STATUS
receita_status <- arrange(aggregate(VALOR_REAL ~ STATUS_LIQ_TITULO, FUN= sum, data = BD), desc(VALOR_REAL))
receita_status
#PLOTANDO RECEITA POR STATUS 
ggplot(BD) +
  aes(x = STATUS_LIQ_TITULO, fill = STATUS_LIQ_TITULO, weight = VALOR_REAL) +
  geom_bar() +
  scale_fill_hue() +
  geom_text(aes(label=..count..), stat = "count",
            position = position_stack())+
  labs(x = "", y = "", title = "VALOR DO T?TULO POR STATUS", fill = "LEGENDA") +
  ylim("")+
  scale_y_continuous(limits = c(0,52000000))+
  theme_gray()

#CALCULANDO RECEITA POR FUNDO
receita_fundo <- arrange(aggregate(VALOR_REAL ~ NOME_FUNDO, FUN= sum, data = BD), desc(VALOR_REAL))
receita_fundo
#PLOTANDO RECEITA POR FUNDO
ggplot(BD) +
  aes(x = NOME_FUNDO, fill = STATUS_LIQ_TITULO, weight = VALOR_REAL) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "", y = "", title = "RECEITA POR FUNDO", fill = "LEGENDA") +
  theme_gray()

#CALCULANDO RECEITA POR PRODUTO
receita_produto <- arrange(aggregate(VALOR_REAL ~ TIPO_PRODUTO, FUN= sum, data = BD), desc(VALOR_REAL))
receita_produto
#PLOTANDO RECEITA POR PRODUTO
ggplot(BD) +
  aes(x = TIPO_PRODUTO, fill = STATUS_LIQ_TITULO, weight = VALOR_REAL) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "", y = "", title = "RECEITA POR PRODUTO", fill = "LEGENDA") +
  theme_gray()

#CALCULANDO RECEITA POR CEDENTE
BDreceita <- subset(BD, BD["STATUS_LIQ_TITULO"]=="EM_DIA")
receita_cedente <- arrange(aggregate(VALOR_REAL ~ CEDENTE, FUN= sum, data = BD), desc(VALOR_REAL))
receita_cedente
#PLOTANDO RECEITA POR CEDENTE
ggplot(BD) +
  aes(x = CEDENTE, fill = STATUS_LIQ_TITULO, weight = VALOR_REAL) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "", y = "", title = "RECEITA POR CEDENTE", fill = "LEGENDA") +
  theme_gray()

#CALCULANDO RECEITA POR REGIAO
receita_regiao <- arrange(aggregate(VALOR_REAL ~ REGIAO_EST_CEDENTE, FUN= sum, data = BD), desc(VALOR_REAL))
receita_regiao
#PLOTANDO RECEITA POR REGIAO
ggplot(BD) +
  aes(x = REGIAO_EST_CEDENTE, fill = STATUS_LIQ_TITULO, weight = VALOR_REAL) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "", y = "",  title = "RECEITA POR REGI?O", fill = "LEGENDA") +
  theme_gray()

#CALCULANDO RECEITA POR ESTADO
receita_estado <- arrange(aggregate(VALOR_REAL ~ ESTADO_CEDENTE, FUN= sum, data = BD), desc(VALOR_REAL))
receita_estado
#RECEITA POR ESTADO
ggplot(BD) +
  aes(x = ESTADO_CEDENTE, fill = STATUS_LIQ_TITULO, weight = VALOR_REAL) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "", y = "", title = "RECEITA POR ESTADO", fill = "LEGENDA") +
  scale_y_continuous(waiver(), limits = c(0, 22568196.80))+
  theme_gray() 


#----------------------------
#CRIANDO ALGORITMO NAO SUPERVISIONADO DE CLUSTERIZACAO PARA SEGMENTAR CEDENTES COM BASE EM PERFIS PARECIDOS

#CRIANDO NOVO DATA FRAME SEM AS COLUNAS DE RISCO
BDclusters <- BD %>% select(2:14,36,37)
head(BDclusters)
#CHECANDO PORCENTAGEM DE DADOS FALTANTES DE CADA VARIAVEL
NAs<- round(colSums(is.na(BDclusters))*100/nrow(BDclusters),2)
NAs
#FILTRANDO VARIAVEIS COM DADOS FALTANTES ACIMA DE 1%
NAs[NAs>1]
#DELETANDO LINHAS COM DADOS FALTANTES DEVIDO A PROPORCAO PEQUENA DE AUSENCIA DE DADOS
BDclusters<- na.omit(BDclusters)
#CHECANDO SE EXISTE ALGUM DADO FALTANTE NA BASE
anyNA(BDclusters)
#CHECANDO NUMERO DE LINHAS
nrow(BDclusters)
#ORDENANDO SEQUENCIA DE COLUNAS
BDclusters <- BDclusters %>% select(1:4,6:10,14,15,11:13,5)
head(BDclusters)

corrplot(cor(BDclusters[,c(12:15)]), method="color")

#UTILIZANDO METODO AUXILIAR GAP PARA IDENTIFICAR O NUMERO IDEAL DE CLUSTERS, UMA VEZ QUE PARA DETERMINAR O NUMERO DE CLUSTERS EXIGE QUE O ANALISTA TENHA MUITO CONHECIMENTO SOBRE O ASSUNTO E TENHA TOTAL CONHECIMENTO SOBRE A DISTRIBUICAO DE DADOS
#NORMALIZANDO DADOS E ARMAZENANDO NA VARIAVEL "DADOS"
dados<- scale(BDclusters[,c(12:15)])
#EXECUTANDO ALGORITMO KMEANS E METODO GAP_STAT PARA ENCONTRAR O NUMERO IDEAL DE CLUSTERS
fviz_nbclust(dados, kmeans, method= "gap_stat")

#REALIZANDO PREDICAO COM ALGORITMO DE CLUSTERIZACAO E UTILIZAMANDO METODO DE NORMALIZACAO "SCALE"    
BDclusters <- predict(preProcess(BDclusters, method ="scale") ,BDclusters)
#COMANDO PARA GARANTIR QUE O LEITOR CHEGUE AO MESMO RESULTADO
set.seed(1)
clusters<- kmeans(BDclusters[12:15], centers=2)

#CLUSTERS
clusters$cluster
#CENTROS DOS CLUSTERS
clusters$centers
#TAMANHO DOS CLUSTERS
clusters$size

#INSERINDO CLUSTERS NO DATASET E VISUALIZANDO A CLASSIFICACAO EM TABELA  
BDclusters$Cluster <- clusters$cluster
View(BDclusters)

#------------
#CRIANDO ALGORITMO SUPERVISIONADO KNN - AFIM DE SEGMENTAR DADOS NOVOS A PARTIR DA VARIAVEL STATUS "EM DIA" OU EM "ATRASO"

#CRIANDO NOVO DATA FRAME COM AS VARIAVEIS NUMERICAS E A VARIAVEL STATUS
BDknn <- BD[,c(6,12:36)]
head(BDknn)
#CHECANDO DADOS FALTANTES
NAs<- round(colSums(is.na(BDknn))*100/nrow(BDknn),2)
NAs
#FILTRANDO COLUNAS COM DADOS FALTANTES ACIMA DE 1%
NAs[NAs>1]
#REMOVENDO COLUNAS COM DADOS FALTANTES ACIMA DE 40%
BDknn <- BDknn[,-c(8,10,14,16)]
#REMOVENDO RESTANTE DAS LINHAS COM DADOS FALTANTES
BDknn <- na.omit(BDknn)
#CHECANDO SE HA DADOS FALTANTES
anyNA(BDknn)
#CHECANDO NUMERO DE LINHAS
nrow(BDknn)

#SEPARANDO DADOS EM TREINO E TESTE, ONDE OS DADOS DE TREINO SERAO ARMAZENADOS NA VARIAVEL "treino" COM 70% DOS DADOS DA BASE E O DADOS DE TESTE RECEBERAO OS OUTROS 30% E SERAO ARMAZENADOS NA VARIAVEL "teste"
filtro <- createDataPartition(y= BDknn$STATUS_LIQ_TITULO, p=0.7, list= FALSE)
treino<- BDknn[filtro,]
teste<- BDknn[-filtro,]

set.seed(1)
#UTILIZANDO A VARIAVEL "STATUS...- "EM DIA" OU "EM ATRASO" PARA TREINAR O MODELO COM O ALGORITMO KMEANS E REALIZANDO NORMALIZACAO COM METODO SCALE PARA QUE AS VARIAVEIS NAO TENHAM UMA IMPORTANCIA MAIOR DO QUE DE FATO TEM
modelo <- train(STATUS_LIQ_TITULO ~ ., data= treino, method="knn", preProcess="scale")

#ENCONTRO MEDIA DE PERFORMANCE DOS DIFERENTES VALORES DE "K" QUE O ALGORITMO TESTE
mean(modelo$results$Accuracy)
#ENCONTRANDO VALOR K DE MELHOR PERFORMANCE
modelo$bestTune$k
#CHECANDO ACUR?CIA DOS MODELOS
modelo$results

#FORMATANDO VARIAVEL STATUS PARA TIPO FATOR AFIM DE CRIAR A MATRIX DE CONFUSAO
teste$STATUS_LIQ_TITULO <- as.factor(teste$STATUS_LIQ_TITULO)
#AVALIANDO RESULTADO DO ALGORITMO NA BASE DE TESTE
prev<- predict(modelo, teste)
confusionMatrix(prev, teste$STATUS_LIQ_TITULO, dnn= c("previsto", "real"))
