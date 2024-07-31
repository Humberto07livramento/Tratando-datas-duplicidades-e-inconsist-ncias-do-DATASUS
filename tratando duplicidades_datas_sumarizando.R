
# Carregando os pacotes ---------------------------------------------------

library(rio)        # pacote para importação de dados
library(tidyverse)  # pacote para manipulação de dados
library(lubridate)  # pacote para operação com datas
library(janitor)    # pacote para limpeza de dados

# Definindo a pasta de trabalho -------------------------------------------

setwd("C:\\opasR\\DADOS")

getwd()

# Importando a base de dados ----------------------------------------------

nindinet <- import("C:\\opasR\\DADOS\\NINDINET_MODULO4.xlsx")

# Inspecionando os tipos das variï¿½veis ------------------------------------

glimpse(nindinet)

nindinet |> select(starts_with("DT_")) |>  glimpse()

# Um pouco sobre formatos de data

lubridate::today()
lubridate::today() - 1
lubridate::today() + hours(72)

hoje <- "03/04/2024" # Criando um objeto 
hoje                 # Visualizando o objeto
class(hoje)          # FunçãO para ver o tipo do objeto

hoje <- dmy("03/04/2024")
class(hoje)
hoje

# Um pouco sobre períodos, durações e diferença

# Função tail() para ver as últimas observações do dataset

# Função as.period() representam unidades humanas como dias e semanas
tail(lubridate::as.period(nindinet$DT_NOTIFIC - nindinet$DT_SIN_PRI))

# Função as.duration() representa um número exato de segundos.
tail(lubridate::as.duration(nindinet$DT_NOTIFIC - nindinet$DT_SIN_PRI))

# Função as.difftime() representa a diferença em segundos
tail(lubridate::as.difftime(nindinet$DT_NOTIFIC - nindinet$DT_SIN_PRI))

# Ajustando os formatos de datas, com {lubridate} -------------------------
library(lubridate)
nindinet <- nindinet |>                          # dados e operador pipe
  mutate(DT_NOTIFIC = ymd(DT_NOTIFIC), # converte para Date
         DT_SIN_PRI = ymd(DT_SIN_PRI),
         DT_INVEST  = ymd(DT_INVEST),
         DT_ENCERRA = ymd(DT_ENCERRA),
         DT_DIGITA  = dmy(DT_DIGITA)   # Atenção ao formato dmy()
  ) 

nindinet |> select(starts_with("DT_")) |>  glimpse()

# Criando as variáveis dos cálculo de oportunidade -----------------------

# Definições:
# Oportunidade de notificação: Intervalo entre DT_SIN_PRI e DT_NOTIFIC
# Oportunidade de digitação: Intervalo entre DT_NOTIFIC e DT_DIGITA
# Oportunidade de encerramento: Intervalo entre DT_NOTIFIC e DT_ENCERRA

# Problema com a operação simples de subtração entre datas <drtn>
nindinet <- nindinet |>
  mutate(oportunidade_notificacao = DT_NOTIFIC - DT_SIN_PRI)
nindinet 

glimpse(nindinet$oportunidade_notificacao)

## Criando variáveis para as oportunidades, com a função mutate()

nindinet <- nindinet |>
  mutate(oportunidade_notificacao  = as.numeric(DT_NOTIFIC - DT_SIN_PRI),
         oportunidade_digitacao    = as.numeric(DT_DIGITA - DT_NOTIFIC),
         oportunidade_encerramento = as.numeric(DT_ENCERRA - DT_NOTIFIC))

## Inspecionando as variáveis criadas

nindinet |> select(starts_with("oportunidade")) |> glimpse()


# Avaliações das oportunidades -----------------------------------------

mean(nindinet$oportunidade_notificacao)   # Cálculo da média
median(nindinet$oportunidade_notificacao) # Cálculo da mediana

# Criando um resumo por ID_AGRAVO

resumo_oportunidade_digitacao <- nindinet |>
  group_by(ID_AGRAVO) |>
  summarise(media_digita = mean(oportunidade_digitacao),
            mediana_digita = median(oportunidade_digitacao))

resumo_oportunidade_digitacao

# Criando uma variável avaliando o encerramento das notificação

nindinet <- nindinet |>
  mutate(avaliacao_encerramento = if_else(oportunidade_encerramento <= 60,
                                          "Oportuno", "Inoportuno"))

# Criando um resumo por ID_AGRAVO e avaliacao_encerramento

resumo_encerramento <- nindinet |> 
  group_by(ID_AGRAVO, avaliacao_encerramento) |> 
  summarise(notificacoes = n())

print(resumo_encerramento)
print(resumo_oportunidade_digitacao)

# Análise de duplicidades -------------------------------------------------

# Definições:
# A duplicidade ocorre quando, no universo de todos os registros,
# um mesmo evento (com o mesmo indivíduo) foi notificado mais de uma vez.


# Buscando duplicidades com a função janitor::get_dupes()

# 5 (Cinco chaves de identificação)

duplicidades5 <- nindinet |> get_dupes(NM_PACIENT, DT_NASC,
                                       NM_MAE_PAC, ID_AGRAVO, DT_SIN_PRI)

# 4 (Quatro chaves de identificação)

duplicidades4 <- nindinet |> get_dupes(NM_PACIENT, DT_NASC,
                                       NM_MAE_PAC, ID_AGRAVO)

# 3 (Trinta chaves de identificação)

duplicidades3 <- nindinet |> get_dupes(NM_PACIENT, DT_NASC, NM_MAE_PAC)

duplicidades_agravo <- nindinet |> get_dupes(ID_AGRAVO, DT_NOTIFIC, DT_NASC)

# 2 (Duas chaves de identificação)

duplicidades2 <- nindinet |> get_dupes(NM_PACIENT, DT_NASC)



# Criar uma base de dados sem duplicidades, com a funï¿½ï¿½o distinct() ------


# Nova base somente com as variáveis que diferenciam os registros

sem_duplicidades <- nindinet |>
  distinct(NM_PACIENT, DT_NASC)

view(sem_duplicidades)

# Com todas as variáveis da base de dados

sem_duplicidades2 <- nindinet |>
  distinct(NM_PACIENT, DT_NASC, .keep_all = TRUE)
view(sem_duplicidades2)

# Análise de inconsistências ----------------------------------------------

# Definições
# Percentual de variáveis relacionadas que apresentam valores coerentes,
# não contraditidórios entre si

## Busca de inconsistências nas variáveis  CS_ESCOL_N  e NU_IDADE_N

## Dicionário de dados SINAN CS_ESCOL_N
## Preenchido automaticamente com a Categoria "10 - não se aplica"
## quando idade menor a 7 anos de idade.
## Quando caso notificado > 7 anos, campo não pode ser
## preenchido com categoria "10 - não se aplica".
# 

# Busca de casos menores de 10 anos com escolaridade superior a 5 anos

# Definindo o total de notificações

nrow(nindinet)


# Buscando a inconsistência e contabilizando

nindinet |>
  filter(NU_IDADE_N >= 4007, NU_IDADE_N <= 4009) |> 
  group_by(CS_ESCOL_N) |>
  summarise(casos = n())

# Filtrando os registros com inconsistência

nindinet |>
  filter(NU_IDADE_N >= 4007, NU_IDADE_N <= 4009) |> 
  filter(CS_ESCOL_N == "05" | CS_ESCOL_N == "06" | CS_ESCOL_N == "07") |>
  select(NU_NOTIFIC, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_ESCOL_N)

# Criando uma variável para a inconsistência

nindinet <- nindinet |>
  mutate(inc_escolaridade = case_when(NU_IDADE_N >= 4007
                                      & NU_IDADE_N <= 4009 &
                                        CS_ESCOL_N == "05" ~ "Inconsistência",
                                      NU_IDADE_N >= 4007
                                      & NU_IDADE_N <= 4009 &
                                        CS_ESCOL_N == "06" ~ "Inconsistência",
                                      NU_IDADE_N >= 4007
                                      & NU_IDADE_N <= 4009 &
                                        CS_ESCOL_N == "07" ~ "Inconsistência",
                                      NU_IDADE_N >= 4007
                                      & NU_IDADE_N <= 4009 &
                                        CS_ESCOL_N == "08" ~ "Inconsistência",
                                      CS_ESCOL_N == "09" ~ "Ignorado",
                                      is.na(CS_ESCOL_N) ~ "Ignorado",
                                      TRUE ~ "Consistente"))



# Análise de completitude -------------------------------------------------

## Avaliando a variável CS_RACA

## Avaliando apenas os registros vazios

nindinet |>                                 # base de dados
  summarise(                                # função de resumo
    completo = sum(!is.na(CS_RACA)),        # soma os diferentes(!) de vazio (NA)
    total_notificacoes = n(),               # conta todas as notificão
    missing = sum(is.na(CS_RACA)),          # soma os iguais a vazio (NA)
    taxa_completitude = (completo / total_notificacoes) * 100)

## Avaliando o preenchimento para cada categoria

nindinet |> 
  group_by(CS_RACA) |>
  summarise(notificacoes = n())


## Incorporando na avaliação os valores preenchidos como "9 - Ignorado"

nindinet |>
  summarise(
    completo = sum(!is.na(CS_RACA) & CS_RACA != "9"),
    total_notificacoes = n(),
    missing = sum(is.na(CS_RACA) | CS_RACA == "9"),
    taxa_completitude = (completo / total_notificacoes) * 100)

# Buscando valores vazios ou ignorados na variável ID_OCUPA_N

nindinet |>
  summarise(
    completo = sum(!is.na(ID_OCUPA_N) & ID_OCUPA_N != "9"),
    total_notificacoes = n(),
    missing = sum(is.na(ID_OCUPA_N) | ID_OCUPA_N == "9"),
    taxa_completitude = (completo / total_notificacoes) * 100)


