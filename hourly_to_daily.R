# Converte de horario para diário

# Arquivos de entrada e saido

INPUT_FILE = "./data/precipitation/dados_meterologicos_horario.csv"
#INPUT_FILE = "./data/precipitation/test_horario.csv"
OUTPUT_FILE = "./results/dados_meterologicos_diario.csv"
# Valor de NO_DATA para arquivo de entrada
NO_DATA <- -99
# valor de NO_DATA para SWAT - Arquivo de saido
SWAT_NO_DATA = -99

# Numero maximo de NA onde o valor eh ignorado na estatistica de agrupamento
# Acima disso resultado é NA
N_MAX_NA = 5


library("dplyr")

# Abre arquivo
print(paste("Lendo arquivo", INPUT_FILE))
data <- read.csv(INPUT_FILE, sep=";", quote="\"", dec="," ,
                 header = TRUE, skip = 2, fileEncoding="utf-8")


## Extrai dados das colunas
# Gera coluna com datetime
hourly <- data.frame(date = as.POSIXct(data[,c(1)], format="%d/%m/%Y %H:%M"))
# Extrai precipitacao
hourly$precip <- data[,c(2)]
# Extrai Temperatura
hourly$temp <- data[,c(3)]
# Extrai velocidade do vento
hourly$wind <- data[,c(4)]
# Extrai radiacao solar
hourly$radiance <- data[,c(5)]
# Extrai umidade
hourly$humid <- data[,c(6)]
# Extrai pressão atmosferica
hourly$pressure <- data[,c(7)]

# TODO: verificar se datas são sequenciais e corretas

# Substitui NO_DATA por NA
hourly[hourly==NO_DATA] <- NA


in_range <- function(df, index,  min_val, max_val, error_string) {
  e = which(between(df, min_val, max_val) == FALSE)
  if (any(e)) {
    print(error_string)
    print(data.frame(index = index[e], val = df[e]))
    #print(df[e])
    return(FALSE)
  }
  return(TRUE)
}

# Verifica se valores estão dentro da faixa esperada
# TODO: verificar qual é a faixa de valores para cada variavel
out_of_range = FALSE
out_of_range <- out_of_range | !in_range(hourly$precip, hourly$date, 0, 100, 
         "AVISO: valor anormal encontrado em precipitacao:")
out_of_range <- out_of_range | !in_range(hourly$wind, hourly$date, 0, 130,  
         "AVISO: valor anormal encontrado em vento:")
out_of_range <- out_of_range | !in_range(hourly$radiance, hourly$date, 0, 120,  
         "AVISO: valor anormal encontrado em radiancia:")
out_of_range <- out_of_range | !in_range(hourly$humid, hourly$date, 0, 100,  
         "AVISO: valor anormal encontrado em humidade:")
out_of_range <- out_of_range | !in_range(hourly$pressure, hourly$date, 0, 1100,  
         "AVISO: valor anormal encontrado em pressao:")
if (out_of_range) {
  print("AVISO: dados de entrada fora da faixa esperada. Verifique dados.")
}


# Converte de horario para diario
# TODO: verificar se resultados estao corretos. 
# TODO: checar funcionamento com NA. Simplesmente ignora??
print("Convertendo dados de horario para diario...")
daily = data.frame()

group <- list(date=as.POSIXct(trunc(hourly$date, "day")))

n_fun <- function(f, n) {
  function(x) {
    if (sum(is.na(x)) > n) {
      return(f(x, na.rm = FALSE))
    } else {
      return(f(x, na.rm = TRUE))
    }
  }
}



# precipitacao
daily <- aggregate(list(precip=hourly$precip), 
                   FUN=n_fun(sum, N_MAX_NA), by=group)

# temperatura max e min no dia
#daily$temp <- aggregate(list(temp=hourly$temp), FUN=mean, by=group)$temp

daily$temp_max <- aggregate(list(temp=hourly$temp), 
                            FUN=n_fun(max, N_MAX_NA), by=group)$temp

daily$temp_min <- aggregate(list(temp=hourly$temp), 
                            FUN=n_fun(min, N_MAX_NA), by=group)$temp


# vento media
daily$wind <- aggregate(list(wind=hourly$wind), FUN=n_fun(mean, N_MAX_NA), by=group)$wind


# radiancia soma
daily$radiance <- aggregate(list(radiance=hourly$radiance), 
                            FUN=n_fun(sum, N_MAX_NA), by=group)$radiance
# humidade média
daily$humid <- aggregate(list(humid=hourly$humid), 
                            FUN=n_fun(mean, N_MAX_NA), by=group)$humid
# pressao média
daily$pressure <- aggregate(list(pressure=hourly$pressure), 
                            FUN=n_fun(mean, N_MAX_NA), by=group)$pressure

# Substitui NA por NO_DATA do SWAT
daily[is.na(daily)] <- SWAT_NO_DATA

# Salva aquivo
# TODO: acertar formato correto para SWAT
print(paste("Salvando arquivo", OUTPUT_FILE))
write.csv(daily,OUTPUT_FILE, row.names = FALSE)

