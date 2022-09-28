# Converte de horario para diário

library("dplyr")

# Abre arquivo
x = "./Data/Precipitation/dados_meterologicos_horario.csv"
print(paste("Lendo arquivo", x))
#hourly <- read.csv(x, sep=";", quote="\"", dec="," ,
data <- read.csv(x, sep=";", quote="\"", dec="," ,
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

# Valor de NO_DATA - dado indisponivel.
NO_DATA <- -99
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
out_of_range <- out_of_range | !in_range(hourly$wind, hourly$date, 0, 100,  
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
# precipitacao
group <- list(date=as.POSIXct(trunc(hourly$date, "day")))
daily <- aggregate(list(precip=hourly$precip), FUN=sum, by=group)
# temperatura
daily$temp <- aggregate(list(temp=hourly$temp), FUN=mean, by=group)$temp
# vento
daily$wind <- aggregate(list(wind=hourly$wind), FUN=mean, by=group)$wind
# radiancia
daily$radiance <- aggregate(list(radiance=hourly$radiance), 
                            FUN=mean, by=group)$radiance
# humidade
daily$humid <- aggregate(list(humid=hourly$humid), 
                            FUN=mean, by=group)$humid
# pressao
daily$pressure <- aggregate(list(pressure=hourly$pressure), 
                            FUN=mean, by=group)$pressure

# Substitui NA por NO_DATA do SWAT
# TODO: qual é o valor correto de NO_DATA para o SWAT?
SWAT_NO_DATA = -999
daily[is.na(daily)] <- SWAT_NO_DATA

# Salva aquivo
# TODO: acertar formato correto para SWAT
output_file = "./Data/output/dados_meterologicos_diario.csv"
print(paste("Salvando arquivo", output_file))
write.csv(daily,output_file, row.names = FALSE)

