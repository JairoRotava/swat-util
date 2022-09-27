# Converte de horario para diário

# Abre arquivo
x = "./Data/Precipitation/dados_meterologicos_horario.csv"
hourly <- read.csv(x, sep=";", quote="\"", dec="," ,
                 header = TRUE, skip = 2, fileEncoding="utf-8")


## Extrai dados das colunas
# Gera coluna com datetime
hourly$date <- as.POSIXct(hourly[,c(1)], format="%d/%m/%Y %H:%M")
# TODO: fazer verificação de data sequencia de faltando?
# Extrai precipitacao
hourly$precip <- hourly[,c(2)]
# Extrai Temperatura
hourly$temp <- hourly[,c(3)]
# Extrai velocidade do vento
hourly$wind <- hourly[,c(4)]
# Extrai radiacao solar
hourly$radiance <- hourly[,c(5)]
# Extrai umidade
hourly$humid <- hourly[,c(6)]
# Extrai pressão atmosferica
hourly$pressure <- hourly[,c(7)]


# TODO: Verificar por -99 ou valores fora da faixa. 
NO_VALUE <- -99
if (any(hourly$precip == NO_VALUE)) {
  print("AVISO: NO_VALUE encontrado em precipitacao")
}
if (any(hourly$temp == NO_VALUE)) {
  print("AVISO: NO_VALUE encontrado em temperatura")
}
if (any(hourly$wind == NO_VALUE)) {
  print("AVISO: NO_VALUE encontrado em velocidade de vento")
}
if (any(hourly$radiance == NO_VALUE)) {
  print("AVISO: NO_VALUE encontrado em radiancia")
}
if (any(hourly$humid == NO_VALUE)) {
  print("AVISO: NO_VALUE encontrado em humidade")
}
if (any(hourly$pressure == NO_VALUE)) {
  print("AVISO: NO_VALUE encontrado em pressao")
}


# Converte de horario para diario
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



# Salva aquivo
output_file = "./Data/output/dados_meterologicos_diario.csv"
write.csv(daily,output_file, row.names = FALSE)
# TODO: acertar formato correto para SWAT
