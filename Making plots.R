#Making Plots#
time <- read.table("D:/Users/Contingencia/Escritorio/Positiva 2020/Exploratory data analysis/household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
## Formato de fechas
time$Date <- as.Date(time$Date, "%d/%m/%Y")

## Filtrar la base para la fechas Feb. 1, 2007 to Feb. 2, 2007
time <- subset(time,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Remover datos incompletos
time <- time[complete.cases(time),]

## Combinar columnas de fexhas con la de horas
dateTime <- paste(time$Date, time$Time)

## Nombrar
dateTime <- setNames(dateTime, "DateTime")

## Remover la columna fecha y tiempo
time <- time[ ,!(names(time) %in% c("Date","Time"))]

## Agregar la columna dateTime
time <- cbind(dateTime, time)

## Format dateTime Column
time$dateTime <- as.POSIXct(dateTime)
#Plot1 
hist(time$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
#Plot2
plot(time$Global_active_power~time$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
#Plot 3
with(time, {
  plot(Sub_metering_1 ~ dateTime, type = "l", 
       ylab = "Global Active Power (kilowatts)", xlab = "")
  lines(Sub_metering_2 ~ dateTime, col = 'Red')
  lines(Sub_metering_3 ~ dateTime, col = 'Blue')
})
legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
#Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(time, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})


