library(dfms)
library(xts)
library(ggplot2)
library(magrittr)
library(vars)

# Conocemos el número de variables y el número de observaciones
dim(BM14_M)

# Ahora de qué fecha a qué fecha son los datos
range(index(BM14_M))

# Vemos el nombre de algunas variables
head(colnames(BM14_M))

# Graficamos las series de tiempo
plot(scale(BM14_M), lwd = 1, main = "Series de Tiempo")

#dev.print(png, file = "bm.png", width = 1000, height = 800)

# Tomamos las series que son mensuales ya que hay series trimestrales
head(BM14_Models)
BM14_Models_M <- subset(BM14_Models, freq == "M")

# Aplicamos logaritmo y una diferenciación para hacer las series estacionarias
BM14_M[, BM14_Models_M$log_trans] %<>% log()
BM14_M_diff = diff(BM14_M)
plot(scale(BM14_M_diff), lwd = 1)


# Ahora lo primero que debemos hacer es determinar el número de factores
# Usaremos el criterio de Bai y Ng
ic = ICr(BM14_M_diff)
print(ic)
plot(ic)
dev.print(png, file = "nf.png", width = 600, height = 600)

# Por el criterio de Onatski
screeplot(ic)

# Concluimos que usaremos 7 factores


# Ahora para seleccionar el orden p del VAR(p)
vars::VARselect(ic$F_pca[, 1:4])

# Estimacion del modelo
model1 = DFM(BM14_M_diff, r = 7, p = 3)
print(model1)
plot(model1)

dfm_summary <- summary(model1)
print(dfm_summary)

# Graficamos residuales y valores ajustados
plot(resid(model1, orig.format = TRUE))
plot(fitted(model1, orig.format = TRUE))


# Podemos graficar los factores estimados por varios métodos
plot(model1, method = "all", type = "individual")
dev.print(png, file = "ts.png", width = 900, height = 400)



# Podemos observar los valores de los factores
head(as.data.frame(model1, time = index(BM14_M_diff)))

# Forecasting
# Predecimos 12 tiempos adelante
fc = predict(model1, h = 12)
print(fc)
plot(fc, xlim = c(320, 370))
dev.print(png, file = "forecast.png", width = 900, height = 600)
head(as.data.frame(fc, pivot = "wide"))





