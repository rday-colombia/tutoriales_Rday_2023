#----------------------------------Iniciales--------------------------------

##Cargar paquetes 
packages <- c("plyr","pROC", "questionr","GoodmanKruskal")  
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, require, character.only = TRUE)  # Load multiple packages

##cargar colores
colors <- c("chartreuse4", 554)

#--------------------cargar una base de datos------------------------------
data <- read.table(file.choose(), header = T)  #cargar dataset
options(scipen = 999) #quitar notación cientifica

##revisar la base de datos
head(data)

data[,2]      #variable age
summary(data[,2])
class(data[,2])

data[,3]      #variable job
summary(data[,3])
class(data[,3])

#---------------convertir varibles cualitativas para lectura en R----------

## Convertir manualmente una variable
job1 <- as.factor(data[,3]) 
class(job1)
levels(job1)
summary(job1)

## convertir varias variables en una base independiente
fac_cols <- sapply(data, is.character)                          
data[fac_cols] <- lapply(data[fac_cols], as.factor)

## Convertir variable cuantitativa en variable cualitativa I
data[,2]
summary(data[,2])
hist(data[,2])

age1 <- as.factor(ifelse(data[,2]<=25,'18-24',
                    ifelse(data[,2]<=32,'25-32',
                     ifelse(data[,2]<=39,'33-39', 
                      ifelse(data[,2]<=46,'40-46',
                        ifelse(data[,2]<=53,'47-53',
                          ifelse(data[,2]<=60,'54-60',
                            '60+')))))))

table(age1)

## Convertir variable cuantitativa en variable cualitativa II
age2 <- quant.cut(data[,2],10)
table(age2)

## otra variable (En este caso una variable Endogena)
riesgo <- as.factor(data[,1])
riesgo <- revalue(riesgo, c("0"="no_riesgo", "1"="si_riesgo"))
summary(riesgo)

#----------Analisis estadistico de Variables cualitativas-----------------

agetab <- table(riesgo,age1)
agepor <- round(prop.table(agetab, 2),2) #numero 2 muestra el resultado sobre las categorias
barplot(agepor, col = colors,  
        border="white", space=0.08, cex.axis=0.7,cex.names=0.8,
        ylim=c(0.5,1), xpd = FALSE) 

##Analisis de correlación entre variables cuantitativas
fac_cual <- data[sapply(data, is.factor)]
GKmatrix = GKtauDataframe(fac_cual)
plot(GKmatrix,diagSize = 0.7,cex=0.6)

fac_cual1 = cbind.data.frame(fac_cual,riesgo)
GKmatrix1 = GKtauDataframe(fac_cual1)
plot(GKmatrix1,diagSize = 0.7,cex=0.6)

#---------------------------modelos de discriminación----------------------

## partir la dataset en 80% 20%
set.seed(107)

split1 <- sample(c(rep(0, 0.8 * nrow(data)), rep(1, 0.2 * nrow(data))))
train <- data[split1 == 0, ]  
test <- data[split1== 1, ]    

## crear un modelo Logit o Probit
varlist <- names(data)
varlist <- varlist[!varlist %in% c("y","contact","duration","previous","poutcome")]
formula <- as.formula(paste("y",paste(varlist,collapse = "+"),sep="~"))

### Logit
model1 <- glm(formula, data = train, family = binomial(logit))
summary(model1)

### Probit
model2 <- glm(formula, data = train, family = binomial(probit))
summary(model2)


## Calculo de un evento de probabilidad
info <- c(v1 = 1, v2 = 35, v3 = 0, v4 = 0, v5 = 1, v6 = 0, v7 = 0, v8 = 0, 
            v9 = 0, v10 = 0, v11 = 0, v12 = 0, v13 = 0, v14 = 1, v15 = 0,
            v16 = 0, v17 = 0, v18 = 1, v19 = 5000, v20 = 1, v21 = 0)
minfo <- as.matrix(data.frame(info))

coeflogl <- as.matrix(summary(model1)$coefficients[,1])
coeflogp <- as.matrix(summary(model2)$coefficients[,1])

### identificar la probabilidad logit
zi <- crossprod(coeflogl, minfo)
pi <- 1/(1+exp(-zi))

### identificar la probabilidad probit
ii <- crossprod(coeflogp, minfo)
pip <- pnorm(ii, mean = 0, sd = 1, lower.tail = T) #CDF

#-------------------pruebas de bondad de ajuste----------------------------

predl <- predict(model1, test, type="response")

areal <- roc(test[,1],predl, percent = TRUE)
threshl <- coords(areal, "best", ret="threshold", transpose = TRUE,best.method="youden")
plot.roc(areal,print.thres=threshl,legacy.axes=TRUE,print.auc=TRUE)

probvl <- cut(predl, breaks=c(-Inf, threshl, Inf), labels=c("0", "1")) #yestimado
yvl <- cut(test[,1], breaks=c(-Inf, threshl, Inf), labels=c("0", "1")) #yreal
discrim <- table(yvl,probvl)
addmargins(discrim)

sens <- discrim[4]/(discrim[2]+discrim[4])
espc <- discrim[1]/(discrim[1]+discrim[3])
