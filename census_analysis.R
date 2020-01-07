## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----load_libraries, include=FALSE---------------------------------------
library(knitr)
library(ggplot2)
library(VIM)

## ----read_csv------------------------------------------------------------
# Carreguem l'arxiu de training i de test.
datatrain <- read.csv("adult-training.csv", header = FALSE, stringsAsFactors = FALSE)
datatest <- read.csv("adult-test.csv", skip = 1, header=FALSE, stringsAsFactors = FALSE)

data_colnames <- c(
  "age",
  "workclass",
  "fnlwgt",
  "education",
  "education-num",
  "marital-status",
  "occupation",
  "relationship",
  "race",
  "sex",
  "capital-gain",
  "capital-loss",
  "hour-per-week",
  "native-country",
  "income"
)

# Assignem els noms a les columnes pertinents.
names(datatrain) <- data_colnames
names(datatest) <- data_colnames

# Ajuntem els dos dataframes.
df <- rbind(datatrain, datatest)

## ----structure-----------------------------------------------------------
str(df)

## ----removeWS------------------------------------------------------------
# Obtenim quines columnes són de tipus character
truth <- sapply(df,is.character)

# Per a les columnes de tipus charcaters, els hi apliquem la funcio trimws
# i el resultat l'ajuntem (cbind) amb les columnes que no són de tipus character
df <- data.frame(
  cbind(
    sapply(
      df[,truth],
      trimws,
      which="both"
    ),
    df[,!truth]
  )
)

str(df)

## ----summary-------------------------------------------------------------
summary(df)

## ----removevariables-----------------------------------------------------
df$fnlwgt <- NULL
df$education <- NULL

## ----factorLevel---------------------------------------------------------
factors <- sapply(df, is.factor)
lapply(df[, factors], levels)

## ----correccioLevel------------------------------------------------------
levels(df$income)[levels(df$income)=="<=50K."] <- "<=50K"
levels(df$income)[levels(df$income)==">50K."] <- ">50K"
levels(df$income)

## ----capital-------------------------------------------------------------
df["capital"] = df$capital.gain - df$capital.loss
df$capital.gain <- NULL
df$capital.loss <- NULL

## ----buits---------------------------------------------------------------
print("Casos amb NA")
colSums(is.na(df))

print("Casos amb cadenes de text buides")
colSums(df == "")

## ----valorsinterrogant---------------------------------------------------
colSums(df == "?")

## ----interrogantToNA-----------------------------------------------------
df[df == "?"] <- NA

## ----kNNimputation-------------------------------------------------------
df <- kNN(data=df, variable = c("workclass", "occupation", "native.country"), impNA=TRUE, imp_var=FALSE)

## ----buitscheck----------------------------------------------------------
colSums(is.na(df))

## ----sentinelas----------------------------------------------------------
attributs_quantitatius = c('age', 'education.num', 'capital', 'hour.per.week')
for (columnName in attributs_quantitatius){
  # Generem una graella amb 1 fila i 2 columnes
  par(
    mfrow=c(1,2),
    oma = c(0, 0, 2, 0)
  )
  # Mostrem el histograma
  hist(
    df[,columnName],
    main="Histogram",
    xlab=paste(columnName, " values")
  )
  # Mostrem el boxplot
  boxplot(
    df[,columnName],
    main="Boxplot"
  )
  # Posem titol al conjunt de grafiques.
  mtext(
    paste("Analysis for ",columnName),
    outer = TRUE,
    cex = 1.5
  )
}

## ----ageoutlier----------------------------------------------------------
df$age[which(abs(scale(df$age))>3)] <- mean(df$age, na.rm=TRUE)

## ----hoursperweek--------------------------------------------------------
df$hour.per.week[which(abs(scale(df$hour.per.week))>3)] <- mean(df$hour.per.week, na.rm=TRUE)

## ----tableworkclass------------------------------------------------------
table(df$workclass)

## ----agruparworkclass----------------------------------------------------
# Convertim a string per a poder treballar
df$workclass <- as.character(df$workclass)
# Agrupem els valors que comencen per " Self-emp" i li posem "Self"
df$workclass[startsWith(df$workclass, "Self-emp")] = "Self"
# Agrupem els valors que acaben per "-gov" i li posem "Govern"
df$workclass[endsWith(df$workclass, "-gov")] = "Govern"
# Agrupem els valors " Never-worked" i " Without-pay" i li posem "No-work"
df$workclass[df$workclass == "Never-worked"] = "No-work"
df$workclass[df$workclass == "Without-pay"] = "No-work"
# Tornem a transformar a factor
df$workclass <- as.factor(df$workclass)

## ----tableworkclass2-----------------------------------------------------
table(df$workclass)

## ----tablemaritalstatus--------------------------------------------------
table(df$marital.status)

## ----agruparmaritalstatus------------------------------------------------
df$marital.status <- as.character(df$marital.status)
# Agrupem els valors que comencen per " Married-" i li posem "Married"
df$marital.status[startsWith(df$marital.status, "Married-")] = "Married"
df$marital.status <- as.factor(df$marital.status)

## ----tablemaritalstatus2-------------------------------------------------
table(df$marital.status)

## ----agruparnationality--------------------------------------------------
df$native.country <- as.character(df$native.country)
# Agrupem els valors que no siguin de EEUU
df$native.country[df$native.country != "United-States"] = "Other"
df$native.country <- as.factor(df$native.country)

## ----stageoflife---------------------------------------------------------
# Funcio per calcular el stage of life
stageOfLife <- function(age) {
if(12<age&age<=19) { result <- "Adolescent"
  } else if (20 < age & age <= 25) {
    result <- "Adult-primerenc"
  } else if (26 < age & age <= 49) {
    result <- "Adult"
} else {
    result <- "Vellesa"
  }
  return(result)
}

# Afegim una nova variable a partir d'agrupar el camp age.
df$StageOfLife <- sapply(df$age, stageOfLife)
df$StageOfLife <- as.factor(df$StageOfLife)

## ----normcolumnToAnalyze-------------------------------------------------

columnToAnalyze <- c("age", "education.num", "hour.per.week", "capital")

for (columnName in columnToAnalyze) {
  # Generem una graella amb 1 fila i 2 columnes
  par(
    mfrow=c(1,2),
    oma = c(0, 0, 2, 0)
  )
  # Mostrem el histograma
  hist(
    df[,columnName],
    main="Histogram",
    xlab=paste(columnName, " values")
  )
  
  # Mostrem el qqplot
  qqnorm(df[,columnName], main = paste("Normal Q-Q Plot for", columnName))
  qqline(df[,columnName], col = "steelblue", lwd = 2)
  
}



## ----andersondarlingtest-------------------------------------------------
library(nortest)
for (columnName in columnToAnalyze) {
  print(ad.test(df[,columnName]))
}

## ----sexvsincome---------------------------------------------------------
table(df$sex, df$income)
prop.table(table(df$sex, df$income), margin = 1)

## ----chisqtest-----------------------------------------------------------
chisq.test(table(df$sex, df$income))

## ----correlation, message=FALSE, warning=FALSE---------------------------
library(polycor)

corr_matrix<-hetcor(df, ML=FALSE, std.err=FALSE)
corr_matrix$correlations

## ----correlationHeatmap, message=FALSE, warning=FALSE--------------------
library(reshape2)

ggplot(
  melt(corr_matrix$correlations), 
  aes(Var2, Var1, fill = value)
)+
geom_tile(color = "white")+
scale_fill_gradient2(
  low = "blue", 
  high = "red", 
  mid = "white", 
  midpoint = 0, 
  limit = c(-1,1), 
  space = "Lab", 
  name="Correlation") +
theme_minimal()+ # minimal theme
theme(
  axis.text.x = element_text(
    angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed()

## ----traintest-----------------------------------------------------------
library(caTools, message=FALSE, warning=FALSE)
# Fixem un valor per a que es pugui reproduir
set.seed(420)
samples = sample.split(df$age, SplitRatio = 0.75)

# Per a aquest prova usarem la variable de edat que indica l'etapa de la vida. Per això borrem la variable age
df_temp <- df
df_temp$age <- NULL

df_train <- subset(df_temp, samples == TRUE)
df_test <- subset(df_temp, samples == FALSE)

## ----regressioLogistica--------------------------------------------------
regLog <- glm(income ~ ., data = df_train, family = binomial('logit'))
summary(regLog)

## ----regLogconfMatrix----------------------------------------------------
prob <- predict(regLog, df_test, type = 'response')
pred <- rep('<=50K', length(prob))
pred[prob >= 0.5] <- '>50K'

# Matriu de confusio
conf_matrix <- table(pred, df_test$income)
conf_matrix

## ----accuracy------------------------------------------------------------
acc = (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)*100
acc

## ----ROC, message=FALSE, warning=FALSE-----------------------------------
library(pROC)
roc(df_test$income ~ prob, plot = TRUE, print.auc = TRUE)

## ----orderdeCoef---------------------------------------------------------
data.frame(V1=sort(exp(coefficients(regLog)), decreasing=TRUE))

## ----grafSexIncome-------------------------------------------------------
ggplot(
    data=df,
aes( x=sex,
        fill=income
    )
)+geom_bar(position="fill")

## ----graphworkclassincomesex---------------------------------------------
# Visualitzem la relació entre les variables "workclass", "income" i "sex".
ggplot(
    data=df,
    aes(
      x=workclass,
      fill=income
    )
) + geom_bar(position="fill") + facet_wrap(~sex)

## ----workclassincomesex--------------------------------------------------
ggplot(
  data=df,
  aes(
    x=workclass,
    fill=income
  )
) + geom_bar() + facet_wrap(~sex)

## ----jobsforstudy--------------------------------------------------------
# Guardem una variable amb els percentatges que relacionen ocupacio ieducació
pWorkEdu <- prop.table(table(df$occupation, df$education.num), 1) * 100
# Agafem el nom de la fila que té el màxim de probabilitat
apply(pWorkEdu, 2, function(x) rownames(pWorkEdu)[which.max(x)])

## ----educationNumincom---------------------------------------------------
ggplot(
    data=df,
aes( x=education.num,
        fill=income
    )
)+geom_bar(position="fill")

## ----StageOfLifemaritalstatus--------------------------------------------
# Relació entre les variables "StageOfLife" i "marital.status"
ggplot(
    data=df,
    aes(
      x=StageOfLife,
      fill=`marital.status`
    )
) + geom_bar(position="fill")

## ----maritalstatus-------------------------------------------------------
ggplot(
  data=df,
  aes(
    x=`marital.status`,
    fill=income
  )
) + geom_bar(position="fill")

## ----raceincome----------------------------------------------------------
ggplot(
  data=df,
  aes(
x=`race`,
    fill=income
  )
) + geom_bar(position="fill") + facet_wrap(~sex)

