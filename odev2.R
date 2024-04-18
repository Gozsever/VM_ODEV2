# Rastgele veri oluşturmak için MASS kütüphanesinden yararlanıyoruz

install.packages("MASS")
library(MASS)

# Rastgele veri setini oluşturma
set.seed(123) # Tekrarlanabilirlik için seed belirleme, rastgele sayı üretiminde kullanılan tohum değerini belirler
n <- 100 # Gözlem sayısı
x1 <- rnorm(n, mean = 5, sd = 2) # Bağımsız değişken 1
x2 <- rnorm(n, mean = 10, sd = 3) # Bağımsız değişken 2

#y değişkeni üretme
y <- 2 * x1 + 3 * x2 + rnorm (n, mean = 0 , sd = 1)

#data.frame ile oluşturulan 3 değişken için veri çerçevesi oluşturduk
data <- data.frame(x1, x2, y)

#Oluşturulan veri setinin ilk 6 satırını gösterme
head(data)

#veri çerçevesinin özet istatistiklerini bulalım
summary(data)
#veri setinin dağılımı ve merkezi eğilimi hakkında bilgi verir

#x1 değişkeni üzerinde kare dönüşümü yapalım
data$x1_squared <- data$x1^2

head(data) #kontrol edelim

#x2 değişkeni üzerinde kare dönüşümü yapalım
data$x2_squared <- data$x2^2

head(data)

#y değişkeni üzerinde logaritmik dönüşüm yapalım 
data$y_log <- log(data$y)

head(data)

#x1 ve x2 için eksik değer tespiti
is_na_x1 <- is.na(data$x1)
is_na_x2 <- is.na(data$x2)

is_na_x1 #kontrolleri yaptık ve eksik değer bulunmadı
is_na_x2

#Aykırı değerlere bakmak için kutu grafiği oluşturma
boxplot(data$x1, main="Box Plot For x1", ylab="x1 Values")
boxplot(data$x2, main="Box Plot For x2", ylab="x2 Values")
boxplot(data)

# z skorları hesaplama
z_scores <- scale(data$y)

#Aykırı değerlerin indekslerini bulma
outliers <- which(abs(z_scores) > 3)

#Aykırı değerlerin gösterimi
data[outliers,]

#Alt ve üst çeyreklikleri hesaplama
Q1 <- quantile(data$x1, 0.25)
Q3 <- quantile(data$x1, 0.75)

#Çeyrekliklerin aralığını hesaplama
IQR <- Q3 - Q1

#Alt ve üst sınır hesaplama katsayı olarak 2 kullandım 
lower_bound <- Q1 - 2 * IQR
upper_bound <- Q3 + 2 * IQR

#Aykırı değerleri belirleyelim
outliers <- data$x1 < lower_bound | data$x1 > upper_bound

#dağılımları keşif
install.packages("ggplot2")
library(ggplot2)

#Yoğunluk grafiği yapalım
ggplot(data, aes(x=x1)) + geom_density(fill= "skyblue", color = "blue") + labs (title= "Yoğunluk Grafiği", x= "X1 Değeri", y= "Yoğunluk")

#Kutu grafiği
ggplot(data, aes(y=x1)) + geom_boxplot(fill= "skyblue", color = "grey") + labs (title= "Kutu Grafiği", x="", y="X1 Değeri")

#Scatter plot grafiği
ggplot(data, aes(x=x1, y=y)) + geom_point(color="blue") + labs(title= "Scatter Plot Grafiği", x="X1 Değeri", y="Y Değeri")

#Veri dönüşümü yapmak için tidyverse paketini kullandım
install.packages("tidyverse")
library(tidyverse)

#Korelasyon matrisi oluşturalım
correlation_matrix <- cor(data)

#Matrisi yazdıralım
print(correlation_matrix)

#Korelasyon katsayılarının mutlak değerlerini bulma
correlation_values <- abs(correlation_matrix)

install.packages("caret")
library(caret)
#korelasyon katsayıları 0.7'den büyük olan değişkenler arasından birini seçip diğerlerini kaldıralım
highly_correlated <- findCorrelation(correlation_values, cutoff=0.7)
selected_variables <- colnames(correlation_values)[-highly_correlated]

#secilen değişkenleri kullanarak yeni veri seti oluşturma
data_selected <- data[,selected_variables]

#standartlaştırma yapalım
data$standardized_x1 <- scale(data$x1)
data$standardized_x2 <- scale(data$x2)

install.packages("glmnet")
library(glmnet)

#rastgele veri üretelim
set.seed(123)

#bağımsız değişkenler
x3 <- rnorm(100)
x4 <- runif(n, min=0, max=10) #uniform dağılımlı bağımsız değişken
x5 <- rpois(n, lambda = 5) #poisson dağılımlı bağımsız değişken
x6 <- rbeta(n, shape1= 2, shape2= 5) #beta dağılımlı bağımsız değişken

#bağımlı değişken
y2 <- 2 * x3 + 3 * x4 + 4 * x5 + 5 * x6 + rnorm(100, mean=0, sd=1)

#veri çerçevesi
data <- data.frame(x3,x4,x5,x6,y2)

#bağımlı ve bağımsız değişkenleri tanımlayalım
x <- as.matrix(data[, -5])
y <- data$y2


#lasso regresyon modeli oluşturalım 
lasso_model <- glmnet (x, y, alpha=1)

#modeli görselleştir
plot(lasso_model)

#katsayıların barplot ile görselleştirilmesi
#barplot fonsiyonu vektör verilerini ksbul eder
#bu sebeple katsayıları vektöre dönüştürdük
coef_vector <- coef(lasso_model)[-1]
barplot(coef_vector, main="Katsayıların dağılımı", xlab="Değişkenler", ylab="Katsayılar", col="skyblue")

#Veriyi çekerek keşifsel veri analizi uygulama
#Bu veri seti, kalp hastalığı teşhisi için kullanılan bir dizi sağlık verisini içerir. 
#Veri seti, 14 farklı özellikten oluşmaktadır ve bu özelliklerin bazıları;
#yaş, cinsiyet, göğüs ağrısı türü, istirahat kan basıncı, serum kolesterol, 
#açlık kan şekeri, dinlenme elektrokardiyografik sonuçlar, maksimum kalp atış hızı,
#egzersize bağlı anjina, egzersize bağlı ST depresyonu, tepe egzersiz ST segmentinin eğimi, majör damarların sayısı ve Talasemi gibi özelliklerdir.
#Veri setinin amacı, verilen özelliklere dayanarak bir hastanın kalp hastalığı olup olmadığını tahmin etmektir.

install.packages("readxl")
install.packages("readr")
install.packages("tidyverse")
install.packages("glmnet")

library(readxl)
library(readr)
library(tidyverse)
library(glmnet)
install.packages("openxlsx")
library(openxlsx)

data1 = read.xlsx("heart_disease_uci.xlsx") #Veriyi sağ taraftaki konsolda bulunan upload kısmından yaptım
#daha sonra dosyamızı seçerek import dataset yapıldı ve verilerimizi çağırmış olduk.

head(data1)

view(data1)

summary(data1) #datamız ile ilgili istatistiksel bilgileri verir

#veriyi data frame ile tablolaştırıyoruz
data1_df = data.frame(data1)
data1_df_tib = as_tibble(data1_df)

head(data1_df_tib)

#devamında veri setindeki özelliklere bakarak bir hastanın kalp hastalığına
#sahip olup olmadığını belirlemek için bir model oluşturalım

#veri setimizin yapısına baktık
str(data1_df_tib)

#eksik verileri bulalım
missing_values = colSums(is.na(data1_df_tib))
print(missing_values)

# sayısal değişkenler için eksik değerleri dolduralım
numeric_variables <- c("trestbps", "chol", "thalch") 

for (variable in numeric_variables) {
  if (sum(is.na(data1_df_tib[[variable]])) > 0) { # Eğer değişkende eksik değer varsa
    mean_value <- mean(data1_df_tib[[variable]], na.rm = TRUE) # Ortalama değeri hesapla
    data1_df_tib[[paste0(variable, "_filled")]] <- ifelse(is.na(data1_df_tib[[variable]]), mean_value, data1_df_tib[[variable]]) # Eksik değerleri ortalama ile doldurma
  }
}

# kategorik değişkenler için eksik değerleri doldur
categorical_variables <- c("fbs", "restecg")

for (variable in categorical_variables) {
  if (sum(is.na(data1_df_tib[[variable]])) > 0) { # Eğer değişkende eksik değer varsa
    data1_df_tib[[paste0(variable, "_filled")]] <- ifelse(is.na(data1_df_tib[[variable]]), "Unknown", data1_df_tib[[variable]]) # Eksik değerleri "Unknown" kategorisi ile doldur
  }
}

#doldurulmuş değişkenlerin özetini alalım
summary(data1_df_tib)

# 'trestbps' değişkenindeki eksik değerleri ortalama ile doldur
mean_trestbps <- mean(data1_df_tib$trestbps, na.rm = TRUE)
data1_df_tib$trestbps_filled <- ifelse(is.na(data1_df_tib$trestbps), mean_trestbps, data1_df_tib$trestbps)

# "trestbps" değişkenindeki aykırı değerleri belirleme
Q1 <- quantile(data1_df_tib$trestbps_filled, 0.25)
Q3 <- quantile(data1_df_tib$trestbps_filled, 0.75)
IQR_age <- Q3 - Q1
lower_bound_age <- Q1 - 1.5 * IQR_age
upper_bound_age <- Q3 + 1.5 * IQR_age
outliers_age <- data1_df_tib$trestbps_filled[data1_df_tib$trestbps_filled < lower_bound_age | data1_df_tib$trestbps_filled > upper_bound_age]

# aykırı değerlerin sayısını ve değerlerini yazdıralım
print(paste("Aykırı değerlerin sayısı (trestbps):", length(outliers_age)))
print(outliers_age)

# aykırı değerleri görselleştirme 
boxplot(data1_df_tib$trestbps_filled, main = "Boxplot of trestbps with Outliers", outline = TRUE)

# aykırı değerlerin sayısını ve değerlerini yazdırma
print(paste("Aykırı değerlerin sayısı (age):", length(outliers_age)))
print(outliers_age)

# sadece sayısal değerlerin bulunduğu sütunları seçelim
numeric_data <- data1_df_tib[sapply(data1_df_tib, is.numeric)]

# korelasyon matrisini hesaplama
correlation_matrix <- cor(numeric_data)

# seçilen bağımlı değişken trestbps
dependent_variable <- "trestbps"

# seçilen bağımsız değişken age
independent_variable <- "age"

# seçilen bağımlı değişkenle seçilen bağımsız değişken arasındaki ilişkiyi inceleyin 
correlation <- cor(data1_df_tib[[dependent_variable]], data1_df_tib[[independent_variable]])


# sonucu yazdırmak için 
print(paste("Bağımlı değişken ile bağımsız değişken arasındaki korelasyon:", correlation))

#karar ağacı modeli oluşturmak için paket
install.packages("rpart")
library(rpart)

# karar ağacı modelini oluşturur
decision_tree_model <- rpart(age ~ ., data = data1_df_tib)

# model özetini yazdıralım
print(decision_tree_model)

# karar ağacını görselleştirme
plot(decision_tree_model)
text(decision_tree_model)

install.packages("rpart.plot")
library(rpart.plot)

# karar ağacını daha güzel bir şekilde görselleştirmeye yarar
rpart.plot(decision_tree_model)

# karar ağacı modelinin özetini alalım
summary(decision_tree_model)

# Burada "ca_filled" ve "thalch_filled" gibi belirli özelliklerin karar ağacı modelinde 
# yüksek bir önemi var. Bu özelliklerin modelin tahmini için büyük bir rol oynadığını 
# ve sonuçlarını belirlemede önemli olduğunu söyleyebiliriz.


