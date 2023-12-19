
#1
library(openintro)
library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)
library(Metrics)
library(ROSE)
library(car)
library(lattice)

laptops <- read.csv("laptops.csv")

head(laptops)

glimpse(laptops)

# Crearea unui subset cu laptopuri de la brandul "Asus"
laptops_asus <- filter(laptops, Brand == "Asus")

# Crearea unui subset cu laptopuri care au cel puțin 512 GB de stocare
laptops_large_storage <- filter(laptops, Storage >= 512)

# Verificarea dimensiunilor acestor subseturi
glimpse(laptops_asus)
glimpse(laptops_large_storage)
# Calcularea medianei pentru RAM
med_RAM <- median(laptops$RAM)

# Crearea unei noi coloane categorice pentru RAM
laptops <- laptops %>%
  mutate(RAM_cat = if_else(RAM < med_RAM, "sub mediană", "egal sau peste mediană"))

# Verificarea rezultatului
glimpse(laptops)

# Combinați tipurile de CPU în categorii mai generale
laptops <- laptops %>%
  mutate(CPU_cat = if_else(grepl("Intel Core i7", CPU), "High-End", 
                           if_else(grepl("Intel Core i5", CPU), "Mid-Range", "Other")))

# Verificarea rezultatului
glimpse(laptops)

# Vizualizarea relației între Preț și Capacitatea de Stocare
ggplot(laptops, aes(x = Storage, y = `Final.Price`)) +
  geom_point() +
  labs(title = "Relația dintre Capacitatea de Stocare și Prețul Laptopurilor",
       x = "Capacitatea de Stocare (GB)",
       y = "Prețul Final (Euro)") +
  theme_minimal()
# Utilizarea funcției summary
summary(laptops)

# Utilizarea funcției glimpse pentru o vedere de ansamblu
glimpse(laptops)

# Recodificarea variabilei RAM
laptops <- laptops %>%
  mutate(RAM_size = case_when(
    RAM <= 4 ~ "mic",
    RAM > 4 & RAM <= 8 ~ "mediu",
    TRUE ~ "mare"
  ))

# Verificarea rezultatului
glimpse(laptops)

# De exemplu, explorarea corelației între RAM și Preț
cor(laptops$RAM, laptops$`Final.Price`, use = "complete.obs")

# Recodificarea variabilei 'Storage'
laptops <- laptops %>%
  mutate(Storage_cat = case_when(
    Storage <= 256 ~ "mic",
    Storage > 256 & Storage <= 512 ~ "mediu",
    TRUE ~ "mare"
  ))

# Verificarea noii variabile
glimpse(laptops)

names(laptops)


#2

# Înlocuirea valorilor lipsă din GPU cu "integrated"
laptops$GPU <- ifelse(laptops$GPU == "", "integrated", laptops$GPU)

# Înlocuirea valorilor lipsă din Storage.type cu "Unknown"
laptops$Storage.type <- ifelse(laptops$Storage.type == "", "Unknown", laptops$Storage.type)

# Calcularea medianei pentru Screen și înlocuirea valorilor lipsă
med_screen <- median(laptops$Screen, na.rm = TRUE)
laptops$Screen <- ifelse(is.na(laptops$Screen), med_screen, laptops$Screen)


# Verificarea setului de date după curățare
head(laptops)


# Filtrarea și eliminarea nivelurilor cu număr mic de observații
laptops_filtered <- laptops %>%
  filter(`Storage.type` != "nivel_cu_puține_observații") %>%
  droplevels()

# Verificarea nivelurilor rămase în variabila 'Storage type'
levels(laptops_filtered$`Storage.type`)


# Grafic cu bare laterale pentru Brand și Storage type
ggplot(laptops, aes(x = Brand, fill = `Storage.type`)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuția Tipurilor de Stocare pe Branduri",
       x = "Brand",
       y = "Numărul de Laptopuri",
       fill = "Tip de Stocare")

# Grafic cu bare laterale pentru Storage type și Brand
ggplot(laptops, aes(x = `Storage.type`, fill = Brand)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuția Brandurilor pe Tipuri de Stocare",
       x = "Tip de Stocare",
       y = "Numărul de Laptopuri",
       fill = "Brand")

# Crearea unei tabele de continuitate pentru Brand și Status
contingency_table <- laptops %>%
  count(Brand, Status) %>%
  pivot_wider(names_from = Status, values_from = n)


# Identifică nivelul cu cele mai puține intrări din tabela de continuitate
lowest_level <- which.min(colSums(contingency_table[, -1])) + 1  # +1 pentru a lua în considerare coloana "Brand"

# Filtrarea și eliminarea nivelurilor cu cele mai puține intrări
laptops_filtered <- subset(laptops, Brand != levels(Brand)[lowest_level])


# Eliminarea nivelurilor nefolosite
laptops_filtered$Brand <- droplevels(laptops_filtered$Brand)


# Crearea barchart-ului pentru Brand și Status
barchart1 <- ggplot(laptops, aes(x = Brand, fill = Status)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuția Laptopurilor după Brand și Status",
       x = "Brand",
       y = "Numărul de Laptopuri",
       fill = "Status")

barchart2 <- ggplot(laptops, aes(x = Status, fill = Brand)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuția Laptopurilor după Status și Brand",
       x = "Status",
       y = "Numărul de Laptopuri",
       fill = "Brand")

#2.1
# Crearea unui tabel de contingenta pentru Brand și Storage type
laptops_contingency <- laptops %>%
  count(Brand, `Storage.type`) %>%
  pivot_wider(names_from = `Storage.type`, values_from = n)

# Verificarea tabelului de contingenta
print(laptops_contingency)

# Aflarea nivelului cu cele mai puține observații
lowest_level <- which.min(colSums(laptops_contingency[, -1]))

# Filtrarea și eliminarea nivelurilor cu cele mai puține intrări
laptops_filtered <- filter(laptops, Brand != levels(laptops$Brand)[lowest_level])
laptops_filtered$Brand <- droplevels(laptops_filtered$Brand)

# Verificarea datelor după filtrare
print(laptops_filtered)

# Grafic cu bare laterale pentru Brand și Storage type
ggplot(laptops, aes(x = Brand, fill = `Storage.type`)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuția Tipurilor de Stocare pe Branduri",
       x = "Brand",
       y = "Numărul de Laptopuri",
       fill = "Tip de Stocare")

# Grafic cu bare laterale pentru Storage type și Brand
ggplot(laptops, aes(x = `Storage.type`, fill = Brand)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuția Brandurilor pe Tipuri de Stocare",
       x = "Tip de Stocare",
       y = "Numărul de Laptopuri",
       fill = "Brand")

# Histograma pentru Final Price cu diferite lățimi de bandă
ggplot(laptops, aes(x = `Final.Price`)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Distribuția Prețurilor Laptopurilor", x = "Preț", y = "Frecvența")

# Boxplot pentru Final Price
ggplot(laptops, aes(y = `Final.Price`)) +
  geom_boxplot() +
  labs(title = "Boxplot pentru Prețurile Laptopurilor", y = "Preț")

# Histograma faceted pentru Final Price segmentată după Storage type
ggplot(laptops, aes(x = `Final.Price`)) +
  geom_histogram(binwidth = 100) +
  facet_wrap(~`Storage.type`) +
  labs(title = "Distribuția Prețurilor Laptopurilor pe Tipuri de Stocare",
       x = "Preț Final", y = "Frecvența")

# Pipeline de date pentru filtrarea și vizualizarea laptopurilor sub 1000 de dolari
laptops %>%
  filter(`Final.Price` < 1000) %>%
  ggplot(aes(x = Brand, fill = Brand)) +
  geom_bar() +
  labs(title = "Numărul de Laptopuri sub 1000 de Dolari pe Branduri",
       x = "Brand", y = "Numărul de Laptopuri")

# Analiza detaliată a variabilei RAM
ggplot(laptops, aes(x = RAM)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribuția Cantității de RAM în Laptopuri", x = "RAM (GB)", y = "Frecvența")

# Boxplot pentru a examina outlier-ii în Final Price
ggplot(laptops, aes(y = `Final.Price`)) +
  geom_boxplot() +
  labs(title = "Outlier-ii în Prețurile Laptopurilor", y = "Preț Final")

# Vizualizarea relației dintre RAM și Final Price
ggplot(laptops, aes(x = RAM, y = `Final.Price`)) +
  geom_point() +
  labs(title = "Relația dintre RAM și Prețul Final al Laptopurilor", x = "RAM (GB)", y = "Preț Final")

#3

# Calculul mediei și medianei prețului pentru fiecare Brand
mean_median_by_brand <- laptops %>%
  group_by(Brand) %>%
  summarize(mean_price = mean(`Final.Price`, na.rm = TRUE), 
            median_price = median(`Final.Price`, na.rm = TRUE))

# Verificarea rezultatelor
print(mean_median_by_brand)

# Calculul deviației standard și IQR pentru preț, grupate după Brand
spread_by_brand <- laptops %>%
  group_by(Brand) %>%
  summarize(sd_price = sd(`Final.Price`, na.rm = TRUE), 
            iqr_price = IQR(`Final.Price`, na.rm = TRUE), 
            count = n())

# Verificarea rezultatelor
print(spread_by_brand)

# Calculul măsurilor pentru un Brand specific, de exemplu "Asus"
center_spread_asus <- filter(laptops, Brand == "Asus") %>%
  summarize(mean_price = mean(`Final.Price`, na.rm = TRUE), 
            sd_price = sd(`Final.Price`, na.rm = TRUE))

# Verificarea rezultatelor
print(center_spread_asus)

# Crearea unei variabile transformate log(RAM) și vizualizarea distribuției
laptops <- mutate(laptops, log_RAM = log(RAM))

# Vizualizarea distribuției
ggplot(laptops, aes(x = log_RAM)) +
  geom_density() +
  labs(title = "Distribuția Logaritmică a RAM-ului", x = "Log(RAM)", y = "Densitate")

# Filtrarea și eliminarea outlier-ilor pentru un brand specific
laptops_no_outliers <- filter(laptops, Brand == "Asus" & `Final.Price` < 2000)

# Crearea unui boxplot pentru distribuția rămasă
ggplot(laptops_no_outliers, aes(y = `Final.Price`, x = Brand)) +
  geom_boxplot() +
  labs(title = "Boxplot pentru Prețurile Laptopurilor Asus (Fără Outlier-i)", y = "Preț Final")

#4

# Crearea unui scatterplot pentru Final.Price și RAM
ggplot(data = laptops, aes(x = RAM, y = `Final.Price`)) +
  geom_point() +
  labs(x = "RAM (GB)", y = "Preț Final (Dolari)", title = "Relația dintre RAM și Prețul Final al Laptopurilor")

# Discretizarea variabilei RAM și crearea unui boxplot
laptops <- mutate(laptops, RAM_cut = cut(RAM, breaks = 5))

ggplot(data = laptops, aes(x = RAM_cut, y = `Final.Price`)) +
  geom_boxplot() +
  labs(x = "Categorie RAM (GB)", y = "Preț Final (Euro)", title = "Distribuția Prețurilor pe Categorii de RAM")

# Crearea unui scatterplot pentru Final.Price și Screen
ggplot(data = laptops, aes(x = Screen, y = `Final.Price`)) +
  geom_point() +
  labs(x = "Dimensiunea Ecranului (inci)", y = "Preț Final (Euro)", title = "Relația dintre Dimensiunea Ecranului și Prețul Final")
# Crearea boxplot-urilor pentru distribuția prețurilor pe categorii de RAM
ggplot(data = laptops, aes(x = RAM_cut, y = `Final.Price`)) +
  geom_boxplot() +
  labs(x = "Categorie RAM (GB)", y = "Preț Final (Euro)", title = "Boxplot-uri pentru Prețuri pe Categorii de RAM")

# Scatterplot pentru Final.Price și RAM
ggplot(data = laptops, aes(x = RAM, y = `Final.Price`)) +
  geom_point() +
  labs(x = "RAM (GB)", y = "Preț Final (Euro)", title = "Relația dintre RAM și Prețul Final al Laptopurilor")
# Scatterplot pentru Final.Price și Storage
ggplot(data = laptops, aes(x = Storage, y = `Final.Price`)) +
  geom_point() +
  labs(x = "Capacitatea de Stocare (GB)", y = "Preț Final (Euro)", title = "Relația dintre Stocare și Prețul Final")
# Scatterplot pentru Final.Price și Screen, diferențiat după Touch
ggplot(data = laptops, aes(x = Screen, y = `Final.Price`, color = as.factor(Touch))) +
  geom_point() +
  labs(x = "Dimensiunea Ecranului (inci)", y = "Preț Final (Euro)", title = "Relația dintre Ecran și Preț, Diferențiată după Touch")
# Scatterplot pentru Final.Price și Brand
ggplot(data = laptops, aes(x = as.factor(Brand), y = `Final.Price`)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Brand", y = "Preț Final (Dolari)", title = "Relația dintre Brand și Prețul Final al Laptopurilor")
# Log-transformarea datelor și crearea unui scatterplot
laptops <- mutate(laptops, log_Final_Price = log(`Final.Price`), log_RAM = log(RAM))

ggplot(data = laptops, aes(x = log_RAM, y = log_Final_Price)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Log(RAM)", y = "Log(Preț Final)", title = "Scatterplot Log-transformed pentru RAM și Prețul Final")
# Identificarea și vizualizarea outlier-ilor
ggplot(data = laptops, aes(x = RAM, y = `Final.Price`)) +
  geom_point(alpha = 0.5) +
  geom_jitter(alpha = 0.5, color = "sky blue", width = 0.2) +
  labs(x = "RAM (GB)", y = "Preț Final (Dolari)", title = "Identificarea Outlier-ilor în Relația dintre RAM și Prețul Final")

# Crearea unui subset de date și scatterplot
laptops_gt_512 <- filter(laptops, Storage > 512)

ggplot(data = laptops_gt_512, aes(x = Screen, y = `Final.Price`)) +
  geom_point() +
  labs(x = "Dimensiunea Ecranului (inci)", y = "Preț Final (Euro)", title = "Scatterplot pentru Laptopuri cu Peste 512 GB Stocare")

#5

# Scatterplot cu linie de regresie liniară pentru RAM și Final.Price
ggplot(data = laptops, aes(x = RAM, y = `Final.Price`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "RAM (GB)", y = "Preț Final (Dolari)", title = "Relația dintre RAM și Prețul Final cu Linia de Regresie")

# Fitting a linear model
lm_model <- lm(`Final.Price` ~ RAM, data = laptops)

# Print the summary of the model
summary(lm_model)

# Identificarea outlier-ilor
ggplot(data = laptops, aes(x = RAM, y = `Final.Price`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "RAM (GB)", y = "Preț Final (Dolari)", title = "Identificarea Outlier-ilor în Relația dintre RAM și Prețul Final")

# Crearea unui model liniar pentru Final.Price ca funcție de RAM
price_ram_mod <- lm(`Final.Price` ~ RAM, data = laptops)

# Vizualizarea coeficienților modelului
coef(price_ram_mod)

# Scatterplot cu linia de regresie pentru Final.Price vs RAM
ggplot(data = laptops, aes(x = RAM, y = `Final.Price`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "RAM (GB)", y = "Preț Final (Dolari)", title = "Modelul Liniar pentru Preț vs. RAM")


# Crearea unui model liniar pentru Final.Price ca funcție de Storage
price_storage_mod <- lm(`Final.Price` ~ Storage, data = laptops)

# Vizualizarea coeficienților modelului
coef(price_storage_mod)

# Scatterplot cu linia de regresie pentru Final.Price vs Storage
ggplot(data = laptops, aes(x = Storage, y = `Final.Price`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Capacitatea de Stocare (GB)", y = "Preț Final (Dolari)", title = "Modelul Liniar pentru Preț vs. Storage")


# Crearea unui model liniar pentru Final.Price ca funcție de Screen
price_screen_mod <- lm(`Final.Price` ~ Screen, data = laptops)

# Vizualizarea coeficienților modelului
coef(price_screen_mod)

# Scatterplot cu linia de regresie pentru Final.Price vs Screen
ggplot(data = laptops, aes(x = Screen, y = `Final.Price`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Dimensiunea Ecranului (inci)", y = "Preț Final (Dolari)", title = "Modelul Liniar pentru Preț vs. Screen")


# Utilizarea funcției coef() pentru afișarea coeficienților
coef(price_ram_mod)

# Utilizarea funcției summary() pentru afișarea unei analize complete a modelului
summary(price_ram_mod)

# Crearea unui nou data frame cu datele augmentate ale modelului liniar
library(broom)
hgt_wgt_tidy <- augment(price_ram_mod)

# Vizualizarea datelor augmentate
glimpse(hgt_wgt_tidy)

#5 modelul logistic

# Asigurați-vă că Status este transformat corect în binar
laptops$Status_bin <- as.numeric(laptops$Status == "New")

# Divizați setul de date
set.seed(123)
partition <- createDataPartition(laptops$Status_bin, p = 0.8, list = FALSE)
training_set <- laptops[partition, ]
test_set <- laptops[-partition, ]

# Verificați nivelurile variabilei prezise
unique(predicted_classes)

# Verificați nivelurile variabilei reale din setul de test
unique(test_set$Status_bin)


# Eliminați observațiile cu niveluri necunoscute
test_set <- test_set[test_set$Brand %in% unique_levels_train, ]

# Refacerea modelului
laptop_logit_model <- glm(Status_bin ~ Brand + RAM + Storage + Screen, data = training_set, family = "binomial")

# Predicția pe setul de test și evaluarea
predicted_probabilities <- predict(laptop_logit_model, newdata = test_set, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
table(predicted_classes)



# Calculul matricei de confuzie folosind clasele corect clasificate
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_set$Status_bin))

# Afișarea rezultatelor
confusion_matrix

# Calculul AUC
auc_score <- roc(test_set$Status_bin, predicted_probabilities)$auc

# Verificarea și afișarea metricilor
confusion_matrix$overall
confusion_matrix$byClass

summary(predicted_probabilities)

table(predicted_classes)

table(test_set$Status_bin)




#------------------------------------------------------------------




laptops$StatusBinary <- ifelse(laptops$Status == 'New', 1, 0)

# Divizarea datelor
set.seed(123)
split <- initial_split(laptops, prop = 0.7)
laptops_train <- training(split) %>% mutate(StatusBinary = ifelse(Status == 'New', 1, 0))
laptops_test <- testing(split) %>% mutate(StatusBinary = ifelse(Status == 'New', 1, 0))

# Modele de regresie liniară
model_lin_ram <- train(Final.Price ~ RAM, data = laptops_train, method = "lm")
print(summary(model_lin_ram))
ggplot(laptops_train, aes(x = RAM, y = Final.Price)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresie Liniară: Prețul Final vs RAM")

model_lin_storage <- train(Final.Price ~ Storage, data = laptops_train, method = "lm")
print(summary(model_lin_storage))
ggplot(laptops_train, aes(x = Storage, y = Final.Price)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresie Liniară: Prețul Final vs Storage")

model_lin_ram_storage <- train(Final.Price ~ RAM + Storage, data = laptops_train, method = "lm")
print(summary(model_lin_ram_storage))
ggplot(laptops_train, aes(x = RAM, y = Final.Price, color = as.factor(Storage))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresie Liniară: Prețul Final vs RAM și Storage")


# Modele de regresie logistică
laptops_train$StatusBinary <- factor(laptops_train$StatusBinary, levels = c(0, 1))
model_log_ram <- train(StatusBinary ~ RAM, data = laptops_train, method = "glm", family = "binomial", trControl = trainControl(method = "none"))
predicted_probabilities_ram <- predict(model_log_ram, laptops_train, type = "prob")
print(str(predicted_probabilities_ram))

# Crearea plotului cu lattice fără linia netedă
lattice::xyplot(StatusBinary ~ RAM, data = laptops_train, 
                groups = predicted_probabilities_ram[, "1"],
                type = "p",
                auto.key = list(columns = 2),
                xlab = "RAM", ylab = "Probabilitatea de a fi 'Nou'")


model_log_storage <- train(StatusBinary ~ Storage, data = laptops_train, method = "glm", family = "binomial")
predicted_probabilities_storage <- predict(model_log_storage, laptops_train, type = "prob")[, 2]
# Crearea plotului cu lattice cu ajustarea span-ului pentru modelul logistic cu Storage
lattice::xyplot(StatusBinary ~ Storage, data = laptops_train, 
                groups = predicted_probabilities_storage,
                type = c("p", "smooth"), 
                span = 1,  
                auto.key = list(columns = 2),
                xlab = "Storage", ylab = "Probabilitatea de a fi 'Nou'")




# Predicții pe setul de test
predicted_prob <- predict(model_log_ram, newdata = laptops_test, type = "prob")

# Utilizarea unui prag (de exemplu, 0.5) pentru a determina clasificarea
predicted_class <- ifelse(predicted_prob[, "1"] > 0.5, 1, 0)


# Calcularea matricei de confuzie
confusionMatrix <- table(Predicted = predicted_class, Actual = laptops_test$StatusBinary)

# Calculul metricilor
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
sensitivity <- confusionMatrix[2,2] / sum(confusionMatrix[2,])
specificity <- confusionMatrix[1,1] / sum(confusionMatrix[1,])
precision <- confusionMatrix[2,2] / sum(confusionMatrix[,2])
recall <- sensitivity  # Sensibilitatea este echivalentă cu recall
F1_score <- 2 * (precision * recall) / (precision + recall)

# Calculul AUC
predicted_prob <- predict(model_log_ram, newdata = laptops_test, type = "prob")
ROC_curve <- pROC::roc(response = laptops_test$StatusBinary, predictor = predicted_prob[,2])
AUC <- pROC::auc(ROC_curve)

# Printarea rezultatelor
cat("Acuratețe:", accuracy, "\n",
    "Sensibilitate (Recall):", recall, "\n",
    "Specificitate:", specificity, "\n",
    "Precizie:", precision, "\n",
    "F1-score:", F1_score, "\n",
    "AUC:", AUC, "\n")

# Plotarea curbei ROC
plot(ROC_curve, main = "Curba ROC pentru Modelul Logistic", col = "#1c61b6")

