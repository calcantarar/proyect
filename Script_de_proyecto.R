#Instalar packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("gtsummary")
install.packages("MatchIt")
install.packages("pubh")
install.packages("cardx")
install.packages("ggmosaic")
install.packages("gt")
install.packages("broom")

#Cargar librerias a utilizar
library(tidyverse)
library(dplyr)
library(gtsummary)
library(readxl)
library(MatchIt)
library(pubh)
library(broom)
library(gt)
library(ggmosaic)

#Ubicar los datos a usar
tuberc = read_xlsx("base_4/pone.xlsx")

#Dimensión de los datos
dim(tuberc)

# Crear categoría de nivel socioeconómico basada en ingreso
tuberc1 <- tuberc %>%
  mutate(socioecon_status = case_when(
    substr(Q204income, 1, 1) == "1" ~ "Low",
    substr(Q204income, 1, 1) == "2" ~ "Medium",
    TRUE ~ "High")) %>%
  mutate(socioecon_status = factor(socioecon_status, levels = c("Low", "Medium", "High")))

#Filtrar datos de controles
controls = tuberc1 %>% 
  filter(substr(Seri_No, 1, 1) == "C")

#Filtrar datos de casos
cases = tuberc1 %>%
  filter(substr(Seri_No, 1, 1) != "C")

#Verificar datos únicos
summary(controls)
summary(cases)

unique(cases$Seri_No)
unique(controls$Seri_No)

# Tablas de doble entrada
## Tablas con factores identificados como predictores independientes
tbl_summary(cases, include=c(Q207numbwindows, Q508admitted, 
                             Q209housemebertb, Q104educ_Status,
                             Q301smoke, Q503vaccinated), by=socioecon_status)%>%
  modify_spanning_header(all_stat_cols() ~ "**Socioeconomic status**") %>% #Agregar encabezado
  modify_header(label = "**Variables (Cases)**") %>% #Modificar título de primera columna
  modify_footnote(everything() ~ NA) #Quitar pie de página

tbl_summary(controls, include=c(Q207numbwindows, Q508admitted, 
                                Q209housemebertb, Q104educ_Status, 
                                Q301smoke, Q503vaccinated), by=socioecon_status) %>%
  modify_spanning_header(all_stat_cols() ~ "**Socioeconomic status**") %>% #Agregar encabezado
  modify_header(label = "**Variables (Controls)**") %>% #Modificar título de primera columna
  modify_footnote(everything() ~ NA) #Quitar pie de página


#Seleccionar variables relevantes
## Para controles
controls1 <- controls %>%
  select(Q207numbwindows, Q508admitted, 
         Q209housemebertb, Q104educ_Status,
         socioecon_status, Q301smoke, Q503vaccinated) %>%
  rename(
    `N° de ventanas` = Q207numbwindows,
    `Admision a hospital` = Q508admitted,
    `Miembros en el hogar` = Q209housemebertb,
    `Nivel educativo` = Q104educ_Status,
    `Fumadores` = Q301smoke,
    `Vacunados` = Q503vaccinated) %>% #Cambiar nombre de las variables
  pivot_longer(cols = c(`N° de ventanas`, `Admision a hospital`, `Miembros en el hogar`, `Nivel educativo`, `Fumadores`, `Vacunados`),
               names_to = "Variable", values_to = "Value") %>% #Convertir datos a formato largo
  drop_na() #Remover datos vacios


## Para casos
cases1 <- cases %>%
  select(Q207numbwindows, Q508admitted, 
         Q209housemebertb, Q104educ_Status,
         socioecon_status, Q301smoke, Q503vaccinated) %>%
  rename(
    `N° de ventanas` = Q207numbwindows,
    `Admision a hospital` = Q508admitted,
    `Miembros en el hogar` = Q209housemebertb,
    `Nivel educativo` = Q104educ_Status,
    `Fumadores` = Q301smoke,
    `Vacunados` = Q503vaccinated) %>% #Cambiar nombre de las variables
  pivot_longer(cols = c(`N° de ventanas`, `Admision a hospital`, `Miembros en el hogar`, `Nivel educativo`, `Fumadores`, `Vacunados`),
               names_to = "Variable", values_to = "Value") %>% #Convertir datos a formato largo
  drop_na() #Remover datos vacios

#Creación de gráfica de barras (Nivel socioeconómico vs Variables)
## Para controles
ggplot(controls1, aes(x = Value, fill = socioecon_status)) +
  geom_bar(position = "fill") +  # Gráficos apilados proporcionalmente
  facet_wrap(~ Variable, scales = "free") +  # Un gráfico por cada variable
  scale_fill_brewer(palette = "Dark2") +  # Paleta de colores atractiva
  labs(title = "Distribución del Nivel Socioeconómico según Diferentes Variables - Control",
       x = "Categorías",
       y = "Proporción",
       fill = "Nivel Socioeconómico") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas para mejor lectura

## Para casos
ggplot(cases1, aes(x = Value, fill = socioecon_status)) +
  geom_bar(position = "fill") +  # Gráficos apilados proporcionalmente
  facet_wrap(~ Variable, scales = "free") +  # Un gráfico por cada variable
  scale_fill_brewer(palette = "Dark2") +  # Paleta de colores atractiva
  labs(title = "Distribución del Nivel Socioeconómico según Diferentes Variables - Casos",
       x = "Categorías",
       y = "Proporción",
       fill = "Nivel Socioeconómico") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas para mejor lectura

# Análisis epidemiológico
## Semilla
set.seed(2023)

## Ajustar datos
tuberc2 = tuberc1 %>%
  rename(Number_of_windows = Q207numbwindows, 
         Admitted_at_hospitals = Q508admitted, 
         House_members_with_TB = Q209housemebertb,
         Education_status = Q104educ_Status,
         Smokers = Q301smoke,
         Vaccinated = Q503vaccinated,
         Socioeconomic_status = socioecon_status) #Renombrar columnas

tuberc2 = tuberc2 %>%
  mutate(Educated_pop = case_when(
    substr(Education_status, 1, 1) == "1" ~ "Illiterate",
    TRUE ~ "Literate")) # Crear nueva columna para analfabetos y no analfabetos

## Crear tabla
### Definir casos y controles
tuberc2 <- tuberc2 %>%
  mutate(
    TB_Caso = ifelse(Seri_No <= 263, "Caso", "Control"),
    TB_Caso = factor(TB_Caso, levels = c("Control", "Caso")),
    
    ### Variables categóricas
    Socioeconomic_status = factor(Socioeconomic_status),
    Number_of_windows = factor(Number_of_windows),
    House_members_with_TB = factor(replace_na(House_members_with_TB, "NO")),
    Smokers = factor(replace_na(Smokers, "NO")),
    Vaccinated = factor(replace_na(Vaccinated, "NO")),
    Admitted_at_hospitals = factor(replace_na(Admitted_at_hospitals, "NO")),
    Educated_pop = factor(Educated_pop)) %>%
  droplevels()

### Función para generar tabla de una variable
crear_tabla_variable <- function(variable) {
  modelo <- glm(TB_Caso ~ get(variable), family = binomial, data = tuberc2)
  
  resultados <- tidy(modelo, exponentiate = TRUE, conf.int = TRUE) %>%
    select(term, estimate, conf.low, conf.high, p.value) %>%
    rename(
      Variable = term,
      OER = estimate,
      IC_Inf = conf.low,
      IC_Sup = conf.high,
      P_Value = p.value
    ) %>%
    mutate(
      # Redondeo y limitación de números grandes
      OER = round(ifelse(OER > 9999999, signif(OER, 7), OER), 3),
      IC_Inf = round(ifelse(IC_Inf > 9999999, signif(IC_Inf, 7), IC_Inf), 3),
      IC_Sup = round(ifelse(IC_Sup > 9999999, signif(IC_Sup, 7), IC_Sup), 3),
      P_Value = round(P_Value, 3),
      
      # Interpretación automática
      Interpretación = case_when(
        OER > 1 & P_Value < 0.05 ~ "Significative risk factor",
        OER > 1 & P_Value >= 0.05 ~ "Non significative risk factor",
        OER < 1 & P_Value < 0.05 ~ "Significative protector factor",
        OER < 1 & P_Value >= 0.05 ~ "Non significative protector factor",
        TRUE ~ "No clear association"
      )
    )
  
  ### Generar tabla profesional con `gt`
  tabla <- resultados %>%
    gt() %>%
    tab_header(
      title = paste("Odds Ratio Crudos (OER) -", variable),
      subtitle = "Modelo de regresión logística para tuberculosis"
    ) %>%
    cols_label(
      Variable = "Categoría",
      OER = "Odds Ratio (OER)",
      IC_Inf = "IC 95% - Inferior",
      IC_Sup = "IC 95% - Superior",
      P_Value = "Valor P",
      Interpretación = "Interpretación"
    ) %>%
    fmt_number(
      columns = c(OER, IC_Inf, IC_Sup, P_Value),
      decimals = 3
    ) %>%
    tab_options(
      table.font.size = 12,
      heading.title.font.size = 14,
      heading.subtitle.font.size = 12
    )
  
  return(tabla)
}

### Generar y mostrar cada tabla
tabla_socieconomico <- crear_tabla_variable("Socioeconomic_status")
tabla_ventanas <- crear_tabla_variable("Number_of_windows")
tabla_contacto <- crear_tabla_variable("House_members_with_TB")
tabla_fumar <- crear_tabla_variable("Smokers")
tabla_vacuna <- crear_tabla_variable("Vaccinated")
tabla_hospital <- crear_tabla_variable("Admitted_at_hospitals")
tabla_educacion <- crear_tabla_variable("Educated_pop")

### Mostrar cada tabla 
print(tabla_socieconomico)
print(tabla_ventanas)
print(tabla_contacto)
print(tabla_fumar)
print(tabla_vacuna)
print(tabla_hospital)
print(tabla_educacion)


## Emparejamiento de datos a evaluar (Entre los más significativos)
tuberc_match = matchit(TB_Caso ~ House_members_with_TB + Vaccinated + Socioeconomic_status + Smokers,
                     exact= ~ House_members_with_TB  + Vaccinated + Socioeconomic_status  + Smokers,
                     ratio=4,
                     data=tuberc2)

data_m = match.data(tuberc_match)

## Obtener OR ajustado
### Vacunados (Variable más significativa)
data_m %>%
  mutate(Vaccinated = fct_rev(Vaccinated)) %>%
  contingency(TB_Caso ~ Vaccinated,
              method = "case.control")

## Crear gráfico de mosaico con variable
tuberc2 %>% ggplot()+
  geom_mosaic(aes(x = product(Vaccinated), fill=TB_Caso), show.legend = FALSE) +
  theme_mosaic()+
  scale_fill_manual(values = c("#4575B4", "#ABD9E9"))

