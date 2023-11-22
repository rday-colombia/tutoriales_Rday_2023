
# Cargar paquetes 
packages <- c("plyr",
              "pROC",
              "questionr",
              "GoodmanKruskal")  

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, require, character.only = TRUE)  # Load multiple packages
