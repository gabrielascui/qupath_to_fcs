# deployment
library(rsconnect)
library(BiocManager)
options(repos = BiocManager::repositories())
rsconnect::setAccountInfo(name='gabrielascui',
                          token='C3BA0F39539B8CF4BB4CAC78F5D1DB5C',
                          secret='FtVxF5qLAexKxX25waYoX7pR7Wdyh/S/kfKnxe83')
getOption("repos")
deployApp()
