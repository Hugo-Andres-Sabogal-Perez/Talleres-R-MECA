# Load the package
library(rsconnect)

# Set your account information
rsconnect::setAccountInfo(name='wjs11h-natalia-plata0angel',
                          token='B3A6C4794DE0803BD76A44485708323D',
                          secret='kEyIihfP5K/5pbLcwhCLCq2ujbF3Q1x1zP1bpeCH')
rsconnect::deployApp('Mapa_app')
