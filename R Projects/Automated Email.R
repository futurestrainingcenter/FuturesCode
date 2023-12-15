library(mailR)

player_data <- read.csv("/Users/watts/Documents/Futures Performance Center/Data/Automated Email Test - Sheet1.csv")

for(i in 1:nrow(player_data)) {
  
  send.mail(from="c13watt@gmail.com",
            to= player_data$Email[i],
            subject="Test Email",
            body="PFA the desired document",
            html=T,
            smtp=list(host.name = "smtp.gmail.com",
                      port = 465,
                      user.name = "c13watts@gmail.com",
                      passwd = "northcal123",
                      ssl = T),
            authenticate=T,
            attach.files=paste0("/Users/watts/Documents/Futures Performance Center/Hitting Reports/", player_data$Name[i], ".pdf"))
}