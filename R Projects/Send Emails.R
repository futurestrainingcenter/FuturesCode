library(mailR)

# Define email details
from <- "c13watts@gmail.com"
to <- "c13watts@gmail.com"
subject <- "Your Player Report"
body <- "Dear Player, please find your report attached."
# Send email
send.mail(from = from,
          to = to,
          subject = subject,
          body = body,
          smtp = list(host.name = "smtp.gmail.com", port = 465,
                      user.name = "c13watts@gmail.com",            
                      passwd = "northcal123", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          inline = FALSE,
          html = TRUE,
          encoding = "utf-8")
