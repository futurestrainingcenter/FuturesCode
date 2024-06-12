import smtplib
import ssl
import os
import csv
from email.message import EmailMessage

# SMTP Server Information
sender = 'reports@futurestrainingcenter.com'
password = 'auqg qrto nrtw dddi'
smtp_server = 'smtp.gmail.com'
port = 465  # For SSL

# Email Content
subject = 'Your Futures Monthly Reports'
body = """
    <html>
<body>
    <p>Dear Futures Member,</p>
    <p>We are pleased to share with you your personalized training reports for the month of May. These reports provide detailed insights into your performance and progress. You will find them attached to this email.</p>
    <p>Should you have any questions about your report please do not hesitate to respond directly to this email. Our Data and Analytics Coordinator is ready to provide any assistance you may require.</p>
    <p>Thank you for your dedication and hard work. We look forward to supporting your ongoing development and achievements.</p>
</body>
</html>
"""

# Path to the folder containing all members' folders
base_folder_path = '/Users/watts/Documents/Futures Performance Center/Final Reports/Futures Reports'

# CSV file containing member names and emails
csv_file_path = '/Users/watts/Downloads/FullClientList.csv'

# Path to the log file
log_file_path = '/Users/watts/Downloads/email_logs.txt'

# Load members from CSV into a dictionary
members = {}
with open(csv_file_path, newline='') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        members[row['Client']] = row['Email']

# Read the log file to get a list of email addresses that have already been processed
processed_emails = set()
if os.path.exists(log_file_path):
    with open(log_file_path, 'r') as log_file:
        processed_emails = set(log_file.read().splitlines())

# Counter for the number of emails sent
emails_sent_count = 0

# Process each folder in the base directory
for member_folder in os.listdir(base_folder_path):
    folder_path = os.path.join(base_folder_path, member_folder)

    if os.path.isdir(folder_path) and member_folder in members:
        member_email = members[member_folder]

        if member_email not in processed_emails:
            # Create email message
            em = EmailMessage()
            em['From'] = sender
            em['To'] = member_email
            em['Subject'] = subject
            em.set_content(body, subtype='html')  # Sets the HTML content

            # Attach PDF files from the member's folder
            for file_name in os.listdir(folder_path):
                if file_name.endswith('.pdf'):
                    file_path = os.path.join(folder_path, file_name)
                    with open(file_path, 'rb') as file:
                        em.add_attachment(file.read(), maintype='application', subtype='pdf', filename=file_name)

            # Send email
            try:
                with smtplib.SMTP_SSL(smtp_server, port, context=ssl.create_default_context()) as smtp:
                    smtp.login(sender, password)
                    smtp.send_message(em)
                emails_sent_count += 1
                print(f'Email sent to {member_email}')

                # Update the log file with the email address
                with open(log_file_path, 'a') as log_file:
                    log_file.write(member_email + '\n')
            except Exception as e:
                print(f'Error sending email to {member_email}: {e}')
        else:
            print(f'Error: Email already sent to {member_email}, Skipping...')
    else:
        print(f'Error: No member found in CSV for folder "{member_folder}". Skipping...')

# Script completed
print(f'Email processing completed. Total emails sent: {emails_sent_count}')