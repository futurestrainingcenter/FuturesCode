import requests

# Replace with your API's authentication URL
auth_url = "https://api.blastconnect.com/auth"

# Replace with your actual username and password
username = "futurestraining"
password = "futurestraining"

# Prepare the payload with your credentials
payload = {
    "username": username,
    "password": password
}

# Send the POST request
response = requests.post(auth_url, json=payload)

# Check if the request was successful
if response.status_code == 200:
    # Extract the token from the response
    token = response.json().get("token")
    print("Authentication successful. Token received:", token)
else:
    print("Authentication failed. Status code:", response.status_code)

