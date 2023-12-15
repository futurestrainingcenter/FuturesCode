import requests

# Replace with your API's insights endpoint URL
insights_url = "YOUR_INSIGHTS_API_URL"

# Your JWT token
jwt_token = "eyG0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.emJpc3MiOiJodHRwOlwvXC9hcGkud2kubG9jYWxcL2F1dGgiLCJzdWIiOiIxIiwiaWF0IjoxNDE2MjUyMDk2LCJliHAiOjE0MTYzMzg0OTZ9.HQirrQki1HePnsE-uPtFtW8taV8F50zbzLR-EVllnpo"

# Prepare the header with the JWT
headers = {
    "Authorization": f"Bearer {jwt_token}"
}

# Send the GET request
response = requests.get(insights_url, headers=headers)

# Check if the request was successful
if response.status_code == 200:
    # Print the JSON response
    print("Insights received:", response.json())
else:
    print("Failed to retrieve insights. Status code:", response.status_code)
