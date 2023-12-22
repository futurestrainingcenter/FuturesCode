import requests
import pandas as pd

# Constants
CLIENT_ID = 'dmXoyydZ3mnt10A=='
CLIENT_SECRET = 'xLK8w6fPthKnCASTTaV2vM3J9MVvxS277roA='
AUTH_URL = 'https://security.valdperformance.com/connect/token'
API_BASE_URL = 'https://prd-use-api-extsmartspeed.valdperformance.com'
TEAM_ID = '9b534eef-b28d-403e-8f24-070741a544fb'

def get_access_token():
    """Authenticate and get access token."""
    payload = {
        'grant_type': 'client_credentials',
        'client_id': CLIENT_ID,
        'client_secret': CLIENT_SECRET,
        'scope': 'api.external'
    }
    response = requests.post(AUTH_URL, data=payload)
    response.raise_for_status()
    return response.json()['access_token']

def get_test_data(access_token, team_id):
    """Retrieve test data for a specified team."""
    headers = {'Authorization': f'Bearer {access_token}'}
    response = requests.get(f'{API_BASE_URL}/v1/team/{team_id}/tests', headers=headers)
    response.raise_for_status()
    return response.json()

def create_csv(data, filename='test_data.csv'):
    """Create a CSV file from the data."""
    df = pd.DataFrame(data)
    df.to_csv(filename, index=False)
    print(f'CSV file created: {filename}')

def main():
    try:
        access_token = get_access_token()
        test_data = get_test_data(access_token, TEAM_ID)
        create_csv(test_data)
    except requests.HTTPError as e:
        print(f'HTTP error occurred: {e}')
    except Exception as e:
        print(f'An error occurred: {e}')

if __name__ == '__main__':
    main()
