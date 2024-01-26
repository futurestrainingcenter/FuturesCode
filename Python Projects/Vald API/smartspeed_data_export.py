import requests
import pandas as pd
from datetime import datetime
import json 

# Constants
CLIENT_ID = 'dmXoyydZ3mnt10A=='
CLIENT_SECRET = 'xLK8w6fPthKnCASTTaV2vM3J9MVvxS277roA='
AUTH_URL = 'https://security.valdperformance.com/connect/token'
API_BASE_URL = 'https://prd-use-api-extsmartspeed.valdperformance.com'
TEAM_ID = '9b534eef-b28d-403e-8f24-070741a544fb'
timestampOfLastRequest = '2000-1-01T00:00:00Z'

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

def get_test_data(access_token, team_id, last_request_timestamp):
    """Retrieve test data for a specified team with pagination."""
    headers = {'Authorization': f'Bearer {access_token}'}
    currentPage = 1
    totalData = []
    while True:
        response = requests.get(f'{API_BASE_URL}/v1/team/{team_id}/tests?ModifiedFromUTC={last_request_timestamp}&Page={currentPage}', headers=headers)
        response.raise_for_status()
        data = response.json()
        if not data:
            break
        totalData.extend(data)
        currentPage += 1

    timestampOfCurrentRequest = datetime.utcnow().isoformat() + 'Z'
    return totalData, timestampOfCurrentRequest

def create_csv(data, filename='smartspeed_data.csv'):
    """Create a CSV file from the data."""
    df = pd.DataFrame(data)

    # Function to handle expansion of JSON-like fields into separate columns
    def expand_json_like_fields(df, column_name):
        if column_name in df.columns and not df[column_name].isnull().all():
            # Parse the column assuming each cell is a dictionary
            expanded_cols = df[column_name].apply(pd.Series)

            # Merge the expanded columns into the original dataframe
            return pd.concat([df.drop([column_name], axis=1), expanded_cols], axis=1)
        return df

    # Expand 'runningSummaryFields' and 'velocityFields'
    df = expand_json_like_fields(df, 'runningSummaryFields')
    df = expand_json_like_fields(df, 'velocityFields')
    df = expand_json_like_fields(df, 'gateSummaryFields')
    df = expand_json_like_fields(df, 'fvpSummaryDto')

    df.to_csv(filename, index=False)
    print(f'CSV file created: {filename}')

def main():
    try:
        access_token = get_access_token()
        test_data, timestampOfCurrentRequest = get_test_data(access_token, TEAM_ID, timestampOfLastRequest)
        create_csv(test_data)
        # Update the timestampOfLastRequest with the timestampOfCurrentRequest for next run
    except requests.HTTPError as e:
        print(f'HTTP error occurred: {e}')
    except Exception as e:
        print(f'An error occurred: {e}')

if __name__ == '__main__':
    main()
