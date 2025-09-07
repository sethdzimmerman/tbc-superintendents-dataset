import requests
from bs4 import BeautifulSoup
import pandas as pd
import time
from urllib.parse import urljoin
from waybackpy import WaybackMachineCDXServerAPI
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading
from datetime import datetime
import os
import numpy as np

#######################################################################################
# Functions
#######################################################################################

def scrape_district_data():
    """
    Scrape NY school district data from the NYSED website
    Returns a DataFrame with district names and URLs
    """
    base_url = "https://data.nysed.gov/lists.php"
    all_districts = []
    
    # Loop through ASCII values for A-Z (65-90)
    for start_value in range(65, 91):
        url = f"{base_url}?start={start_value}&type=district"
        letter = chr(start_value)
        
        print(f"Scraping districts starting with '{letter}' (start={start_value})...")
        
        try:
            # Add a small delay to be respectful to the server
            time.sleep(1)
            
            # Make the request
            response = requests.get(url)
            response.raise_for_status()  # Raise an exception for bad status codes
            
            # Parse the HTML
            soup = BeautifulSoup(response.content, 'html.parser')
            
            # Find the lists section
            lists_section = soup.find('section', {'class': 'lists'})
            
            if not lists_section:
                print(f"  Warning: No 'lists' section found for letter '{letter}'")
                continue
            
            # Find all district title divs within the lists section
            title_divs = lists_section.find_all('div', {'class': 'title'})
            
            districts_found = 0
            for title_div in title_divs:
                # Look for the link within the title div
                link = title_div.find('a', href=True)
                
                if link:
                    href = link.get('href')
                    district_name = link.get_text(strip=True)
                    
                    # Skip if no district name
                    if not district_name:
                        continue
                    
                    # Convert relative URLs to absolute URLs
                    if href.startswith('/'):
                        href = urljoin("https://data.nysed.gov", href)
                    elif not href.startswith('http'):
                        href = urljoin("https://data.nysed.gov/", href)
                    
                    # Store the district data
                    all_districts.append({
                        'district_name': district_name,
                        'url': href,
                        'letter': letter
                    })
                    districts_found += 1
            
            print(f"  Found {districts_found} districts for letter '{letter}'")
            
        except requests.RequestException as e:
            print(f"  Error fetching data for letter '{letter}': {e}")
            continue
        except Exception as e:
            print(f"  Unexpected error for letter '{letter}': {e}")
            continue
    
    # Create DataFrame
    df = pd.DataFrame(all_districts)
    
    if not df.empty:
        # Remove duplicates based on district name and URL
        df = df.drop_duplicates(subset=['district_name', 'url'])
        
        # Sort by district name
        df = df.sort_values('district_name').reset_index(drop=True)
        
        print(f"\nTotal districts found: {len(df)}")
    else:
        print("\nNo districts found. The website structure might have changed.")
    
    return df

def inspect_page_structure(sample_url=None):
    """
    Helper function to inspect the HTML structure of a sample page
    """
    if not sample_url:
        sample_url = "https://data.nysed.gov/lists.php?start=65&type=district"
    
    print(f"Inspecting page structure for: {sample_url}")
    
    try:
        response = requests.get(sample_url)
        response.raise_for_status()
        soup = BeautifulSoup(response.content, 'html.parser')
        
        print("\n--- Page Title ---")
        print(soup.title.get_text() if soup.title else "No title found")
        
        print("\n--- Looking for 'lists' section ---")
        lists_section = soup.find('section', {'class': 'lists'})
        if lists_section:
            print("Found 'lists' section")
            
            # Count title divs
            title_divs = lists_section.find_all('div', {'class': 'title'})
            print(f"Found {len(title_divs)} title divs in lists section")
            
            # Show first few district names as examples
            print("\n--- Sample Districts ---")
            for i, title_div in enumerate(title_divs[:5]):
                link = title_div.find('a', href=True)
                if link:
                    district_name = link.get_text(strip=True)
                    href = link.get('href')
                    print(f"  {i+1}. {district_name} | {href}")
        else:
            print("No 'lists' section found")
        
        print(f"\n--- All links on page (first 10) ---")
        all_links = soup.find_all('a', href=True)
        print(f"Found {len(all_links)} total links")
        
        # Show first few links as examples
        for i, link in enumerate(all_links[:10]):
            print(f"  {i+1}. Text: '{link.get_text(strip=True)[:50]}...' | Href: '{link.get('href')}'")
            
    except Exception as e:
        print(f"Error inspecting page: {e}")

def get_wayback_snapshots(districts_df, start_year=2017, end_year=None, delay_between_requests=5, batch_size=10, batch_delay=60):
    """
    Get all Wayback Machine snapshots for district URLs with conservative rate limiting
    
    Args:
        districts_df: DataFrame with district_name and url columns
        start_year: Start year for snapshot search (default: 2017)
        end_year: End year for snapshot search (default: current year)
        delay_between_requests: Delay between API requests in seconds (default: 3)
        batch_size: Number of districts before taking a break (default: 5)
        batch_delay: Delay in seconds between batches (default: 60)
    
    Returns:
        DataFrame with columns: district_name, original_url, snapshot_timestamp, snapshot_url, snapshot_date, year
    """
    if end_year is None:
        end_year = datetime.now().year
    
    print(f"Getting Wayback Machine snapshots from {start_year} to {end_year}...")
    
    all_snapshots = []
    total_districts = len(districts_df)
    
    for idx, row in districts_df.iterrows():
        district_name = row['district_name']
        original_url = row['url']
        
        # Convert relative URL to absolute if needed
        if not original_url.startswith('http'):
            if original_url.startswith('/'):
                original_url = f"https://data.nysed.gov{original_url}"
            else:
                original_url = f"https://data.nysed.gov/{original_url}"
        
        print(f"Processing {idx + 1}/{total_districts}: {district_name}")
        
        try:
            # Create CDX API object
            cdx = WaybackMachineCDXServerAPI(original_url)
            
            # Get snapshots
            snapshots = cdx.snapshots()
            
            # Filter by year range
            filtered_snapshots = []
            for snapshot in snapshots:
                try:
                    snapshot_year = int(snapshot.timestamp[:4])  # Extract year from timestamp
                    if start_year <= snapshot_year <= end_year:
                        filtered_snapshots.append(snapshot)
                except (ValueError, AttributeError):
                    continue
            
            print(f"  Found {len(filtered_snapshots)} snapshots for {district_name}")
            
            # Add snapshots to our list
            for snapshot in filtered_snapshots:
                all_snapshots.append({
                    'district_name': district_name,
                    'original_url': original_url,
                    'snapshot_timestamp': snapshot.timestamp,
                    'snapshot_url': snapshot.archive_url,
                    'snapshot_date': datetime.strptime(snapshot.timestamp, '%Y%m%d%H%M%S').strftime('%Y-%m-%d %H:%M:%S'),
                    'year': int(snapshot.timestamp[:4])
                })
            
            # Be respectful to the API
            time.sleep(1)
            
        except Exception as e:
            print(f"  Error getting snapshots for {district_name}: {str(e)}")
            continue
    
    # Create DataFrame
    snapshots_df = pd.DataFrame(all_snapshots)
    
    if not snapshots_df.empty:
        # Sort by district name, year, and timestamp
        snapshots_df = snapshots_df.sort_values(['district_name', 'year', 'snapshot_timestamp']).reset_index(drop=True)
        
        print(f"\n=== ALL SNAPSHOTS SUMMARY ===")
        print(f"Total snapshots found: {len(snapshots_df)}")
        print(f"Districts with snapshots: {snapshots_df['district_name'].nunique()}")
        print(f"Date range: {snapshots_df['snapshot_date'].min()} to {snapshots_df['snapshot_date'].max()}")
        
        # Show snapshots per year
        yearly_counts = snapshots_df['year'].value_counts().sort_index()
        print(f"\nSnapshots per year:")
        for year, count in yearly_counts.items():
            print(f"  {year}: {count}")
    else:
        print("\nNo snapshots found.")
    
    return snapshots_df

def filter_to_last_snapshot_per_year(snapshots_df):
    """
    Filter snapshots DataFrame to keep only the last snapshot per district per year
    
    Args:
        snapshots_df: DataFrame from get_wayback_snapshots()
    
    Returns:
        DataFrame with one snapshot per district per year (the latest one)
    """
    if snapshots_df.empty:
        return snapshots_df
    
    print("Filtering to last snapshot per district per year...")
    
    # Fix data types - ensure snapshot_timestamp is string for consistent sorting
    snapshots_df_clean = snapshots_df.copy()
    snapshots_df_clean['snapshot_timestamp'] = snapshots_df_clean['snapshot_timestamp'].astype(str)
    
    # Also ensure year is integer
    snapshots_df_clean['year'] = snapshots_df_clean['year'].astype(int)
    
    print(f"Data types after cleaning:")
    print(f"  snapshot_timestamp: {snapshots_df_clean['snapshot_timestamp'].dtype}")
    print(f"  year: {snapshots_df_clean['year'].dtype}")
    
    # Group by district and year, then take the row with the max timestamp
    filtered_df = (snapshots_df_clean
                .sort_values('snapshot_timestamp')  # Sort by timestamp first
                .groupby(['district_name', 'year'])
                .last()  # Take the last (most recent) snapshot for each group
                .reset_index())
    
    print(f"Filtered from {len(snapshots_df)} total snapshots to {len(filtered_df)} snapshots")
    print(f"(one per district per year)")
    
    # Show summary
    if not filtered_df.empty:
        yearly_counts = filtered_df['year'].value_counts().sort_index()
        print(f"\nFiltered snapshots per year:")
        for year, count in yearly_counts.items():
            print(f"  {year}: {count}")
    
    return filtered_df

# Example usage functions
def get_snapshots_for_sample_districts(districts_df, sample_size=5):
    """
    Get snapshots for a small sample of districts (for testing)
    """
    if len(districts_df) == 0:
        print("No districts data available")
        return pd.DataFrame()
    
    sample_df = districts_df.head(sample_size)
    print(f"Getting snapshots for {sample_size} sample districts...")
    return get_wayback_snapshots(sample_df)

def get_snapshots_with_progress_saving(districts_df, save_every=10, output_file="/Users/aeg88/Desktop/snapshots_progress.csv", 
                                    start_year=2017, end_year=None, delay_between_requests=5):
    """
    Get snapshots with automatic progress saving - won't lose data if interrupted
    
    Args:
        districts_df: DataFrame with district data
        save_every: Save progress every N districts (default: 10)
        output_file: File to save progress to (default: "snapshots_progress.csv")
        start_year: Start year for snapshots
        end_year: End year for snapshots
        delay_between_requests: Delay between requests
    
    Returns:
        DataFrame with all snapshots
    """
    if end_year is None:
        end_year = datetime.now().year
    
    print(f"Processing {len(districts_df)} districts with progress saving")
    print(f"Saving progress every {save_every} districts to '{output_file}'")
    print("You can safely interrupt and resume from where you left off!")
    
    # Check if progress file already exists
    all_snapshots = []
    processed_districts = set()
    start_idx = 0
    
    try:
        if pd.io.common.file_exists(output_file):
            print(f"Found existing progress file: {output_file}")
            existing_df = pd.read_csv(output_file)
            all_snapshots = existing_df.to_dict('records')
            processed_districts = set(existing_df['district_name'].unique())
            start_idx = len(processed_districts)
            print(f"Resuming from district {start_idx + 1} (already processed {len(processed_districts)} districts)")
    except Exception as e:
        print(f"Could not load existing progress file: {e}")
        print("Starting from the beginning...")
    
    try:
        for idx, row in districts_df.iterrows():
            # Skip already processed districts
            if row['district_name'] in processed_districts:
                continue
                
            district_name = row['district_name']
            original_url = row['url']
            
            # Convert relative URL to absolute if needed
            if not original_url.startswith('http'):
                if original_url.startswith('/'):
                    original_url = f"https://data.nysed.gov{original_url}"
                else:
                    original_url = f"https://data.nysed.gov/{original_url}"
            
            print(f"Processing {idx + 1}/{len(districts_df)}: {district_name}")
            
            try:
                time.sleep(delay_between_requests)
                
                cdx = WaybackMachineCDXServerAPI(original_url)
                snapshots = cdx.snapshots()
                
                district_snapshots = 0
                for snapshot in snapshots:
                    try:
                        snapshot_year = int(snapshot.timestamp[:4])
                        if start_year <= snapshot_year <= end_year:
                            all_snapshots.append({
                                'district_name': district_name,
                                'original_url': original_url,
                                'snapshot_timestamp': snapshot.timestamp,
                                'snapshot_url': snapshot.archive_url,
                                'snapshot_date': datetime.strptime(snapshot.timestamp, '%Y%m%d%H%M%S').strftime('%Y-%m-%d %H:%M:%S'),
                                'year': int(snapshot.timestamp[:4])
                            })
                            district_snapshots += 1
                    except (ValueError, AttributeError):
                        continue
                
                processed_districts.add(district_name)
                print(f"  Found {district_snapshots} snapshots")
                
                # Save progress every N districts
                if len(processed_districts) % save_every == 0:
                    temp_df = pd.DataFrame(all_snapshots)
                    temp_df.to_csv(output_file, index=False)
                    print(f"  ✓ Progress saved ({len(processed_districts)} districts completed)")
                
            except Exception as e:
                print(f"  Error processing {district_name}: {str(e)}")
                continue
    
    except KeyboardInterrupt:
        print(f"\n⚠️  Process interrupted by user!")
        print(f"Saving progress for {len(processed_districts)} completed districts...")
        
        # Save what we have so far
        if all_snapshots:
            temp_df = pd.DataFrame(all_snapshots)
            temp_df.to_csv(output_file, index=False)
            print(f"✓ Progress saved to '{output_file}'")
            print(f"To resume, run the function again - it will automatically continue from where you left off")
            return temp_df
        else:
            print("No data to save")
            return pd.DataFrame()
    
    # Final save
    if all_snapshots:
        final_df = pd.DataFrame(all_snapshots)
        final_df = final_df.sort_values(['district_name', 'year', 'snapshot_timestamp']).reset_index(drop=True)
        final_df.to_csv(output_file, index=False)
        
        print(f"\n=== FINAL SUMMARY ===")
        print(f"Total snapshots found: {len(final_df)}")
        print(f"Districts processed: {len(processed_districts)}")
        print(f"Results saved to: {output_file}")
        
        return final_df
    else:
        return pd.DataFrame()

def resume_snapshot_collection(progress_file="snapshots_progress.csv"):
    """
    Resume snapshot collection from a saved progress file
    
    Args:
        progress_file: Path to the progress CSV file
    
    Returns:
        DataFrame with current progress
    """
    try:
        df = pd.read_csv(progress_file)
        print(f"Loaded {len(df)} snapshots from {progress_file}")
        print(f"Districts completed: {df['district_name'].nunique()}")
        print(f"Date range: {df['snapshot_date'].min()} to {df['snapshot_date'].max()}")
        return df
    except FileNotFoundError:
        print(f"Progress file '{progress_file}' not found")
        return pd.DataFrame()
    except Exception as e:
        print(f"Error loading progress file: {e}")
        return pd.DataFrame()

# scrape for superintendent name 
def retry_failed_scrapes(results_df, retry_attempts=3, delay_between_requests=5):
    """
    Retry scraping for failed snapshots with longer delays
    
    Args:
        results_df: DataFrame from scrape_district_details_from_snapshots()
        retry_attempts: Number of retry attempts (default: 3)
        delay_between_requests: Delay between requests in seconds (default: 5)
    
    Returns:
        Updated DataFrame with retry results
    """
    failed_df = results_df[results_df['scrape_success'] == False].copy()
    
    if len(failed_df) == 0:
        print("No failed scrapes to retry!")
        return results_df
    
    print(f"Retrying {len(failed_df)} failed scrapes with longer delays...")
    
    # Create snapshots DataFrame for retry
    retry_snapshots = failed_df[['district_name', 'original_url', 'snapshot_timestamp', 
                                'snapshot_url', 'snapshot_date', 'year']].copy()
    
    # Retry scraping with longer delays
    retry_results = scrape_district_details_from_snapshots(
        retry_snapshots, 
        retry_attempts=retry_attempts,
        delay_between_requests=delay_between_requests
    )
    
    # Update original DataFrame with retry results
    updated_results = results_df.copy()
    for idx, retry_row in retry_results.iterrows():
        # Find matching row in original DataFrame
        mask = ((updated_results['district_name'] == retry_row['district_name']) & 
                (updated_results['year'] == retry_row['year']))
        
        if mask.any():
            # Update the row with retry results
            updated_results.loc[mask, ['superintendent', 'legal_name', 'beds_code', 
                                    'institution_id', 'scrape_success', 'scrape_error']] = [
                retry_row['superintendent'], retry_row['legal_name'], retry_row['beds_code'],
                retry_row['institution_id'], retry_row['scrape_success'], retry_row['scrape_error']
            ]
    
    return updated_results

# def scrape_district_details_from_snapshots(snapshots_df, delay_between_requests=5, batch_size=10, batch_delay=60):
#     """
#     Scrape district details with very conservative rate limiting to avoid blocks
    
#     Args:
#         snapshots_df: DataFrame with snapshot URLs
#         delay_between_requests: Delay in seconds between individual requests (default: 5)
#         batch_size: Number of requests before taking a longer break (default: 10)
#         batch_delay: Delay in seconds between batches (default: 60)
    
#     Returns:
#         DataFrame with original snapshot data plus scraped details
#     """
#     print(f"Scraping district details from {len(snapshots_df)} snapshots...")
#     print(f"Using conservative rate limiting:")
#     print(f"  - {delay_between_requests}s between requests")
#     print(f"  - {batch_delay}s break every {batch_size} requests")
#     print(f"  - Estimated total time: {len(snapshots_df) * delay_between_requests + (len(snapshots_df) // batch_size) * batch_delay:.0f} seconds")
    
#     results = []
#     total_snapshots = len(snapshots_df)
    
#     for idx, row in snapshots_df.iterrows():
#         district_name = row['district_name']
#         snapshot_url = row['snapshot_url']
        
#         print(f"Processing {idx + 1}/{total_snapshots}: {district_name} ({row['year']})")
        
#         # Take a longer break every batch_size requests
#         if idx > 0 and idx % batch_size == 0:
#             print(f"  Taking {batch_delay}s break after {batch_size} requests...")
#             time.sleep(batch_delay)
        
#         # Initialize variables
#         superintendent = None
#         legal_name = None
#         beds_code = None
#         institution_id = None
#         scrape_success = False
#         scrape_error = None
        
#         try:
#             # Conservative delay before each request
#             time.sleep(delay_between_requests)
            
#             # Use headers that mimic a real browser
#             headers = {
#                 'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
#                 'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
#                 'Accept-Language': 'en-US,en;q=0.5',
#                 'Accept-Encoding': 'gzip, deflate',
#                 'Connection': 'keep-alive'
#             }
            
#             response = requests.get(snapshot_url, timeout=30, headers=headers)
#             response.raise_for_status()
            
#             # Parse HTML
#             soup = BeautifulSoup(response.content, 'html.parser')
            
#             # Find the contact-details div
#             contact_div = soup.find('div', {'class': 'contact-details'})
            
#             if contact_div:
#                 # Extract superintendent from h4 tag
#                 h4_tag = contact_div.find('h4')
#                 if h4_tag:
#                     superintendent_text = h4_tag.get_text(strip=True)
#                     # Extract name after "SUPERINTENDENT: "
#                     if 'SUPERINTENDENT:' in superintendent_text:
#                         superintendent = superintendent_text.replace('SUPERINTENDENT:', '').strip()
                
#                 # Extract other details from list items
#                 li_tags = contact_div.find_all('li')
#                 for li in li_tags:
#                     li_text = li.get_text(strip=True)
                    
#                     if li_text.startswith('LEGAL NAME:'):
#                         legal_name = li_text.replace('LEGAL NAME:', '').strip()
#                     elif li_text.startswith('BEDS CODE:'):
#                         beds_code = li_text.replace('BEDS CODE:', '').strip()
#                     elif li_text.startswith('INSTITUTION ID:'):
#                         institution_id = li_text.replace('INSTITUTION ID:', '').strip()
            
#             scrape_success = True
#             print(f"  ✓ Scraped: {superintendent or 'N/A'} | {legal_name or 'N/A'}")
            
#         except requests.exceptions.ConnectionError as e:
#             scrape_error = f"Connection refused - likely rate limited: {str(e)}"
#             print(f"  ✗ Connection refused (rate limit?)")
            
#         except requests.exceptions.Timeout as e:
#             scrape_error = f"Timeout error: {str(e)}"
#             print(f"  ✗ Timeout error")
            
#         except Exception as e:
#             scrape_error = f"Other error: {str(e)}"
#             print(f"  ✗ Error: {str(e)}")
        
#         # Add result to list
#         result_row = row.to_dict()
#         result_row.update({
#             'superintendent': superintendent,
#             'legal_name': legal_name,
#             'beds_code': beds_code,
#             'institution_id': institution_id,
#             'scrape_success': scrape_success,
#             'scrape_error': scrape_error
#         })
        
#         results.append(result_row)
        
#         # If we're getting rate limited, suggest stopping
#         if not scrape_success and 'Connection refused' in str(scrape_error):
#             consecutive_failures = sum(1 for r in results[-5:] if not r.get('scrape_success', False))
#             if consecutive_failures >= 3:
#                 print(f"\n⚠️  Warning: {consecutive_failures} consecutive connection failures.")
#                 print("This suggests you've been rate limited by the Wayback Machine.")
#                 print("Consider stopping and resuming later, or using longer delays.")
                
#                 response = input("Continue anyway? (y/n): ").lower()
#                 if response != 'y':
#                     print("Stopping scraping due to rate limiting.")
#                     break
    
#     # Create final DataFrame
#     results_df = pd.DataFrame(results)
    
#     # Show summary
#     if not results_df.empty:
#         successful_scrapes = results_df['scrape_success'].sum()
#         print(f"\n=== SCRAPING SUMMARY ===")
#         print(f"Total snapshots processed: {len(results_df)}")
#         print(f"Successful scrapes: {successful_scrapes}")
#         print(f"Failed scrapes: {len(results_df) - successful_scrapes}")
        
#         # Show error breakdown
#         if len(results_df) - successful_scrapes > 0:
#             error_df = results_df[results_df['scrape_success'] == False]
#             error_types = error_df['scrape_error'].apply(lambda x: x.split(':')[0] if x else 'Unknown').value_counts()
#             print(f"\nError breakdown:")
#             for error_type, count in error_types.items():
#                 print(f"  {error_type}: {count}")
        
#         # Show data availability
#         print(f"\nData availability:")
#         print(f"  Superintendent: {results_df['superintendent'].notna().sum()}")
#         print(f"  Legal name: {results_df['legal_name'].notna().sum()}")
#         print(f"  BEDS code: {results_df['beds_code'].notna().sum()}")
#         print(f"  Institution ID: {results_df['institution_id'].notna().sum()}")
    
#     return results_df

# def scrape_in_small_batches(snapshots_df, batch_size=5, batch_delay=300):
#     """
#     Scrape snapshots in very small batches with long delays to avoid rate limiting
    
#     Args:
#         snapshots_df: DataFrame with snapshot URLs
#         batch_size: Number of snapshots per batch (default: 5)
#         batch_delay: Delay in seconds between batches (default: 300 = 5 minutes)
    
#     Returns:
#         DataFrame with scraped results
#     """
#     print(f"Scraping {len(snapshots_df)} snapshots in batches of {batch_size}")
#     print(f"With {batch_delay/60:.1f} minute breaks between batches")
    
#     all_results = []
#     total_batches = (len(snapshots_df) + batch_size - 1) // batch_size
    
#     for batch_num in range(total_batches):
#         start_idx = batch_num * batch_size
#         end_idx = min(start_idx + batch_size, len(snapshots_df))
#         batch_df = snapshots_df.iloc[start_idx:end_idx].copy()
        
#         print(f"\n=== BATCH {batch_num + 1}/{total_batches} ===")
#         print(f"Processing snapshots {start_idx + 1} to {end_idx}")
        
#         # Scrape this batch with very conservative settings
#         batch_results = scrape_district_details_from_snapshots(
#             batch_df,
#             delay_between_requests=10,  # 10 seconds between requests
#             batch_size=batch_size,     # No sub-batching within the batch
#             batch_delay=0              # No sub-batch delays
#         )
        
#         all_results.append(batch_results)
        
#         # Long delay between batches (except for the last batch)
#         if batch_num < total_batches - 1:
#             print(f"\nWaiting {batch_delay/60:.1f} minutes before next batch...")
#             time.sleep(batch_delay)
    
#     # Combine all batch results
#     if all_results:
#         final_results = pd.concat(all_results, ignore_index=True)
        
#         print(f"\n=== FINAL SUMMARY ===")
#         successful_scrapes = final_results['scrape_success'].sum()
#         print(f"Total snapshots processed: {len(final_results)}")
#         print(f"Successful scrapes: {successful_scrapes}")
#         print(f"Success rate: {successful_scrapes/len(final_results)*100:.1f}%")
        
#         return final_results
#     else:
#         return pd.DataFrame()


def scrape_district_details_from_snapshots(snapshots_df, delay_between_requests=5, batch_size=10, batch_delay=120, 
                                        max_workers=3, backup_frequency=50, backup_dir="/Users/aeg88/Desktop/scrape_backups"):
    """
    Scrape district details with conservative rate limiting, parallel processing, and automatic backups
    
    Args:
        snapshots_df: DataFrame with snapshot URLs
        delay_between_requests: Delay in seconds between individual requests (default: 5)
        batch_size: Number of requests before taking a longer break (default: 10)
        batch_delay: Delay in seconds between batches (default: 60)
        max_workers: Number of parallel threads (default: 3)
        backup_frequency: Save backup CSV every N completed rows (default: 50)
        backup_dir: Directory to save backup files (default: "scrape_backups")
    
    Returns:
        DataFrame with original snapshot data plus scraped details
    """
    # Create backup directory
    os.makedirs(backup_dir, exist_ok=True)
    
    # Calculate timing with parallelization
    total_requests = len(snapshots_df)
    estimated_time_per_worker = (total_requests / max_workers) * delay_between_requests
    estimated_batch_delays = (total_requests // batch_size) * batch_delay
    estimated_total_time = estimated_time_per_worker + estimated_batch_delays
    
    print(f"Scraping district details from {total_requests} snapshots...")
    print(f"Using conservative rate limiting with parallelization:")
    print(f"  - {delay_between_requests}s between requests per worker")
    print(f"  - {batch_delay}s break every {batch_size} requests globally")
    print(f"  - {max_workers} parallel workers")
    print(f"  - Backup every {backup_frequency} completed rows")
    print(f"  - Estimated total time: {estimated_total_time:.0f} seconds")
    
    # Shared state for rate limiting and progress tracking
    class SharedState:
        def __init__(self):
            self.lock = threading.Lock()
            self.request_count = 0
            self.last_batch_time = time.time()
            self.results = []
            self.completed_count = 0
            
    state = SharedState()
    
    def scrape_single_district(row_data):
        """Scrape a single district with rate limiting"""
        idx, row = row_data
        district_name = row['district_name']
        snapshot_url = row['snapshot_url']
        
        # Initialize variables
        superintendent = None
        legal_name = None
        beds_code = None
        institution_id = None
        scrape_success = False
        scrape_error = None
        
        try:
            # Rate limiting logic
            with state.lock:
                state.request_count += 1
                current_request_num = state.request_count
                
                # Check if we need a batch delay
                if current_request_num > 1 and current_request_num % batch_size == 1:
                    batch_delay_needed = batch_delay - (time.time() - state.last_batch_time)
                    if batch_delay_needed > 0:
                        print(f"  Taking {batch_delay_needed:.1f}s break after {batch_size} requests...")
                        time.sleep(batch_delay_needed)
                    state.last_batch_time = time.time()
            
            # Individual request delay
            time.sleep(delay_between_requests)
            
            print(f"Processing {idx + 1}/{total_requests}: {district_name} ({row['year']}) [Worker {threading.current_thread().name}]")
            
            # Use headers that mimic a real browser
            headers = {
                'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
                'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
                'Accept-Language': 'en-US,en;q=0.5',
                'Accept-Encoding': 'gzip, deflate',
                'Connection': 'keep-alive'
            }
            
            response = requests.get(snapshot_url, timeout=30, headers=headers)
            response.raise_for_status()
            
            # Parse HTML
            soup = BeautifulSoup(response.content, 'html.parser')
            
            # Find the contact-details div
            contact_div = soup.find('div', {'class': 'contact-details'})
            
            if contact_div:
                # Extract superintendent from h4 tag
                h4_tag = contact_div.find('h4')
                if h4_tag:
                    superintendent_text = h4_tag.get_text(strip=True)
                    # Extract name after "SUPERINTENDENT: "
                    if 'SUPERINTENDENT:' in superintendent_text:
                        superintendent = superintendent_text.replace('SUPERINTENDENT:', '').strip()
                
                # Extract other details from list items
                li_tags = contact_div.find_all('li')
                for li in li_tags:
                    li_text = li.get_text(strip=True)
                    
                    if li_text.startswith('LEGAL NAME:'):
                        legal_name = li_text.replace('LEGAL NAME:', '').strip()
                    elif li_text.startswith('BEDS CODE:'):
                        beds_code = li_text.replace('BEDS CODE:', '').strip()
                    elif li_text.startswith('INSTITUTION ID:'):
                        institution_id = li_text.replace('INSTITUTION ID:', '').strip()
            
            scrape_success = True
            print(f"  ✓ Scraped: {superintendent or 'N/A'} | {legal_name or 'N/A'} [Worker {threading.current_thread().name}]")
            
        except requests.exceptions.ConnectionError as e:
            scrape_error = f"Connection refused - likely rate limited: {str(e)}"
            print(f"  ✗ Connection refused (rate limit?) [Worker {threading.current_thread().name}]")
            
        except requests.exceptions.Timeout as e:
            scrape_error = f"Timeout error: {str(e)}"
            print(f"  ✗ Timeout error [Worker {threading.current_thread().name}]")
            
        except Exception as e:
            scrape_error = f"Other error: {str(e)}"
            print(f"  ✗ Error: {str(e)} [Worker {threading.current_thread().name}]")
        
        # Create result row
        result_row = row.to_dict()
        result_row.update({
            'superintendent': superintendent,
            'legal_name': legal_name,
            'beds_code': beds_code,
            'institution_id': institution_id,
            'scrape_success': scrape_success,
            'scrape_error': scrape_error,
            'processing_order': idx
        })
        
        return result_row, scrape_success, scrape_error
    
    def save_backup(results_list, backup_num):
        """Save backup CSV file"""
        if results_list:
            backup_df = pd.DataFrame(results_list)
            backup_df = backup_df.sort_values('processing_order')  # Maintain original order
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            backup_filename = f"{backup_dir}/scrape_backup_{backup_num}_{timestamp}.csv"
            backup_df.to_csv(backup_filename, index=False)
            print(f"  💾 Backup saved: {backup_filename} ({len(results_list)} rows)")
    
    # Execute parallel scraping
    results = []
    consecutive_failures = 0
    backup_count = 0
    
    with ThreadPoolExecutor(max_workers=max_workers, thread_name_prefix="Scraper") as executor:
        # Submit all tasks
        future_to_data = {
            executor.submit(scrape_single_district, (idx, row)): (idx, row) 
            for idx, row in snapshots_df.iterrows()
        }
        
        # Process completed tasks
        for future in as_completed(future_to_data):
            try:
                result_row, scrape_success, scrape_error = future.result()
                results.append(result_row)
                
                with state.lock:
                    state.completed_count += 1
                    current_completed = state.completed_count
                
                # Track consecutive failures for rate limiting detection
                if not scrape_success and 'Connection refused' in str(scrape_error):
                    consecutive_failures += 1
                else:
                    consecutive_failures = 0
                
                # Check for rate limiting
                if consecutive_failures >= 3:
                    print(f"\n⚠️  Warning: {consecutive_failures} consecutive connection failures.")
                    print("This suggests you've been rate limited by the Wayback Machine.")
                    print("Consider stopping and resuming later, or using longer delays.")
                    
                    # Cancel remaining futures
                    for remaining_future in future_to_data:
                        if not remaining_future.done():
                            remaining_future.cancel()
                    break
                
                # Save backup every backup_frequency completed rows
                if current_completed % backup_frequency == 0:
                    backup_count += 1
                    save_backup(results, backup_count)
                
            except Exception as e:
                idx, row = future_to_data[future]
                print(f"  ✗ Task failed for {row['district_name']}: {str(e)}")
                
                # Add failed result
                result_row = row.to_dict()
                result_row.update({
                    'superintendent': None,
                    'legal_name': None,
                    'beds_code': None,
                    'institution_id': None,
                    'scrape_success': False,
                    'scrape_error': f"Task execution error: {str(e)}",
                    'processing_order': idx
                })
                results.append(result_row)
                
                with state.lock:
                    state.completed_count += 1
    
    # Final backup if there are remaining results
    if len(results) % backup_frequency != 0:
        backup_count += 1
        save_backup(results, backup_count)
    
    # Create final DataFrame
    results_df = pd.DataFrame(results)
    
    # Sort by original order to maintain consistency
    if not results_df.empty:
        results_df = results_df.sort_values('processing_order').drop('processing_order', axis=1)
        
        # Show summary
        successful_scrapes = results_df['scrape_success'].sum()
        print(f"\n=== SCRAPING SUMMARY ===")
        print(f"Total snapshots processed: {len(results_df)}")
        print(f"Successful scrapes: {successful_scrapes}")
        print(f"Failed scrapes: {len(results_df) - successful_scrapes}")
        print(f"Backup files created: {backup_count}")
        
        # Show error breakdown
        if len(results_df) - successful_scrapes > 0:
            error_df = results_df[results_df['scrape_success'] == False]
            error_types = error_df['scrape_error'].apply(lambda x: x.split(':')[0] if x else 'Unknown').value_counts()
            print(f"\nError breakdown:")
            for error_type, count in error_types.items():
                print(f"  {error_type}: {count}")
        
        # Show data availability
        print(f"\nData availability:")
        print(f"  Superintendent: {results_df['superintendent'].notna().sum()}")
        print(f"  Legal name: {results_df['legal_name'].notna().sum()}")
        print(f"  BEDS code: {results_df['beds_code'].notna().sum()}")
        print(f"  Institution ID: {results_df['institution_id'].notna().sum()}")
    
    return results_df


#######################################################################################
# run functions 
#######################################################################################

# First, get the districts data (as before)
districts_df = scrape_district_data()

# Option 1: Test with a small sample first (recommended)
sample_snapshots = get_snapshots_for_sample_districts(districts_df, sample_size=5)

all_snapshots = get_snapshots_with_progress_saving(districts_df)



# Find districts that are in districts_df but not in all_snapshots
districts_in_df = set(districts_df['district_name'].unique())
districts_in_snapshots = set(all_snapshots['district_name'].unique()) if not all_snapshots.empty else set()

missing_districts = districts_in_df - districts_in_snapshots

# Create DataFrame with missing districts (includes all original columns)
missing_districts_df = districts_df[districts_df['district_name'].isin(missing_districts)].copy()

all_snapshots_miss = get_snapshots_with_progress_saving(missing_districts_df)


filtered_snapshots = filter_to_last_snapshot_per_year(all_snapshots_miss)
filtered_snapshots.to_csv('/Users/aeg88/Desktop/ny_school_district_snapshots.csv', index=False)

scraped_supers = scrape_district_details_from_snapshots(filtered_snapshots)

#######################################################################################
# Get failed runs and re-run scrape on failed attempts 
#######################################################################################

failed_scrapes = scraped_supers[scraped_supers['scrape_error'].notna()]

failed_scrapes = failed_scrapes.iloc[:, :-6]

scraped_supers2 = scrape_district_details_from_snapshots(failed_scrapes)

#######################################################################################
# Combine two scaped datasets and clean
#######################################################################################

scrapes_1 = scraped_supers[scraped_supers['scrape_error'].isna()]

scraped_supers2.loc[0, 'superintendent'] = 'MICHAEL HOOSE'
scraped_supers2.loc[0, 'legal_name'] = 'CORTLAND CITY SCHOOL DISTRICT'
scraped_supers2.loc[0, 'beds_code'] = '110200010000'
scraped_supers2.loc[0, 'institution_id'] = '800000053659'
scraped_supers2.loc[0, 'scrape_success'] = True
scraped_supers2.loc[0, 'scrape_error'] = None


# append 
all_supers_scraped = pd.concat([scrapes_1, scraped_supers2], ignore_index=True)

# clean supt name 
all_supers_scraped['superintendent'] = all_supers_scraped['superintendent'].str.lstrip('ACTING ')

all_supers_scraped = all_supers_scraped[all_supers_scraped['superintendent'].notna()]

replacements = {
    'ODD': 'T',
    'MY': 'A',
    'HARL': 'C',
    'HERYL': 'C',
    'DREA': 'AN',
    'DY': 'AN',
    'YNTHIA': 'C',
    'DAM': 'A'
}

# Loop through each replacement
for start_string, addition in replacements.items():
    all_supers_scraped['superintendent'] = np.where(
        all_supers_scraped['superintendent'].str.startswith(start_string, na=False),
        addition + all_supers_scraped['superintendent'],
        all_supers_scraped['superintendent']
    )


# Export 
all_supers_scraped.to_csv('../data/raw/ny/ny_scraped_supers.csv', index=False)

