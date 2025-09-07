from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, NoSuchElementException
import os
import time
import requests


def scrape_district_pdfs(url, output_folder):
    """
    Scrapes a website for PDFs by parsing the table structure with a direct approach.
    
    Args:
        url (str): The URL of the webpage containing the table
        output_folder (str): The folder where PDFs will be saved
    """
    # Create output folder if it doesn't exist
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)
        print(f"Created output folder: {output_folder}")
    
    # Configure Chrome options
    chrome_options = Options()
    #chrome_options.add_argument("--headless")  # Run in headless mode (no GUI)
    chrome_options.add_argument("--disable-gpu")
    chrome_options.add_argument("--window-size=1920,1080")
    chrome_options.add_argument("--disable-extensions")
    chrome_options.add_argument("--no-sandbox")
    chrome_options.add_argument("--disable-dev-shm-usage")
    
    # Add user-agent to appear more like a regular browser
    chrome_options.add_argument("user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.212 Safari/537.36")
    
    print("Starting Chrome WebDriver...")
    driver = webdriver.Chrome(options=chrome_options)
    
    try:
        print(f"Navigating to URL: {url}")
        driver.get(url)
        
        # Wait for page to load
        wait = WebDriverWait(driver, 20)
        wait.until(EC.presence_of_element_located((By.TAG_NAME, "body")))
        
        # Give the page more time to fully render
        time.sleep(5)
        
        print("Page loaded, looking for frames/iframes...")
        
        # Find all frames and iframes
        frames = driver.find_elements(By.TAG_NAME, "frame")
        iframes = driver.find_elements(By.TAG_NAME, "iframe")
        all_frames = frames + iframes
        print(f"Found {len(all_frames)} total frames/iframes")
        
        # List to store all PDF links
        pdf_links = []
        max_links = 1100  # Limit to first 1100 links
        current_year = None  # Track the current year
        
        # Try each frame/iframe
        table_found = False
        for idx, frame in enumerate(all_frames):
            try:
                print(f"Switching to frame/iframe {idx+1}")
                driver.switch_to.frame(frame)
                
                # Wait for frame content to load
                time.sleep(2)
                
                # Try to find any tables
                tables = driver.find_elements(By.TAG_NAME, "table")
                print(f"Found {len(tables)} tables in frame/iframe {idx+1}")
                
                for table_idx, table in enumerate(tables):
                    try:
                        # Check if this table has the expected structure
                        rows = table.find_elements(By.TAG_NAME, "tr")
                        if not rows:
                            continue
                            
                        print(f"Processing table {table_idx+1} with {len(rows)} rows")
                        
                        # Check the first row for headers to confirm this is the right table
                        header_row = rows[0]
                        headers = header_row.find_elements(By.TAG_NAME, "th")
                        header_texts = [h.text.strip() for h in headers]
                        print(f"Table {table_idx+1} headers: {header_texts}")
                        
                        # Look for a table that has Year and District in headers
                        if not ('Year' in header_texts and 'District' in header_texts):
                            print(f"Table {table_idx+1} doesn't have the expected headers, skipping")
                            continue
                            
                        print(f"Found the correct table! Processing rows...")
                        table_found = True
                        
                        # Process data rows (skip header row)
                        for row_idx, row in enumerate(rows[1:], 1):
                            if len(pdf_links) >= max_links:
                                print(f"Reached the maximum limit of {max_links} links")
                                break
                                
                            try:
                                # Get all th elements in this row
                                th_elements = row.find_elements(By.TAG_NAME, "th")
                                num_th = len(th_elements)
                                
                                print(f"Row {row_idx}: Found {num_th} th elements")
                                
                                # Extract year and district based on the number of th elements
                                district_name = None
                                
                                if num_th == 3:
                                    # Row has year, district, and link
                                    year_element = th_elements[0]
                                    district_element = th_elements[1]
                                    link_element = th_elements[2]
                                    
                                    current_year = year_element.text.strip()
                                    district_name = district_element.text.strip()
                                    
                                    print(f"Row {row_idx}: Year={current_year}, District={district_name}")
                                    
                                elif num_th == 2:
                                    # Row has district and link (use current_year from previous row)
                                    district_element = th_elements[0]
                                    link_element = th_elements[1]
                                    
                                    district_name = district_element.text.strip()
                                    print(f"Row {row_idx}: Using year={current_year}, District={district_name}")
                                    
                                else:
                                    print(f"Row {row_idx}: Unexpected number of th elements, skipping")
                                    continue
                                
                                # Find PDF link in the last th element
                                link_th = th_elements[-1]
                                links = link_th.find_elements(By.TAG_NAME, "a")
                                
                                for link in links:
                                    href = link.get_attribute("href")
                                    if href and href.endswith(".pdf"):
                                        # Format filename: YYYY_XYZ_District.pdf
                                        year_part = current_year.split('-')[0] if current_year else "Unknown"
                                        
                                        # Clean up district name
                                        district_part = district_name.replace(" School District", "").replace(" District", "")
                                        
                                        # Create file name
                                        file_name = f"{year_part}_{district_part}.pdf"
                                        # Sanitize filename
                                        file_name = ''.join(c for c in file_name if c.isalnum() or c in ' -_.')
                                        file_name = file_name.replace(' ', '_')
                                        
                                        pdf_links.append({
                                            "url": href,
                                            "text": link.text.strip(),
                                            "file_name": file_name
                                        })
                                        print(f"Found PDF link {len(pdf_links)}/{max_links}: {href} (Will save as: {file_name})")
                                        
                                        if len(pdf_links) >= max_links:
                                            break
                            except Exception as e:
                                print(f"Error processing row {row_idx}: {e}")
                                continue
                                
                        # If we found links in this table, we can stop looking
                        if pdf_links and table_found:
                            break
                    except Exception as e:
                        print(f"Error processing table {table_idx+1}: {e}")
                
                # Return to main document
                driver.switch_to.default_content()
                
                # If we found links in this frame, we can stop looking at frames
                if pdf_links and table_found:
                    break
                    
            except Exception as e:
                print(f"Error processing frame/iframe {idx+1}: {e}")
                driver.switch_to.default_content()
        
        # Download each PDF
        if pdf_links:
            print(f"Preparing to download {len(pdf_links)} PDFs")
            
            headers = {
                "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.212 Safari/537.36"
            }
            
            for i, pdf_info in enumerate(pdf_links):
                try:
                    pdf_url = pdf_info["url"]
                    filename = pdf_info["file_name"]
                    
                    file_path = os.path.join(output_folder, filename)
                    
                    print(f"Downloading {i+1}/{len(pdf_links)}: {pdf_url} to {file_path}...")
                    
                    # Download using requests to avoid browser download dialogs
                    response = requests.get(pdf_url, headers=headers)
                    response.raise_for_status()
                    
                    with open(file_path, 'wb') as f:
                        f.write(response.content)
                    
                    print(f"Successfully downloaded {filename}")
                    
                except Exception as e:
                    print(f"Error downloading {pdf_url}: {e}")
        else:
            print("No PDF links found")
                    
    except Exception as e:
        print(f"An error occurred: {e}")
    
    finally:
        # Close the browser
        print("Closing WebDriver...")
        driver.quit()


if __name__ == "__main__":
    # Set the URL and output directory
    website_url = "https://public-edsight.ct.gov/overview/profile-and-performance-reports?language=en_US"
    output_dir = "/Users/aeg88/Desktop/super_district_pdfs"
    
    print(f"Starting scraper with URL: {website_url}")
    print(f"PDFs will be saved to: {output_dir}")
    
    # Run the scraper
    scrape_district_pdfs(website_url, output_dir)


# from selenium import webdriver
# from selenium.webdriver.chrome.options import Options
# from selenium.webdriver.common.by import By
# from selenium.webdriver.support.ui import WebDriverWait
# from selenium.webdriver.support import expected_conditions as EC
# from selenium.common.exceptions import TimeoutException, NoSuchElementException
# from urllib.parse import urljoin
# import os
# import time
# import requests


# def scrape_district_pdfs(url, output_folder):
#     """
#     Scrapes a website for PDFs by parsing a specific table in the HTML.
    
#     Args:
#         url (str): The URL of the webpage containing the table
#         output_folder (str): The folder where PDFs will be saved
#     """
#     # Create output folder if it doesn't exist
#     if not os.path.exists(output_folder):
#         os.makedirs(output_folder)
#         print(f"Created output folder: {output_folder}")
    
#     # Configure Chrome options
#     chrome_options = Options()
#     #chrome_options.add_argument("--headless")  # Run in headless mode (no GUI)
#     chrome_options.add_argument("--disable-gpu")
#     chrome_options.add_argument("--window-size=1920,1080")
#     chrome_options.add_argument("--disable-extensions")
#     chrome_options.add_argument("--no-sandbox")
#     chrome_options.add_argument("--disable-dev-shm-usage")
    
#     # Add user-agent to appear more like a regular browser
#     chrome_options.add_argument("user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.212 Safari/537.36")
    
#     print("Starting Chrome WebDriver...")
#     driver = webdriver.Chrome(options=chrome_options)
    
#     try:
#         print(f"Navigating to URL: {url}")
#         driver.get(url)
        
#         # Wait for page to load
#         wait = WebDriverWait(driver, 20)
#         wait.until(EC.presence_of_element_located((By.TAG_NAME, "body")))
        
#         # Give the page a little more time to fully render
#         time.sleep(5)
        
#         print("Page loaded, looking for content...")
        
#         # The URL might redirect to a frame-based page; wait a bit longer
#         time.sleep(5)
        
#         # First check if there are any iframes or frames that might contain our content
#         frames = driver.find_elements(By.TAG_NAME, "frame")
#         iframes = driver.find_elements(By.TAG_NAME, "iframe")
#         print(f"Found {len(frames)} frames and {len(iframes)} iframes on the page")
        
#         # Try to find frameset
#         try:
#             frameset = driver.find_element(By.TAG_NAME, "frameset")
#             print("Found a frameset, the content might be in frames")
#         except NoSuchElementException:
#             print("No frameset found")
        
#         # List to store all PDF links
#         pdf_links = []
#         max_links = 1100  # Limit to first 4,100 links
#         current_year = None  # Track the current year across rows
        
#         # First, try to look inside both frames and iframes
#         all_frames = frames + iframes
#         table_found = False
        
#         for idx, frame in enumerate(all_frames):
#             try:
#                 print(f"Switching to frame/iframe {idx+1}")
#                 driver.switch_to.frame(frame)
                
#                 # Wait a moment for the frame content to load
#                 time.sleep(2)
                
#                 # Print the page source to debug
#                 page_source = driver.page_source
#                 print(f"Frame {idx+1} source length: {len(page_source)} characters")
#                 print(f"Frame {idx+1} contains 'table class=\"table\"': {'table class=\"table\"' in page_source}")
#                 print(f"Frame {idx+1} contains 'Procedure Report': {'Procedure Report' in page_source}")
                
#                 # Try to find the table inside the frame using various selectors
#                 tables = []
#                 try:
#                     # Try with the exact summary you provided
#                     tables = driver.find_elements(By.CSS_SELECTOR, 'table.table[summary*="Procedure Report"]')
#                     if tables:
#                         print(f"Found {len(tables)} tables matching summary in frame {idx+1}")
#                         table = tables[0]
#                         table_found = True
#                 except Exception as e:
#                     print(f"Error searching for tables in frame {idx+1}: {e}")
                
#                 if not tables:
#                     try:
#                         # Try any table with class="table"
#                         tables = driver.find_elements(By.CSS_SELECTOR, 'table.table')
#                         if tables:
#                             print(f"Found {len(tables)} tables with class 'table' in frame {idx+1}")
#                             table = tables[0]
#                             table_found = True
#                     except Exception as e:
#                         print(f"Error searching for tables by class in frame {idx+1}: {e}")
                
#                 if not tables:
#                     # Try all tables and look for one with specific headers
#                     try:
#                         all_tables = driver.find_elements(By.TAG_NAME, 'table')
#                         print(f"Found {len(all_tables)} total tables in frame {idx+1}")
                        
#                         for t_idx, t in enumerate(all_tables):
#                             try:
#                                 headers = t.find_elements(By.XPATH, './/th')
#                                 header_texts = [h.text.strip() for h in headers if h.text.strip()]
#                                 print(f"Table {t_idx+1} headers: {', '.join(header_texts)}")
                                
#                                 # If this table has headers for Year, District, etc.
#                                 if any('Year' in h for h in header_texts) and any('District' in h for h in header_texts):
#                                     print(f"Found a table with Year and District headers in frame {idx+1}")
#                                     table = t
#                                     table_found = True
#                                     tables = [table]
#                                     break
#                             except Exception as e:
#                                 print(f"Error checking headers for table {t_idx+1}: {e}")
#                     except Exception as e:
#                         print(f"Error searching for all tables in frame {idx+1}: {e}")
                
#                 if table_found and tables:
#                     # Process the table found in iframe
#                     # Find all rows in the table
#                     rows = table.find_elements(By.TAG_NAME, "tr")
#                     print(f"Found {len(rows)} rows in the table in iframe {idx+1}")
                    
#                     # Process each row to find PDF links
#                     for row_idx, row in enumerate(rows):
#                         if len(pdf_links) >= max_links:
#                             print(f"Reached the maximum limit of {max_links} links")
#                             break
                        
#                         try:
#                             # Check for year cell - look for cells with rowspan attribute
#                             year_cells = row.find_elements(By.XPATH, ".//th[@rowspan]")
#                             for cell in year_cells:
#                                 cell_text = cell.text.strip()
#                                 if cell_text and any(char.isdigit() for char in cell_text):
#                                     if "-" in cell_text:  # Looks like a year range (e.g., 2023-24)
#                                         current_year = cell_text
#                                         print(f"Found year: {current_year}")
                            
#                             # Try to find district name in the second column
#                             # We'll use XPath to target the second th element in the row
#                             district_cells = row.find_elements(By.XPATH, ".//th[2]")
#                             if not district_cells:  # Try td elements if th not found
#                                 district_cells = row.find_elements(By.XPATH, ".//td[2]")
                            
#                             district_name = "Unknown_District"
#                             if district_cells:
#                                 district_name = district_cells[0].text.strip()
#                                 if district_name:
#                                     print(f"Row {row_idx+1}: Found district: {district_name}")
                            
#                             # Find all links in this row
#                             links = row.find_elements(By.TAG_NAME, "a")
                            
#                             for link in links:
#                                 href = link.get_attribute("href")
#                                 if href and href.endswith(".pdf"):
#                                     # Format filename: YYYY_XYZ_District.pdf
#                                     # Extract year (first part of "2023-24")
#                                     year_part = current_year.split('-')[0] if current_year else "Unknown"
                                    
#                                     # Clean up district name
#                                     district_part = district_name
#                                     district_part = district_part.replace(" School District", "")
#                                     district_part = district_part.replace(" District", "")
                                    
#                                     # Create file name
#                                     file_name = f"{year_part}_{district_part}.pdf"
#                                     # Sanitize filename
#                                     file_name = ''.join(c for c in file_name if c.isalnum() or c in ' -_.')
#                                     file_name = file_name.replace(' ', '_')
                                    
#                                     pdf_links.append({
#                                         "url": href,
#                                         "text": link.text.strip(),
#                                         "row_text": f"{current_year} - {district_part}",
#                                         "file_name": file_name
#                                     })
#                                     print(f"Found PDF link {len(pdf_links)}/{max_links}: {href} (Will save as: {file_name})")
                                    
#                                     if len(pdf_links) >= max_links:
#                                         break
#                         except Exception as e:
#                             print(f"Error processing row {row_idx+1} in iframe {idx+1}: {e}")
                
#                 # Switch back to the main window
#                 driver.switch_to.default_content()
                
#                 # If we found the table and processed links, no need to check other iframes
#                 if table_found and pdf_links:
#                     break
                
#             except Exception as e:
#                 print(f"Error processing iframe {idx+1}: {e}")
#                 # Make sure we're back to the main window
#                 driver.switch_to.default_content()
        
#         # If table not found in iframes, look in the main page
#         if not table_found:
#             print("Looking for table in the main page...")
#             try:
#                 # Try with the exact summary you provided
#                 table = driver.find_element(By.CSS_SELECTOR, 'table.table[summary="Procedure Report: Detailed and/or summarized report"]')
#                 print("Target table found in main page with exact summary match!")
#                 table_found = True
#             except NoSuchElementException:
#                 try:
#                     # Try with a partial match
#                     table = driver.find_element(By.XPATH, '//table[@class="table" and contains(@summary, "Procedure Report")]')
#                     print("Target table found in main page with partial summary match!")
#                     table_found = True
#                 except NoSuchElementException:
#                     try:
#                         # Try any table with class="table"
#                         table = driver.find_element(By.CSS_SELECTOR, 'table.table')
#                         print("Found a table with class 'table' in main page")
#                         table_found = True
#                     except NoSuchElementException:
#                         print("Could not find the specified table in main page.")
#                         table_found = False
            
#             if table_found:
#                 # Process the table found in main page
#                 rows = table.find_elements(By.TAG_NAME, "tr")
#                 print(f"Found {len(rows)} rows in the table in main page")
                
#                 # Process each row to find PDF links
#                 for row_idx, row in enumerate(rows):
#                     if len(pdf_links) >= max_links:
#                         print(f"Reached the maximum limit of {max_links} links")
#                         break
                    
#                     try:
#                         # Check for year cell
#                         year_cells = row.find_elements(By.XPATH, ".//th[@rowspan]")
#                         for cell in year_cells:
#                             cell_text = cell.text.strip()
#                             if cell_text and any(char.isdigit() for char in cell_text):
#                                 if "-" in cell_text:  # Looks like a year range (e.g., 2023-24)
#                                     current_year = cell_text
#                                     print(f"Found year: {current_year}")
                        
#                         # Find district name (second column)
#                         district_cells = row.find_elements(By.XPATH, ".//th[position()=2]")
#                         if not district_cells:  # Try td elements if th not found
#                             district_cells = row.find_elements(By.XPATH, ".//td[position()=2]")
                        
#                         district_name = "Unknown_District"
#                         if district_cells:
#                             district_name = district_cells[0].text.strip()
#                             if district_name:
#                                 print(f"Row {row_idx+1}: Found district: {district_name}")
                        
#                         # If district is empty or doesn't contain text, try another approach
#                         if not district_name or district_name == "Unknown_District":
#                             # Try to get all text cells in the row
#                             cells = row.find_elements(By.XPATH, ".//th | .//td")
#                             row_text = " ".join([cell.text.strip() for cell in cells if cell.text.strip()])
#                             print(f"Row text: {row_text}")
                            
#                             # Extract district name from row text if possible
#                             if row_text and "District" in row_text:
#                                 parts = row_text.split("District")
#                                 if len(parts) > 1:
#                                     # Take the part before "District" and clean it
#                                     district_name = parts[0].strip() + " District"
#                                     print(f"Extracted district from row text: {district_name}")
                        
#                         # Find all links in this row
#                         links = row.find_elements(By.TAG_NAME, "a")
                        
#                         for link in links:
#                             href = link.get_attribute("href")
#                             if href and href.endswith(".pdf"):
#                                 # If the link text is "- - District Level - -", don't use it as district name
#                                 link_text = link.text.strip()
#                                 if link_text == "- - District Level - -" and district_name == "Unknown_District":
#                                     # As a last resort, try to extract district from the URL
#                                     try:
#                                         # Extract filename from the URL
#                                         filename = href.split('/')[-1]
#                                         # If it contains an underscore, might have district code
#                                         if '_' in filename:
#                                             parts = filename.split('_')
#                                             if len(parts) >= 2:
#                                                 district_code = parts[0]
#                                                 district_name = f"District_{district_code}"
#                                                 print(f"Extracted district code from URL: {district_name}")
#                                     except:
#                                         pass
                        
#                         for link in links:
#                             href = link.get_attribute("href")
#                             if href and href.endswith(".pdf"):
#                                 # Format filename: YYYY_XYZ_District.pdf
#                                 year_part = current_year.split('-')[0] if current_year else "Unknown"
                                
#                                 # Clean up district name
#                                 district_part = district_name
#                                 district_part = district_part.replace(" School District", "")
#                                 district_part = district_part.replace(" District", "")
                                
#                                 # Create file name
#                                 file_name = f"{year_part}_{district_part}.pdf"
#                                 # Sanitize filename
#                                 file_name = ''.join(c for c in file_name if c.isalnum() or c in ' -_.')
#                                 file_name = file_name.replace(' ', '_')
                                
#                                 pdf_links.append({
#                                     "url": href,
#                                     "text": link.text.strip(),
#                                     "row_text": f"{current_year} - {district_part}",
#                                     "file_name": file_name
#                                 })
#                                 print(f"Found PDF link {len(pdf_links)}/{max_links}: {href} (Will save as: {file_name})")
                                
#                                 if len(pdf_links) >= max_links:
#                                     break
#                     except Exception as e:
#                         print(f"Error processing row {row_idx+1} in main page: {e}")
        
#         # If we still don't have any links, try a more general approach
#         if not pdf_links:
#             print("No PDF links found in tables. Searching entire page...")
            
#             # Switch back to main content if we were in an iframe
#             driver.switch_to.default_content()
            
#             # Find all links on the page
#             links = driver.find_elements(By.TAG_NAME, "a")
#             print(f"Found {len(links)} total links on the main page")
            
#             for link_idx, link in enumerate(links):
#                 if len(pdf_links) >= max_links:
#                     print(f"Reached the maximum limit of {max_links} links")
#                     break
                    
#                 try:
#                     href = link.get_attribute("href")
#                     if href and href.endswith(".pdf"):
#                         link_text = link.text.strip()
                        
#                         # Create a unique filename for main page links
#                         file_name = f"pdf_{link_idx+1}.pdf"
                        
#                         pdf_links.append({
#                             "url": href,
#                             "text": link_text,
#                             "row_text": f"Main page - {link_text}",
#                             "file_name": file_name
#                         })
#                         print(f"Found PDF link {len(pdf_links)}/{max_links}: {href} (Will save as: {file_name})")
#                 except Exception as e:
#                     print(f"Error processing link {link_idx+1}: {e}")
#                     continue
        
#         # Download each PDF if we found any links
#         if pdf_links:
#             print(f"Preparing to download {len(pdf_links)} PDFs")
            
#             headers = {
#                 "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.212 Safari/537.36"
#             }
            
#             for i, pdf_info in enumerate(pdf_links):
#                 try:
#                     pdf_url = pdf_info["url"]
                    
#                     # Use the file_name from pdf_info if available
#                     if "file_name" in pdf_info and pdf_info["file_name"]:
#                         filename = pdf_info["file_name"]
#                     else:
#                         # Fallback to index-based naming if something went wrong
#                         filename = f"pdf_{i+1}.pdf"
                    
#                     file_path = os.path.join(output_folder, filename)
                    
#                     print(f"Downloading {pdf_url} to {file_path}...")
                    
#                     # Download using requests to avoid browser download dialogs
#                     response = requests.get(pdf_url, headers=headers)
#                     response.raise_for_status()
                    
#                     with open(file_path, 'wb') as f:
#                         f.write(response.content)
                    
#                     print(f"Successfully downloaded {filename}")
                    
#                 except Exception as e:
#                     print(f"Error downloading {pdf_url}: {e}")
#         else:
#             print("No PDF links found")
                    
#     except Exception as e:
#         print(f"An error occurred: {e}")
    
#     finally:
#         # Close the browser
#         print("Closing WebDriver...")
#         driver.quit()


# if __name__ == "__main__":
#     # Set the URL and output directory
#     website_url = "https://public-edsight.ct.gov/overview/profile-and-performance-reports?language=en_US"
#     output_dir = "/Users/aeg88/Desktop/super_district_pdfs"
    
#     print(f"Starting scraper with URL: {website_url}")
#     print(f"PDFs will be saved to: {output_dir}")
    
#     # Run the scraper
#     scrape_district_pdfs(website_url, output_dir)