import time
import csv
import json
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, NoSuchElementException, ElementClickInterceptedException
import re


class UrbanTurfScraper:
    def __init__(self, headless=True, wait_time=10):
        """
        Initialize the scraper with Chrome options
        
        Args:
            headless (bool): Whether to run browser in headless mode
            wait_time (int): Maximum wait time for elements to load
        """
        self.wait_time = wait_time
        self.projects_data = []
        
        # Configure Chrome options
        chrome_options = Options()
        if headless:
            chrome_options.add_argument("--headless")
        chrome_options.add_argument("--no-sandbox")
        chrome_options.add_argument("--disable-dev-shm-usage")
        chrome_options.add_argument("--disable-gpu")
        chrome_options.add_argument("--window-size=1920,1080")
        chrome_options.add_argument("--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
        
        # Initialize the driver
        self.driver = webdriver.Chrome(options=chrome_options)
        self.wait = WebDriverWait(self.driver, wait_time)
    
    def get_project_links(self, url):
        """
        Get all project links and basic information from the main pipeline page
        
        Args:
            url (str): The URL to scrape
            
        Returns:
            list: List of dictionaries containing project info
        """
        print(f"Navigating to: {url}")
        self.driver.get(url)
        
        # Wait for the page to load
        time.sleep(10)
        
        projects_info = []
        
        try:
            # Wait for project elements to be present
            self.wait.until(EC.presence_of_element_located((By.CLASS_NAME, "pipeline-item")))
            # Find all project containers
            project_elements = self.driver.find_elements(By.CLASS_NAME, "pipeline-item")
            print(f"Found {len(project_elements)} projects")

            time.sleep(10)

            for i, project in enumerate(project_elements):
                try:
                    # Get project name and link
                    anchor_tag = project.find_element(By.TAG_NAME, "a")
                    # Get the href attribute
                    project_link = anchor_tag.get_attribute("href")
                    name = anchor_tag.get_attribute("title")
                    
                    projects_info.append({
                        'name': name,
                        'size': 'Not extracted yet',
                        'link': project_link,
                        'address': 'Not extracted yet'
                    })
                    
                except Exception as e:
                    print(f"Error processing project {i+1}: {e}")
                    continue
            
        except TimeoutException:
            print("Timeout waiting for projects to load")
        except Exception as e:
            print(f"Error finding projects: {e}")
        
        return projects_info
    
    def get_project_info(self, project_link):
        """
        Navigate to a project page and extract the project info
        
        Args:
            project_link (str): URL of the project page
            
        Returns:
            str: The project info or 'Not found'
        """
        try:
            print(f"Visiting project page: {project_link}")
            self.driver.get(project_link)
            time.sleep(3)

            s_paragraph = self.driver.find_element(By.XPATH, "//p[span[@class='label' and text()='No. of units:']]")
            s = s_paragraph.text.split(":")[-1].strip()  
            a_paragraph = self.driver.find_element(By.XPATH, "//p[span[@class='label' and text()='Address:']]")
            a = a_paragraph.text.split(":")[-1].strip()  

            return (a, s)
            
        except Exception as e:
            print(f"Error getting address from {project_link}: {e}")
            return ("Error extracting address", "Error extracting size")
    
    def scrape_all_projects(self, url):
        """
        Main method to scrape all project information
        
        Args:
            url (str): The URL to scrape
            
        Returns:
            list: Complete project data
        """
        try:
            # Get basic project info and links
            projects_info = self.get_project_links(url)
            
            if not projects_info:
                print("No projects found on the main page")
                return []
            
            print(f"\nFound {len(projects_info)} projects. Now extracting addresses...")
            
            # Visit each project page to get the address
            for i, project in enumerate(projects_info):
                print(f"\nProcessing project {i+1}/{len(projects_info)}: {project['name']}")
                
                # Get the address from the project page
                info = self.get_project_info(project['link'])
                project['address'] = info[0]
                project['size'] = info[1]
                
                # Add delay between requests to be respectful
                time.sleep(1)
                
            
            self.projects_data = projects_info
            return projects_info
            
        except Exception as e:
            print(f"Error in scrape_all_projects: {e}")
            return []
    
    def save_to_csv(self, filename="urbanturf_projects.csv"):
        """Save the scraped data to a CSV file"""
        if not self.projects_data:
            print("No data to save")
            return
        
        with open(filename, 'w', newline='', encoding='utf-8') as csvfile:
            fieldnames = ['name', 'size', 'address', 'link']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            
            writer.writeheader()
            for project in self.projects_data:
                writer.writerow(project)
        
        print(f"Data saved to {filename}")
    
    def close(self):
        """Close the browser driver"""
        if self.driver:
            self.driver.quit()
    
    def __enter__(self):
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()


def main():
    """
    Main function to run the scraper
    """
    url = "https://dc.urbanturf.com/pipeline?search_string=&project_type=0&status=0&more_or_less=0&number_of_units=&number_of_units_selector=0&city=0&state=DC&zip=0&order_by=last_updated&direction=desc&filtered=Yes&limit=500"
    
    # Use context manager for automatic cleanup
    with UrbanTurfScraper(headless=False) as scraper:
        print("Starting UrbanTurf pipeline scraper...")
        
        # Scrape all projects
        projects = scraper.scrape_all_projects(url)
        
        if projects:
            print(f"\n=== SCRAPING COMPLETE ===")
            print(f"Successfully scraped {len(projects)} projects")
            
            # Save results
            scraper.save_to_csv()
            
            # Print summary
            print("\n=== SAMPLE RESULTS ===")
            for i, project in enumerate(projects[:3]):  # Show first 3 projects
                print(f"\nProject {i+1}:")
                print(f"Name: {project['name']}")
                print(f"Size: {project['size']}")
                print(f"Address: {project['address']}")
                print(f"Link: {project['link']}")
        else:
            print("No projects were scraped successfully")


if __name__ == "__main__":
    main()
