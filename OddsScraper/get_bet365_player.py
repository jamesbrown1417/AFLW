# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in CSV of URLs=========================================================
import pandas as pd
# Read csv (no header col)
url_df = pd.read_csv('Data/BET365_HTML/urls.csv', header=None)

# Convert first column to a list
url_df = url_df[0]

# Get H2H HTML===============================================================
import asyncio

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")
    
    async with webdriver.Chrome(options=options) as driver:
        for index, url in enumerate(url_df, start=1): # Start counting from 1 for match_n
            try:
                await driver.get(url)
                await driver.sleep(5)
                
                
                # Scroll into view of disposal button and click
                disposal_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'gl-MarketGroupButton_Text') and text()='Disposals']")
                await driver.execute_script("arguments[0].scrollIntoView(true);", disposal_button)
                await driver.execute_script("window.scrollBy(0, -150)")
                await disposal_button.click()
                await driver.sleep(2)
                
                print("timepoint 2")
                
                # If there is a button that says Player Disposals, click it
                try:
                    player_disposals_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'gl-MarketGroupButton_Text') and text()='Player Disposals']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_disposals_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_disposals_button.click()
                    await driver.sleep(2)
                except:
                    pass
                
                # If there is a button that says Disposal Specials, click it
                try:
                    disposal_specials_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'gl-MarketGroupButton_Text') and text()='Disposal Specials']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", disposal_specials_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await disposal_specials_button.click()
                    await driver.sleep(2)
                except:
                    pass
                
                # Get all elements with class 'bbl-ShowMoreForHScroll ' that has text 'Show more'
                button_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'bbl-ShowMoreForHScroll ') and contains(text(), 'Show more')]")
                    
                # Scroll into view of the first button and click
                await driver.execute_script("arguments[0].scrollIntoView(true);", button_elements[0])
                await driver.execute_script("window.scrollBy(0, -150)")
                await button_elements[0].click()

                # Scroll into view of the second button
                await driver.execute_script("arguments[0].scrollIntoView(true);", button_elements[1])
                await driver.execute_script("window.scrollBy(0, -150)")
                    
                # Click on the second button
                await button_elements[1].click()
                    
                await driver.sleep(2)
                
                # Scroll into view of the third button
                try:
                    
                    specials_button_element = await driver.find_element(By.XPATH, "//div[contains(@class, 'bbl-ShowMore ') and text()='Show more']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", specials_button_element)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    
                    # Click on the third button
                    await specials_button_element.click()
                    print('pressed third button')
                    await driver.sleep(2)
                    
                except:
                    pass
                    
                # Write out html to file------------------------------------------------
                    
            # wait for elem to exist
                elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
                body_html_players_a = await elem.get_attribute('outerHTML')
                with open(f"Data/BET365_HTML/body_html_players_a_match_{index}.txt", 'w') as f:
                    f.write(body_html_players_a)
                    
                # Get "bbl-TabSwitcherItem_TabText " element
                tab_elements = await driver.find_elements(By.XPATH, ".//div[contains(@class, 'bbl-TabSwitcherItem_TabText ')]")
                    
                # Click on the second tab of second section after scrolling into view
                await driver.execute_script("arguments[0].scrollIntoView(true);", tab_elements[3])
                await driver.execute_script("window.scrollBy(0, -150)")
                await tab_elements[3].click()
                    
                await driver.sleep(2)
                    
                # Get all elements with class 'bbl-ShowMoreForHScroll ' that has text 'Show more'
                button_elements_2 = await driver.find_elements(By.XPATH, "//div[contains(@class, 'bbl-ShowMoreForHScroll ') and contains(text(), 'Show more')]")
                    
                await driver.sleep(2)
                    
                # Click on the first button after scrolling into view
                await driver.execute_script("arguments[0].scrollIntoView(true);", button_elements_2[0])
                await driver.execute_script("window.scrollBy(0, -150)")
                await button_elements_2[0].click()
                    
                await driver.sleep(2)
                    
                # Write out html to file------------------------------------------------
                # wait for elem to exist
                elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
                body_html_players_b = await elem.get_attribute('outerHTML')
                with open(f"Data/BET365_HTML/body_html_players_b_match_{index}.txt", 'w') as f:
                    f.write(body_html_players_b)
                        
            except Exception as e:
                print(f"An error occurred with URL {url}: {e}. Moving to the next URL.")
                print(f"The specific error was: {e}")
                continue  # Proceed to the next iteration of the loop

asyncio.run(main())