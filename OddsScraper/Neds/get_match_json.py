from playwright.async_api import async_playwright
import asyncio
import pandas as pd
import json
import os

async def main():
    # Load URLs from the CSV file
    match_urls = pd.read_csv("OddsScraper\\Neds\\neds_afl_match_urls.csv")
    if "url" not in match_urls.columns:
        raise ValueError("CSV file must contain 'url' column")
    urls = match_urls["url"].tolist()

    # Ensure the directory exists
    output_dir = "OddsScraper\\Neds\\"
    os.makedirs(output_dir, exist_ok=True)

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=True)
        page = await browser.new_page()

        # Counter to generate unique file names
        file_counter = 1

        # Process each URL
        for url in urls:
            # Handler to be registered and unregistered dynamically
            async def handle_response(response):
                nonlocal file_counter
                request = response.request
                if 'card' in request.url:
                    try:
                        json_body = await response.json()
                        file_path = os.path.join(output_dir, f"data_{file_counter}.json")
                        with open(file_path, 'w', encoding='utf-8') as f:
                            json.dump(json_body, f, ensure_ascii=False, indent=4)
                        print(f"Saved JSON to {file_path}")
                        file_counter += 1
                        # Unregister the handler after the first relevant response
                        page.remove_listener('response', handle_response)
                    except Exception as e:
                        print(f"Error reading JSON from {request.url}: {e}")

            # Register the response handler
            page.on('response', handle_response)

            try:
                await page.goto(url)
                # Wait for the specific element to be visible
                await page.wait_for_selector('[data-testid="market-title"]', state='visible')
            except Exception as e:
                print(f"Error navigating to {url}: {e}")

        await browser.close()

asyncio.run(main())
