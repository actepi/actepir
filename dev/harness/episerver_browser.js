// dev/harness/episerver_browser.js
//
// Baseline browser checks of the EpiServer Browser on synthetic data. Run with
//   Rscript dev/harness/run_app.R dev/harness/episerver_browser.js <output dir> browse

const { chromium } = require('playwright');
const fs = require('fs');

(async () => {
  const [url, out] = process.argv.slice(2);
  fs.mkdirSync(out, { recursive: true });
  const browser = await chromium.launch();
  const page = await (await browser.newContext({ viewport: { width: 1100, height: 800 } })).newPage();
  page.on('pageerror', e => console.log('PAGE ERROR:', e.message));
  page.on('console', m => { if (m.type() === 'error') console.log('CONSOLE ERROR:', m.text()); });

  await page.goto(url);
  await page.waitForSelector('#col_table tbody tr', { timeout: 60000 });
  await page.waitForTimeout(1200);
  console.log('COLUMNS listed:', await page.$$eval('#col_table tbody tr', r => r.length));

  // Buttons go to Iris, rgb(86, 44, 140), on hover
  const background = sel => page.$eval(sel, e => getComputedStyle(e).backgroundColor);
  for (const sel of ['#insert_code', '#done']) {
    const before = await background(sel);
    await page.hover(sel);
    await page.waitForTimeout(400);
    console.log('HOVER', sel, before, '->', await background(sel));
  }
  await page.mouse.move(600, 700);
  await page.screenshot({ path: `${out}/episerver_browser.png` });

  await browser.close();
})().catch(e => { console.log('SCRIPT ERROR:', e.message); process.exit(1); });
