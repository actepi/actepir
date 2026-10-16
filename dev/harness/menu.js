// dev/harness/menu.js
//
// Browser checks of the ACT Epidemiology menu, run by run_menu.R. It ends by
// clicking the DSR Calculator's Web browser button, so the dialog returns
// list(tool = "dsr", display = "browser").

const { chromium } = require('playwright');

(async () => {
  const [url, out] = process.argv.slice(2);
  const browser = await chromium.launch();
  const page = await (await browser.newContext({ viewport: { width: 440, height: 560 } })).newPage();
  page.on('pageerror', e => console.log('PAGE ERROR:', e.message));

  // The dialog's process takes a few seconds to start serving
  for (let i = 0; i < 60; i++) {
    try {
      await page.goto(url);
      await page.waitForSelector('#dsr_browser', { timeout: 2000 });
      break;
    } catch (e) {
      await page.waitForTimeout(500);
    }
  }
  await page.waitForTimeout(800);

  console.log('LOGO (natural width, height shown):', JSON.stringify(await page.$eval(
    '.epi-bar-logo img', i => [i.naturalWidth, Math.round(i.getBoundingClientRect().height)])));

  // Buttons go to Iris, rgb(86, 44, 140), on hover
  const background = sel => page.$eval(sel, e => getComputedStyle(e).backgroundColor);
  for (const sel of ['#dsr_browser', '#cancel']) {
    const before = await background(sel);
    await page.hover(sel);
    await page.waitForTimeout(400);
    console.log('HOVER', sel, before, '->', await background(sel));
  }
  await page.screenshot({ path: `${out}/menu.png` });

  await page.click('#dsr_browser');
  await page.waitForTimeout(1000);
  await browser.close();
})().catch(e => { console.log('SCRIPT ERROR:', e.message); process.exit(1); });
