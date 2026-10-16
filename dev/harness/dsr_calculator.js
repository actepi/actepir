// dev/harness/dsr_calculator.js
//
// Baseline browser checks of the DSR Calculator on synthetic data. Run with
//   Rscript dev/harness/run_app.R dev/harness/dsr_calculator.js <output dir>
// It prints one line per check and saves a screenshot of each tab to the
// output directory. Copy it, or add to it, for the checks a change needs.

const { chromium } = require('playwright');
const fs = require('fs');

(async () => {
  const [url, out] = process.argv.slice(2);
  fs.mkdirSync(out, { recursive: true });
  const browser = await chromium.launch();
  const context = await browser.newContext({ viewport: { width: 1440, height: 900 } });
  // Copy table and Copy Code write to the clipboard
  await context.grantPermissions(['clipboard-read', 'clipboard-write'], { origin: url });
  const page = await context.newPage();
  page.on('pageerror', e => console.log('PAGE ERROR:', e.message));
  page.on('console', m => { if (m.type() === 'error') console.log('CONSOLE ERROR:', m.text()); });

  // Waits for Shiny to finish, then a little longer for widgets to draw
  const settle = async (ms = 1200) => {
    await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'));
    await page.waitForTimeout(ms);
  };
  const text = async sel => ((await page.textContent(sel)) || '').replace(/\s+/g, ' ').trim();
  const background = sel => page.$eval(sel, e => getComputedStyle(e).backgroundColor);
  const clipboard = () => page.evaluate(() => navigator.clipboard.readText());
  const shot = name => page.screenshot({ path: `${out}/${name}.png` });
  // Inputs are set through their widgets, as a choice in the page would be
  const select = (id, value) => page.evaluate(([id, v]) => $('#' + id)[0].selectize.setValue(v), [id, value]);

  await page.goto(url);
  await page.waitForSelector('#calculate');
  await settle();

  // Layout: the tabs in the top bar, and the sidebar's sections and inputs
  console.log('TABS:', JSON.stringify(await page.$$eval('.navbar-nav a.nav-link', as => as.map(a => a.textContent.trim()))));
  console.log('SIDEBAR:', JSON.stringify(await page.$$eval('.accordion-item', items => items.map(i => [
    i.querySelector('.accordion-title').textContent.trim(),
    [...i.querySelectorAll('label.control-label, .checkbox label')].map(l => l.textContent.trim())
  ]))));

  // Primary buttons go to Iris, rgb(86, 44, 140), on hover
  const before = await background('#calculate');
  await page.hover('#calculate');
  await page.waitForTimeout(400);
  console.log('HOVER Calculate:', before, '->', await background('#calculate'));
  await page.mouse.move(700, 600);

  // ED presentations, ACT residents by SA3, the default years
  await select('level', 'sa3');
  await settle();
  await page.click('#calculate');
  await page.waitForSelector('#summary_table tbody tr', { timeout: 60000 });
  await settle(1500);
  console.log('INFO BAR:', await text('#info_bar'));
  console.log('RATES rows:', await page.$$eval('#summary_table tbody tr', r => r.length));
  console.log('N.P. cells (text, reason):', JSON.stringify(await page.$$eval(
    '#summary_table tbody td span[title]', s => s.slice(0, 3).map(e => [e.textContent, e.title]))));
  await shot('01_rates');

  // Each table, and what Copy table copies from it
  for (const t of ['rates', 'age', 'counts']) {
    await page.click(`#table a[data-value=${t}]`);
    await settle(1500);
    await page.click('#copy_table');
    await page.waitForTimeout(800);
    const lines = (await clipboard()).trim().split('\n');
    console.log(`COPY ${t}: ${lines.length - 1} rows, header`, JSON.stringify(lines[0]));
  }
  console.log('COUNTS highlighted cells:', await page.$$eval('#counts_table tbody .dsr-short', e => e.length));
  await shot('02_counts');
  await page.click('#table a[data-value=rates]');

  // Copy Code: the dsr_calculate() call and its arguments
  await page.click('#insert_code');
  await page.waitForTimeout(800);
  console.log('CODE:', JSON.stringify((await clipboard()).split('\n').filter(l => /dsr_calculate|min_|suppress/.test(l))));

  // Map
  await page.click('a[data-value=map]');
  await page.waitForSelector('#map img', { timeout: 60000 });
  await settle(2000);
  await shot('03_map');

  // Explore: the linked map and funnel plot
  await page.click('a[data-value=explore]');
  await page.waitForSelector('#ex_funnel svg [data-id]', { timeout: 60000 });
  await settle(2500);
  const areas = sel => page.$$eval(sel, es => new Set(es.map(e => e.getAttribute('data-id'))).size);
  console.log('EXPLORE heading:', await text('#ex_heading'));
  console.log('EXPLORE note:', await text('#ex_method'));
  console.log('EXPLORE areas: map', await areas('#ex_map svg [data-id]'), 'funnel', await areas('#ex_funnel svg [data-id]'));
  await shot('04_explore');

  // Help
  await page.click('a[data-value=help]');
  await settle(800);
  console.log('HELP sections:', JSON.stringify(await page.$$eval('.tab-pane.active h5', hs => hs.map(h => h.textContent.trim()))));
  await shot('05_help');

  // Done stops the app and the page says so
  await page.click('#done');
  await page.waitForSelector('.epi-stopped', { timeout: 15000 });
  console.log('DONE:', await text('.epi-stopped'));

  await browser.close();
})().catch(e => { console.log('SCRIPT ERROR:', e.message); process.exit(1); });
