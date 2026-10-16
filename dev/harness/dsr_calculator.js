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
  // Populations and event totals below the minimums, each in its colour
  const highlights = cls => page.$$eval(`#counts_table tbody .${cls}`,
    es => [es.length, es.length ? getComputedStyle(es[0]).backgroundColor : null]);
  console.log('COUNTS highlighted cells: populations', JSON.stringify(await highlights('dsr-short-pop')),
    'event totals', JSON.stringify(await highlights('dsr-short-events')));
  await shot('02_counts');
  await page.click('#table a[data-value=rates]');

  // Copy Code: the dsr_calculate() call and its arguments
  await page.click('#insert_code');
  await page.waitForTimeout(800);
  console.log('CODE:', JSON.stringify((await clipboard()).split('\n').filter(l => /dsr_calculate|min_|suppress/.test(l))));

  // Explore: the linked map and funnel plot
  await page.click('a[data-value=explore]');
  await page.waitForSelector('#ex_funnel svg [data-id]', { timeout: 60000 });
  await settle(2500);
  const areas = sel => page.$$eval(sel, es => new Set(es.map(e => e.getAttribute('data-id'))).size);
  console.log('EXPLORE heading:', await text('#ex_heading'));
  console.log('EXPLORE note:', await text('#ex_method'));
  console.log('EXPLORE areas: map', await areas('#ex_map svg [data-id]'), 'funnel', await areas('#ex_funnel svg [data-id]'));
  await shot('03_explore');

  // The toolbars, shown on pointing at a plot, in Iris rgb(86, 44, 140),
  // with no PNG download
  await page.hover('#ex_map svg');
  await page.waitForTimeout(800);
  console.log('EXPLORE map toolbar:', JSON.stringify(await page.$$eval('#ex_map .ggiraph-toolbar-icon',
    is => is.map(i => [i.getAttribute('title'), getComputedStyle(i).fill]))));
  await shot('04_explore_toolbar');

  // Download: one menu of the four files. Choosing one closes the menu.
  await page.click('#ex_download');
  await page.waitForTimeout(500);
  console.log('EXPLORE download menu:', JSON.stringify(await page.$$eval('#ex_download + .dropdown-menu .dropdown-item',
    as => as.map(a => [a.textContent.trim(), a.classList.contains('disabled') ? 'disabled' : 'enabled']))));
  await shot('05_explore_download');
  const download = async click => {
    const [d] = await Promise.all([page.waitForEvent('download', { timeout: 30000 }), click()]);
    const file = `${out}/${d.suggestedFilename()}`;
    await d.saveAs(file);
    return [d.suggestedFilename(), fs.readFileSync(file).subarray(0, 4).toString('latin1').replace(/[^ -~]/g, '')];
  };
  for (const id of ['ex_map_pdf', 'ex_map_png', 'ex_funnel_pdf', 'ex_funnel_png']) {
    if (!(await page.isVisible(`#${id}`))) await page.click('#ex_download');
    console.log(`EXPLORE ${id}:`, JSON.stringify(await download(() => page.click(`#${id}`))));
  }

  // At state level, the ACT and surrounds are two areas: neither plot is
  // shown (htmlwidgets hides the last one drawn rather than removing it)
  await select('scope', 'SURROUNDS');
  await select('level', 'state');
  await settle();
  await page.click('#calculate');
  await settle(2500);
  const shown = sel => page.$eval(sel, e => getComputedStyle(e).visibility !== 'hidden' &&
    e.querySelectorAll('svg [data-id]').length > 0);
  console.log('EXPLORE state level: note', JSON.stringify(await text('#ex_note')),
    'map shown', await shown('#ex_map'), 'funnel shown', await shown('#ex_funnel'),
    'heading', JSON.stringify(await text('#ex_heading')));
  await shot('06_explore_state');
  await select('scope', 'ACT');
  await select('level', 'sa3');
  await settle();
  await page.click('#calculate');
  await settle(1500);

  // Help
  await page.click('a[data-value=help]');
  await settle(800);
  console.log('HELP sections:', JSON.stringify(await page.$$eval('.tab-pane.active h5', hs => hs.map(h => h.textContent.trim()))));
  await shot('07_help');

  // Diagnosis codes: words list the codes whose titles have words starting
  // with them, and Enter adds the code highlighted; a code or range is added
  // as typed, by Enter or Tab; other text is not added; the note spells out
  // each entry
  let box = '#codes-selectized';
  let control = '#codes ~ .selectize-control';
  const suggestions = () => page.$$eval(`${control} .selectize-dropdown-content [data-selectable]`,
    os => os.map(o => o.textContent.trim()));
  const tags = () => page.$$eval(`${control} .selectize-input > [data-value]`,
    ts => ts.map(t => [t.getAttribute('data-value'), t.getAttribute('title')]));
  const typeCodes = async (s, key) => {
    await page.click(box);
    await page.type(box, s, { delay: 60 });
    await page.waitForFunction(c => document.querySelectorAll(`${c} .selectize-dropdown-content [data-selectable]`).length > 0,
      control, { timeout: 5000 }).catch(() => {});
    await page.waitForTimeout(400);
    const shown = await suggestions();
    await page.keyboard.press(key);
    await page.waitForTimeout(400);
    return shown;
  };
  console.log('SEARCH myocard:', JSON.stringify(await typeCodes('myocard', 'Escape')));
  await page.fill(box, '');
  console.log('SEARCH old myoc, Enter:', JSON.stringify(await typeCodes('old myoc', 'Enter')));
  console.log('SEARCH I25-I27, Enter:', JSON.stringify(await typeCodes('I25-I27', 'Enter')));
  console.log('SEARCH J4, Tab:', JSON.stringify((await typeCodes('J4', 'Tab')).slice(0, 3)));
  console.log('SEARCH zzzz, Tab:', JSON.stringify(await typeCodes('zzzz', 'Tab')));
  await page.fill(box, '');
  console.log('CODES tags:', JSON.stringify(await tags()));
  await settle(800);
  console.log('CODES note:', await text('#codes_note'));
  // Clicking the empty box lists nothing
  await page.click(box);
  await page.waitForTimeout(400);
  console.log('CODES list on clicking the empty box:',
    await page.$eval(`${control} .selectize-dropdown`, d => getComputedStyle(d).display));
  await shot('08_codes_note');
  await page.click(box);
  await page.type(box, 'acute myoc', { delay: 60 });
  await page.waitForTimeout(800);
  await shot('09_codes_search');
  await page.keyboard.press('Escape');

  // Procedure codes: disabled for ED. For APC, words list the codes and
  // blocks, Enter adds the one highlighted, a code or block range is added as
  // typed, and the note spells out each entry. Diagnoses or procedures then
  // select the events.
  box = '#procedures-selectized';
  control = '#procedures ~ .selectize-control';
  const disabled = () => page.evaluate(() => [
    $('#procedures')[0].selectize.isDisabled,
    $('input[name=procedure]').prop('disabled'),
    $('input[name=combine]').prop('disabled')]);
  console.log('PROCEDURES disabled with ED:', JSON.stringify(await disabled()));
  console.log('PROCEDURES note with ED:', await text('#procedures_note'));
  await select('dataset', 'APC');
  await settle();
  console.log('PROCEDURES disabled with APC:', JSON.stringify(await disabled()));
  console.log('SEARCH nerve, Enter:', JSON.stringify(await typeCodes('nerve', 'Enter')));
  console.log('SEARCH 39001-01, Enter:', JSON.stringify(await typeCodes('39001-01', 'Enter')));
  console.log('SEARCH 1-5, Tab:', JSON.stringify((await typeCodes('1-5', 'Tab')).slice(0, 3)));
  await page.fill(box, '');
  console.log('PROCEDURES tags:', JSON.stringify(await tags()));
  await settle(800);
  console.log('PROCEDURES note:', await text('#procedures_note'));
  await page.check('input[name=combine][value=or]');
  await page.click('a[data-value=tables]');
  await page.click('#calculate');
  await settle(2000);
  console.log('INFO BAR with codes:', (await text('#info_bar')).split('. ')[0]);
  await shot('10_procedures');

  // With the codes, event totals fall below 20 too: blue beside the yellow
  await page.click('#table a[data-value=counts]');
  await settle(1500);
  console.log('COUNTS with codes: populations', JSON.stringify(await highlights('dsr-short-pop')),
    'event totals', JSON.stringify(await highlights('dsr-short-events')));
  console.log('COUNTS note colours:', JSON.stringify(await page.$$eval('#counts_note .dsr-short',
    es => es.map(e => [e.textContent, getComputedStyle(e).backgroundColor]))));
  await shot('11_counts_codes');

  // Done stops the app and the page says so
  await page.click('#done');
  await page.waitForSelector('.epi-stopped', { timeout: 15000 });
  console.log('DONE:', await text('.epi-stopped'));

  await browser.close();
})().catch(e => { console.log('SCRIPT ERROR:', e.message); process.exit(1); });
