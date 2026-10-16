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
  // Scrollbars shown, as on Windows: Playwright hides them by default
  const browser = await chromium.launch({ ignoreDefaultArgs: ['--hide-scrollbars'] });
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

  // Layout: the name at the left of the top bar, the tabs after a gap, the
  // logo at the right and no Done; the sidebar's sections and inputs
  console.log('TABS:', JSON.stringify(await page.$$eval('.navbar-nav a.nav-link', as => as.map(a => a.textContent.trim()))));
  console.log('TOP BAR:', JSON.stringify(await page.evaluate(() => {
    const r = s => document.querySelector(s).getBoundingClientRect();
    const brand = r('.navbar-brand'), tab = r('.navbar-nav a.nav-link'), logo = r('.navbar .epi-logo');
    return { nameLeft: Math.round(brand.left), gapToTabs: Math.round(tab.left - brand.right),
             middles: [brand, tab, logo].map(b => Math.round(b.top + b.height / 2)),
             logoRight: Math.round(window.innerWidth - logo.right), logoHeight: Math.round(logo.height),
             barHeight: Math.round(r('.navbar').height), done: !!document.querySelector('#done') };
  })));
  console.log('SIDEBAR:', JSON.stringify(await page.$$eval('.accordion-item', items => items.map(i => [
    i.querySelector('.accordion-title').textContent.trim(),
    [...i.querySelectorAll('label.control-label, .checkbox label')].map(l => l.textContent.trim())
  ]))));

  // Section headings: the info bar's tint, rgb(234, 230, 241), in bold
  // Nightshade, rgb(50, 5, 87)
  console.log('HEADINGS:', JSON.stringify(await page.$eval('.accordion-button', b => {
    const s = getComputedStyle(b);
    return [s.backgroundColor, s.color, s.fontWeight];
  })));

  // Each information button shows its notes and hides them again
  for (const id of await page.$$eval('aside#settings .epi-help', ds => ds.map(d => d.id))) {
    // A closed <details> hides its content with content-visibility, which
    // leaves offsetHeight as it was
    const visible = () => page.$eval(`#${id} .epi-note`, e => e.checkVisibility());
    const before = await visible();
    await page.click(`#${id} > summary`);
    await page.waitForTimeout(300);
    const open = await visible();
    const note = (await text(`#${id} .epi-note`)).slice(0, 50);
    if (id === 'codes_help') await shot('00_help_open');
    await page.click(`#${id} > summary`);
    await page.waitForTimeout(300);
    console.log(`HELP ${id}: hidden ${!before}, shown ${open}, hidden again ${!(await visible())}:`, JSON.stringify(note));
  }

  // The wheel and the scrollbar's thumb scroll the sidebar when the pointer
  // reaches the scrollbar from the results, and the edge widens the sidebar
  const sidebar = await page.$eval('aside#settings', s => {
    const r = s.getBoundingClientRect();
    return { right: r.right, top: r.top, bottom: r.bottom,
             scrollbar: s.offsetWidth - s.clientWidth - parseFloat(getComputedStyle(s).borderRightWidth) };
  });
  const sidebarState = () => page.$eval('aside#settings', s => [s.scrollTop, Math.round(s.getBoundingClientRect().width)]);
  const y = Math.round((sidebar.top + sidebar.bottom) / 2) + 100;
  const wheeled = [];
  for (let off = 2; off <= sidebar.scrollbar; off += 3) {
    await page.$eval('aside#settings', s => { s.scrollTop = 0; });
    await page.mouse.move(sidebar.right + 80, y);
    await page.mouse.move(sidebar.right - off, y, { steps: 80 + off });
    await page.mouse.wheel(0, 300);
    await page.waitForTimeout(300);
    wheeled.push((await sidebarState())[0] > 0);
  }
  console.log(`SIDEBAR scrollbar ${sidebar.scrollbar}px, wheel from the results scrolls it at`,
    `${wheeled.filter(Boolean).length} of ${wheeled.length} points`);
  await page.$eval('aside#settings', s => { s.scrollTop = 0; });
  await page.mouse.move(sidebar.right + 80, sidebar.top + 40);
  await page.mouse.move(sidebar.right - 5, sidebar.top + 40, { steps: 85 });
  await page.mouse.down();
  await page.mouse.move(sidebar.right - 5, sidebar.top + 190, { steps: 10 });
  await page.mouse.up();
  console.log('SIDEBAR thumb dragged from the results: scrolled', (await sidebarState())[0] > 0);
  await page.$eval('aside#settings', s => { s.scrollTop = 0; });
  await page.mouse.move(sidebar.right + 80, y);
  await page.mouse.move(sidebar.right, y, { steps: 80 });
  await page.mouse.down();
  await page.mouse.move(sidebar.right + 120, y, { steps: 12 });
  await page.mouse.up();
  await page.waitForTimeout(300);
  console.log('SIDEBAR width after dragging its edge 120px:', (await sidebarState())[1]);
  await page.reload();
  await page.waitForSelector('#calculate');
  await settle();

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
  // The notification is in the info bar's colours, not Shiny's blue
  console.log('NOTIFICATION:', JSON.stringify(await page.$eval('#shiny-notification-panel .shiny-notification', n => {
    const s = getComputedStyle(n);
    return [n.textContent.replace(/\s+/g, ' ').replace('\u00d7', '').trim(), s.backgroundColor, s.color, s.borderLeftColor];
  })));
  await page.screenshot({ path: `${out}/02a_notification.png`, clip: { x: 1440 - 420, y: 900 - 160, width: 420, height: 160 } });
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

  // The bug button opens the problem report; Copy report copies what was
  // described and the details shown. In a short window the dialog fits,
  // and the wheel outside the details does not move it.
  await page.setViewportSize({ width: 1440, height: 650 });
  await page.click('#bug_report');
  await page.waitForSelector('#bug_details:not(:empty)');
  await page.waitForTimeout(800);
  console.log('REPORT dialog:', await text('.modal-title'), '|', await text('.modal-body p'));
  const dialogTop = () => page.$eval('.modal-content', e => Math.round(e.getBoundingClientRect().top));
  const fits = await page.$eval('.modal-content', e => e.getBoundingClientRect().bottom <= window.innerHeight);
  const top0 = await dialogTop();
  const what = await page.$eval('#bug_what', e => { const r = e.getBoundingClientRect(); return [r.left + 20, r.top + 20]; });
  await page.mouse.move(what[0], what[1]);
  await page.mouse.wheel(0, 400);
  await page.waitForTimeout(300);
  const details = await page.$eval('#bug_details', e => { const r = e.getBoundingClientRect(); return [r.left + 40, r.top + 20]; });
  await page.mouse.move(details[0], details[1]);
  for (let i = 0; i < 6; i++) await page.mouse.wheel(0, 400);
  await page.waitForTimeout(300);
  console.log('REPORT in a 650px window: fits', fits, '| moved by the wheel', (await dialogTop()) !== top0,
    '| details scrolled', await page.$eval('#bug_details', e => e.scrollTop > 0));
  await page.fill('#bug_what', 'The map was blank.');
  await page.waitForTimeout(600);
  await shot('02b_report');
  await page.click('#bug_copy');
  await page.waitForTimeout(800);
  const report = await clipboard();
  const lines = report.split('\n');
  console.log('REPORT headings:', JSON.stringify(lines.filter((l, i) => /^-+$/.test(lines[i + 1] || ''))));
  console.log('REPORT lines:', JSON.stringify(lines.filter(l => /^(The map|Calculated|Settings error|Code button|codes =|level =|actepir |Browser|Window|Errors shown)/.test(l))));
  await page.click('.modal-footer [data-bs-dismiss=modal]');
  await page.waitForTimeout(600);
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.waitForTimeout(400);

  // Explore: the linked map and funnel plot
  await page.click('a[data-value=explore]');
  await page.waitForSelector('#ex_funnel svg [data-id]', { timeout: 60000 });
  await settle(2500);
  const areas = sel => page.$$eval(sel, es => new Set(es.map(e => e.getAttribute('data-id'))).size);
  console.log('EXPLORE heading:', await text('#ex_heading'));
  console.log('EXPLORE note:', await text('#ex_method'));
  console.log('EXPLORE areas: map', await areas('#ex_map svg [data-id]'), 'funnel', await areas('#ex_funnel svg [data-id]'));
  // The settings run down the left of the plots, which fit the window, in a
  // panel with no dividing border that stays in view as the page scrolls
  const panel = () => page.$eval('#ex_settings', e => {
    const s = getComputedStyle(e), r = e.getBoundingClientRect();
    return { borderRight: s.borderRightStyle, top: Math.round(r.top), height: Math.round(r.height) };
  });
  // Scrolls the element that scrolls the tab: the first ancestor of the
  // panel that overflows
  const scrollTab = y => page.$eval('#ex_settings', (e, y) => {
    for (let el = e.parentElement; el; el = el.parentElement) {
      const o = getComputedStyle(el).overflowY;
      if ((o === 'auto' || o === 'scroll') && el.scrollHeight > el.clientHeight) {
        el.scrollTop = y === 'end' ? el.scrollHeight : y;
        return el.scrollTop;
      }
    }
    const doc = document.scrollingElement;
    doc.scrollTop = y === 'end' ? doc.scrollHeight : y;
    return doc.scrollTop;
  }, y);
  await page.setViewportSize({ width: 1440, height: 650 });
  await page.waitForTimeout(600);
  const panelTop = await panel();
  const scrolled = await scrollTab('end');
  await page.waitForTimeout(400);
  console.log('EXPLORE panel:', JSON.stringify(panelTop), `scrolled ${scrolled}px:`, JSON.stringify(await panel()));
  await scrollTab(0);
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.waitForTimeout(600);
  console.log('EXPLORE settings:', JSON.stringify(await page.$$eval('#ex_settings label.control-label, #ex_settings .checkbox label, #ex_settings .btn',
    ls => ls.filter(l => l.offsetParent).map(l => l.textContent.trim()))));
  console.log('EXPLORE layout:', JSON.stringify(await page.evaluate(() => {
    const r = s => document.querySelector(s).getBoundingClientRect();
    return { settingsRight: Math.round(r('#ex_settings').right), mapLeft: Math.round(r('#ex_map svg').left),
             plotsBottom: Math.round(Math.max(r('#ex_map svg').bottom, r('#ex_funnel svg').bottom)), window: window.innerHeight };
  })));
  await shot('03_explore');

  // The toolbars, shown on pointing at a plot, in Iris rgb(86, 44, 140),
  // with no PNG download
  await page.hover('#ex_map svg');
  await page.waitForTimeout(800);
  console.log('EXPLORE map toolbar:', JSON.stringify(await page.$$eval('#ex_map .ggiraph-toolbar-icon',
    is => is.map(i => [i.getAttribute('title'), getComputedStyle(i).fill]))));
  await shot('04_explore_toolbar');
  // The funnel plot's toolbar has no zoom with rectangle
  await page.hover('#ex_funnel svg');
  await page.waitForTimeout(800);
  console.log('EXPLORE funnel toolbar:', JSON.stringify(await page.$$eval('#ex_funnel .ggiraph-toolbar-icon',
    is => is.map(i => i.getAttribute('title')))));

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

  // When the app stops, the page says so: here the connection is closed
  await page.evaluate(() => Shiny.shinyapp.$socket.close());
  await page.waitForSelector('.epi-stopped', { timeout: 15000 });
  console.log('STOPPED:', await text('.epi-stopped'));

  await browser.close();
})().catch(e => { console.log('SCRIPT ERROR:', e.message); process.exit(1); });
