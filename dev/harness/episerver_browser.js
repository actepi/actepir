// dev/harness/episerver_browser.js
//
// Baseline browser checks of the Dataset Browser on synthetic data
// (fake_browse_episerver() in tests/testthat/helper-browse.R). Run with
//   Rscript dev/harness/run_app.R dev/harness/episerver_browser.js <output dir> browse
// It prints one line per check and saves screenshots to the output directory.

const { chromium } = require('playwright');
const fs = require('fs');

(async () => {
  const [url, out] = process.argv.slice(2);
  fs.mkdirSync(out, { recursive: true });
  // Scrollbars shown, as on Windows: Playwright hides them by default
  const browser = await chromium.launch({ ignoreDefaultArgs: ['--hide-scrollbars'] });
  const context = await browser.newContext({ viewport: { width: 1920, height: 950 } });
  // Copy Code and Copy report write to the clipboard
  await context.grantPermissions(['clipboard-read', 'clipboard-write'], { origin: url });
  const page = await context.newPage();
  page.on('pageerror', e => console.log('PAGE ERROR:', e.message));
  page.on('console', m => { if (m.type() === 'error') console.log('CONSOLE ERROR:', m.text()); });

  // Waits until Shiny has been idle for 600ms, as one choice can take two
  // round trips (the Table list is updated, then the table read), then a
  // little longer for widgets to draw
  const settle = async (ms = 800) => {
    await page.waitForTimeout(200);
    for (let quiet = 0; quiet < 3; ) {
      const busy = await page.evaluate(() => document.documentElement.classList.contains('shiny-busy'));
      quiet = busy ? 0 : quiet + 1;
      await page.waitForTimeout(200);
    }
    await page.waitForTimeout(ms);
  };
  const text = async sel => ((await page.textContent(sel)) || '').replace(/\s+/g, ' ').trim();
  const background = sel => page.$eval(sel, e => getComputedStyle(e).backgroundColor);
  const clipboard = () => page.evaluate(() => navigator.clipboard.readText());
  const shot = name => page.screenshot({ path: `${out}/${name}.png` });
  // Inputs are set through their widgets, as a choice in the page would be
  const select = async (id, value) => {
    await page.evaluate(([id, v]) => $('#' + id)[0].selectize.setValue(v), [id, value]);
    await settle();
  };
  const info = () => text('#info_bar');
  const rows = () => page.$$eval('#col_table tbody tr', r => r.length);

  await page.goto(url);
  await page.waitForSelector('#col_table tbody tr', { timeout: 60000 });
  await settle();

  // Layout: the name at the left of the top bar, the tabs after a gap, the
  // logo at the right and no Done; the sidebar's sections and settings
  console.log('TABS:', JSON.stringify(await page.$$eval('.navbar-nav a.nav-link', as => as.map(a => a.textContent.trim()))));
  console.log('TOP BAR:', JSON.stringify(await page.evaluate(() => {
    const r = s => document.querySelector(s).getBoundingClientRect();
    const brand = r('.navbar-brand'), tab = r('.navbar-nav a.nav-link'), logo = r('.navbar .epi-logo');
    return { name: document.querySelector('.navbar-brand').textContent.trim(),
             nameLeft: Math.round(brand.left), gapToTabs: Math.round(tab.left - brand.right),
             middles: [brand, tab, logo].map(b => Math.round(b.top + b.height / 2)),
             logoRight: Math.round(window.innerWidth - logo.right), logoHeight: Math.round(logo.height),
             barHeight: Math.round(r('.navbar').height), done: !!document.querySelector('#done') };
  })));
  console.log('SIDEBAR:', JSON.stringify(await page.$$eval('.accordion-item', items => items.map(i => [
    i.querySelector('.accordion-title').textContent.trim(),
    [...i.querySelectorAll('label.control-label, .checkbox label')].map(l => l.textContent.trim())
  ]))));
  console.log('SIDEBAR width:', await page.$eval('aside#settings', s => Math.round(s.getBoundingClientRect().width)),
    '| foot:', JSON.stringify(await page.$$eval('.epi-actions > *', es => es.map(e => e.id || e.querySelector('[id]').id))),
    '| resizable:', await page.$eval('aside#settings', s => !!s.closest('.bslib-sidebar-layout').querySelector('.bslib-sidebar-resize-handle')));
  // Section headings: the info bar's tint, rgb(234, 230, 241), in bold
  // Nightshade, rgb(50, 5, 87)
  console.log('HEADINGS:', JSON.stringify(await page.$eval('.accordion-button', b => {
    const s = getComputedStyle(b);
    return [s.backgroundColor, s.color, s.fontWeight];
  })));
  // Labels, boxes and gaps take the shared spacing
  console.log('SPACING:', JSON.stringify(await page.evaluate(() => {
    const label = getComputedStyle(document.querySelector('#db-label'));
    const box = getComputedStyle(document.querySelector('#db + .selectize-control .selectize-input'));
    const field = getComputedStyle(document.querySelector('#db').closest('.shiny-input-container'));
    return { label: [label.fontSize, label.fontWeight], box: box.fontSize, gap: field.marginBottom };
  })));

  // Each information button shows its notes and hides them again
  for (const id of await page.$$eval('aside#settings .epi-help', ds => ds.map(d => d.id))) {
    // A closed <details> hides its content with content-visibility
    const visible = () => page.$eval(`#${id} .epi-note`, e => e.checkVisibility());
    const before = await visible();
    await page.click(`#${id} > summary`);
    await page.waitForTimeout(300);
    const open = await visible();
    await page.click(`#${id} > summary`);
    await page.waitForTimeout(300);
    console.log(`HELP ${id}: hidden ${!before}, shown ${open}, hidden again ${!(await visible())}`);
  }

  // The Table list: tables, then views, each with its dataset label muted;
  // the chosen table shows its name alone. Typing finds a table by its label.
  console.log('DATABASES:', JSON.stringify(await page.evaluate(() => Object.keys($('#db')[0].selectize.options))));
  console.log('INFO first table:', await info());
  await page.click('#table + .selectize-control .selectize-input');
  await page.waitForTimeout(400);
  console.log('TABLE groups:', JSON.stringify(await page.$$eval('#table + .selectize-control .optgroup-header', hs => hs.map(h => h.textContent.trim()))));
  console.log('TABLE options:', JSON.stringify(await page.$$eval('#table + .selectize-control .option', os => os.map(o => {
    const label = o.querySelector('.browse-label');
    return label ? [o.firstChild.textContent.trim(), label.textContent] : [o.textContent.trim()];
  }))));
  // Muted, and light on the chosen option's Nightshade
  console.log('TABLE label colours (chosen, other):', JSON.stringify(await page.$$eval('#table + .selectize-control .option .browse-label', ls => ls.slice(0, 2).map(l => {
    const s = getComputedStyle(l);
    return [l.closest('.option').classList.contains('selected'), s.color, s.opacity];
  }))));
  await shot('01_table_list');
  // A single select takes typing once its item is deleted
  await page.keyboard.press('Backspace');
  await page.keyboard.type('emergency');
  await page.waitForTimeout(400);
  console.log('TABLE search "emergency":', JSON.stringify(await page.$$eval('#table + .selectize-control .option', os => os.map(o => o.textContent.trim()))));
  await page.keyboard.press('Enter');
  await settle();
  console.log('TABLE item:', await text('#table + .selectize-control .selectize-input .item'), '|', await info());

  // Where each table's labels come from, as the info bar says
  for (const t of ['Deaths', 'vw_ED', 'Plain', 'Births', 'APC']) {
    await select('table', t);
    console.log(`INFO ${t}:`, await info());
  }

  // APC: the columns with their labels, value labels (shown by default)
  // and notes; the buttons above them; nothing between the info bar and
  // the table but the buttons
  const tabs = () => page.$$eval('.navbar-nav a.nav-link', as => as.map(a => a.textContent.replace(/\s+/g, ' ').trim()));
  console.log('TABS for APC:', JSON.stringify(await tabs()));
  console.log('COLUMNS listed:', await rows(), '| headings:',
    JSON.stringify(await page.$$eval('#col_table .dataTables_scrollHead th', ths => ths.map(t => t.textContent.trim()))));
  console.log('DESCRIPTIONS:', JSON.stringify(await page.$$eval('#col_table tbody tr', trs => trs.slice(0, 3).map(tr =>
    [...tr.children].map(td => td.textContent.trim())))));
  console.log('TOOLS:', JSON.stringify(await page.$$eval('.tab-pane[data-value=columns] .options-row > *', es => es.map(e =>
    [e.id || e.className, e.textContent.trim(), e.disabled === undefined ? null : e.disabled]))),
    '| table header below info bar by', await page.evaluate(() => Math.round(
      document.querySelector('#col_table .dataTables_scrollHead').getBoundingClientRect().top -
      document.querySelector('#info_bar .info-bar').getBoundingClientRect().bottom)), 'px');
  await shot('02_columns');

  // The Collection tab: the description, notes and linked tables
  await page.click('a[data-value=collection]');
  await page.waitForTimeout(600);
  console.log('COLLECTION:', await text('#about'));
  await shot('02b_collection');
  await select('table', 'Plain');
  console.log('COLLECTION Plain:', await text('#about'), '| tabs:', JSON.stringify(await tabs()));
  await select('table', 'APC');
  await page.click('a[data-value=columns]');
  await settle();

  // The table fits the window below its top, so the page needs no scrolling
  for (const [w, h] of [[1920, 950], [1536, 730]]) {
    await page.setViewportSize({ width: w, height: h });
    await page.waitForTimeout(600);
    console.log(`FIT ${w} x ${h}:`, JSON.stringify(await page.evaluate(() => {
      const body = document.querySelector('#col_table .dataTables_scrollBody');
      const r = body.getBoundingClientRect();
      return { tableBottom: Math.round(r.bottom), window: window.innerHeight,
               bodyScrolls: body.scrollHeight > body.clientHeight,
               pageScrolls: document.scrollingElement.scrollHeight > window.innerHeight };
    })));
    if (w === 1536) await shot('03_fit_1536x730');
  }
  await page.setViewportSize({ width: 1920, height: 950 });
  await page.waitForTimeout(600);

  // Value labels: shown by default; Expand and Collapse open and close
  // every list; opening a list does not choose its row
  const chosenRows = () => page.$$eval('#col_table tbody tr.selected, #col_table tbody tr.active', r => r.length);
  const openLists = () => page.$$eval('#col_table td.browse-values details[open]', ds => ds.length);
  console.log('VALUE LABELS shown:', await page.$eval('#show_values', e => e.checked),
    '| lists:', await page.$$eval('#col_table td.browse-values details', ds => ds.length));
  await page.click('#values_open');
  await page.waitForTimeout(300);
  console.log('VALUE LABELS expanded:', await openLists(), '|', await text('#col_table td.browse-values details[open] div'));
  await shot('04_value_labels');
  await page.click('#values_close');
  await page.waitForTimeout(300);
  console.log('VALUE LABELS collapsed:', await openLists());
  await page.click('#col_table td.browse-values details summary');
  await page.waitForTimeout(300);
  console.log('VALUE LABELS one opened:', await openLists(), '| rows chosen:', await chosenRows());
  // Without value labels, Expand and Collapse are disabled, and look it
  await page.click('#show_values');
  await settle();
  console.log('VALUE LABELS off: buttons', JSON.stringify(await page.$$eval('#values_open, #values_close', bs => bs.map(b =>
    [b.disabled, getComputedStyle(b).opacity]))), '| column shown:',
    await page.$$eval('#col_table .dataTables_scrollHead th', ths => ths.some(t => t.textContent.trim() === 'Value labels')));
  await shot('04b_value_labels_off');
  await page.click('#show_values');
  await settle();
  console.log('VALUE LABELS on again: buttons disabled', JSON.stringify(await page.$$eval('#values_open, #values_close', bs => bs.map(b => b.disabled))));

  // Rows chosen give the select() in the code. They are Lavender with
  // Nightshade text, with an Iris bar at their left.
  await page.click('#col_table tbody tr:nth-child(2) td:nth-child(2)');
  await page.click('#col_table tbody tr:nth-child(3) td:nth-child(2)');
  await page.waitForTimeout(500);
  console.log('ROWS chosen:', await chosenRows(), '|', await text('#chosen_note'),
    '| Clear selection disabled:', await page.$eval('#clear_rows', b => b.disabled));
  console.log('ROWS colours (first cell, other cell):', JSON.stringify(await page.$$eval('#col_table tbody tr.selected td', tds => {
    const s = e => { const c = getComputedStyle(e); return [c.boxShadow, c.color]; };
    return [s(tds[0]), s(tds[1])];
  })));
  await shot('05_rows_chosen');

  // Buttons go to Iris, rgb(86, 44, 140), on hover
  const before = await background('#insert_code');
  await page.hover('#insert_code');
  await page.waitForTimeout(400);
  console.log('HOVER #insert_code', before, '->', await background('#insert_code'));

  await page.click('#insert_code');
  await page.waitForSelector('.shiny-notification');
  console.log('NOTIFICATION:', await text('.shiny-notification-content-text'), '|',
    JSON.stringify(await page.$eval('.shiny-notification', e => {
      const s = getComputedStyle(e);
      return [s.backgroundColor, s.color, s.borderLeftColor];
    })));
  console.log('CODE:', JSON.stringify(await clipboard()));
  await page.mouse.move(1200, 600);
  await page.waitForTimeout(2500);

  // Clear selection clears the rows chosen, and is then disabled
  await page.click('#clear_rows');
  await settle();
  console.log('CLEAR SELECTION: rows chosen', await chosenRows(), '|', await text('#chosen_note'),
    '| disabled:', await page.$eval('#clear_rows', b => b.disabled));

  // A label table chosen is read alone, for the columns and the list
  await select('labels_source', 'ref.DataLabels');
  console.log('LABELS ref.DataLabels:', await info(), '|',
    JSON.stringify(await page.evaluate(() => $('#table')[0].selectize.options.APC.label)));
  // The code names the label table chosen
  await page.click('#insert_code');
  await page.waitForTimeout(600);
  console.log('CODE ref.DataLabels:', JSON.stringify(await clipboard()));
  await select('labels_source', 'dbo.DataLabels');
  console.log('LABELS dbo.DataLabels:', await info());
  await select('labels_source', 'auto');

  // Download: the table's dictionary as PDF and HTML, in a menu that opens
  // upwards. Without Quarto, the PDF's item is disabled and not a link.
  await page.click('#dict_download');
  await page.waitForTimeout(400);
  console.log('DOWNLOAD MENU:', JSON.stringify(await page.$$eval(
    '#dict_download + .dropdown-menu .dropdown-item',
    items => items.map(i => [i.textContent.trim(), i.classList.contains('disabled')]))),
    '| opens', await page.evaluate(() => {
      const menu = document.querySelector('#dict_download + .dropdown-menu').getBoundingClientRect();
      return menu.bottom <= document.querySelector('#dict_download').getBoundingClientRect().top ? 'up' : 'down';
    }));
  await shot('08_download_menu');
  for (const id of ['dict_html', 'dict_pdf']) {
    if (!(await page.$('#' + id))) continue;
    if (!(await page.isVisible('#' + id))) await page.click('#dict_download');
    const [download] = await Promise.all([page.waitForEvent('download'), page.click('#' + id)]);
    const file = `${out}/${download.suggestedFilename()}`;
    await download.saveAs(file);
    console.log('DOWNLOAD', id + ':', download.suggestedFilename(), '| starts',
      JSON.stringify(fs.readFileSync(file).subarray(0, 5).toString()));
    if (id !== 'dict_html') continue;
    // The HTML file on its own: its heading, columns and search
    const html = await context.newPage();
    await html.goto('file://' + file);
    await html.fill('#search', 'female');
    console.log('DICTIONARY HTML:', await html.textContent('h1'), '|',
      (await html.textContent('.source')).trim(), '| rows',
      await html.$$eval('table.columns > tbody > tr', r => r.length), '| search "female":',
      (await html.textContent('#count')).trim(), '| requests',
      JSON.stringify(await html.evaluate(() => performance.getEntriesByType('resource').length)));
    await html.fill('#search', '');
    await html.screenshot({ path: `${out}/08b_dictionary_html.png` });
    await html.close();
  }

  // Dictionaries of all tables: the dialog, then the whole database's
  // dictionaries written to a folder in the output directory
  if (!(await page.isVisible('#dict_bulk'))) await page.click('#dict_download');
  await page.click('#dict_bulk');
  await page.waitForSelector('#bulk_write', { state: 'visible' });
  await page.waitForTimeout(500);
  console.log('BULK DIALOG:', await text('.modal-title'), '|', JSON.stringify(await page.$$eval(
    '#bulk_scope input, #bulk_formats input',
    inputs => inputs.map(i => [i.value, i.checked, i.disabled]))),
    '| folder', JSON.stringify(await page.inputValue('#bulk_dir')),
    '| choose', !!(await page.$('#bulk_choose')));
  await shot('08c_bulk_dialog');
  await page.check('#bulk_scope input[value=db]');
  await page.fill('#bulk_dir', `${out}/bulk`);
  await page.click('#bulk_write');
  await page.waitForFunction(() => /Dictionaries written/.test(
    (document.querySelector('.modal-title') || {}).textContent || ''), null, { timeout: 120000 });
  await page.waitForTimeout(500);
  console.log('BULK DONE:', JSON.stringify(await page.$$eval('.modal-body p', ps => ps.map(p => p.textContent.trim()))),
    '| files', JSON.stringify(fs.readdirSync(`${out}/bulk`, { recursive: true }).filter(f => f.endsWith('.html')).sort()));
  await shot('08d_bulk_done');
  await page.click('.modal-footer button[data-bs-dismiss=modal]');
  await page.waitForTimeout(500);

  // Another schema and a database without label tables
  await select('schema', 'stage');
  console.log('SCHEMA', await page.evaluate(() => $('#schema').val()) + ':', await info());
  await select('db', 'HospitalAPC');
  await page.waitForTimeout(800);
  console.log('DATABASE HospitalAPC:', await info(), '| labels tables:',
    JSON.stringify(await page.evaluate(() => Object.keys($('#labels_source')[0].selectize.options))));
  await select('db', 'Analysis');
  await page.waitForTimeout(800);

  // Help: the shared Help style, without the info bar
  await page.click('a[data-value=help]');
  await page.waitForTimeout(600);
  console.log('HELP tab:', JSON.stringify(await page.$$eval('.help h5', hs => hs.map(h => h.textContent))),
    '| text', await page.$eval('.help', e => getComputedStyle(e).fontSize),
    '| info bar shown', await page.$eval('#info_bar', e => e.checkVisibility()));
  await shot('06_help');
  await page.click('a[data-value=columns]');
  await settle();

  // The bug button opens the problem report; Copy report copies what was
  // described and the details shown
  await page.click('#bug_report');
  await page.waitForSelector('#bug_details:not(:empty)');
  await page.waitForTimeout(800);
  console.log('REPORT dialog:', await text('.modal-title'), '|', await text('.modal-body p'));
  await page.fill('#bug_what', 'The labels looked old.');
  await page.waitForTimeout(600);
  await shot('07_report');
  await page.click('#bug_copy');
  await page.waitForTimeout(800);
  const report = await clipboard();
  const lines = report.split('\n');
  console.log('REPORT headings:', JSON.stringify(lines.filter((l, i) => /^-+$/.test(lines[i + 1] || ''))));
  console.log('REPORT lines:', JSON.stringify(lines.filter(l => /^(The labels|Databases|Label tables|Labels|Errors|Code button|db =|table =|actepir |Browser)/.test(l))));
  await page.click('.modal-footer [data-bs-dismiss=modal]');
  await page.waitForTimeout(600);

  // When the app stops, the page says so: here the connection is closed
  await page.evaluate(() => Shiny.shinyapp.$socket.close());
  await page.waitForSelector('.epi-stopped', { timeout: 15000 });
  console.log('STOPPED:', await text('.epi-stopped'));

  await browser.close();
})().catch(e => { console.log('SCRIPT ERROR:', e.message); process.exit(1); });
