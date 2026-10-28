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

  // The browser opens on the Tables tab; the checks below start on Columns
  await page.goto(url);
  await page.waitForSelector('#tables_table tbody tr', { timeout: 60000 });
  await settle();
  console.log('OPENS ON:', await text('.navbar-nav a.nav-link.active'), '| columns drawn',
    !!(await page.$('#col_table tbody tr')));
  await page.click('a[data-value=columns]');
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

  // Each information button shows its notes and hides them again: the
  // sidebar's here, the Columns tab's once it is shown
  const helpButtons = async panel => {
    for (const id of await page.$$eval(`${panel} .epi-help`, ds => ds.map(d => d.id))) {
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
  };
  await helpButtons('aside#settings');

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
  // The tab's own panel: the code's settings, Copy Code and Download
  console.log('COLUMNS panel:', JSON.stringify(await page.evaluate(() => {
    const aside = document.querySelector('#columns_settings');
    return { width: Math.round(aside.getBoundingClientRect().width),
             settings: [...aside.querySelectorAll('label.control-label, .checkbox label')].map(l => l.textContent.trim()),
             buttons: [...aside.querySelectorAll('.epi-stack > *')].map(e => (e.id || e.querySelector('[id]').id)) };
  })));
  await helpButtons('#columns_settings');
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
               pageScrolls: document.scrollingElement.scrollHeight > window.innerHeight,
               tableWidth: Math.round(r.width),
               columns: [...document.querySelectorAll('#col_table .dataTables_scrollHead th')].map(th =>
                 [th.textContent.trim(), Math.round(th.getBoundingClientRect().width)]) };
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

  // Notes: a list that opens, as value labels do, without choosing its row
  console.log('NOTES lists:', JSON.stringify(await page.$$eval('#col_table td.browse-notes details summary',
    ss => ss.map(s => s.textContent.trim()))));
  await page.click('#col_table td.browse-notes details summary');
  await page.waitForTimeout(300);
  console.log('NOTES one opened:', await text('#col_table td.browse-notes details[open] div'),
    '| rows chosen:', await chosenRows());
  await page.click('#col_table td.browse-notes details summary');
  await page.waitForTimeout(300);

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
  // downwards from the Columns tab's panel, placed so that the panel does
  // not clip it. Without Quarto, the PDF's item is disabled and not a link.
  await page.click('#dict_download');
  await page.waitForTimeout(400);
  console.log('DOWNLOAD MENU:', JSON.stringify(await page.$$eval(
    '#dict_download + .dropdown-menu .dropdown-item',
    items => items.map(i => [i.textContent.trim(), i.classList.contains('disabled')]))),
    '| opens', await page.evaluate(() => {
      const menu = document.querySelector('#dict_download + .dropdown-menu').getBoundingClientRect();
      return menu.bottom <= document.querySelector('#dict_download').getBoundingClientRect().top ? 'up' : 'down';
    }), '|', JSON.stringify(await page.evaluate(() => {
      const menu = document.querySelector('#dict_download + .dropdown-menu');
      const aside = document.querySelector('#columns_settings');
      return { position: getComputedStyle(menu).position,
               bottom: Math.round(menu.getBoundingClientRect().bottom),
               panelBottom: Math.round(aside.getBoundingClientRect().bottom),
               panelScrolls: aside.scrollHeight > aside.clientHeight + 1 };
    })));
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
    // Its tabs: Columns first; Dataset information with the description
    // and the notes under their themes
    const tabState = () => html.evaluate(() => [...document.querySelectorAll('.tabs [role=tab]')].map(t =>
      [t.textContent, t.getAttribute('aria-selected'), !document.getElementById(t.getAttribute('aria-controls')).hidden]));
    console.log('DICTIONARY HTML tabs:', JSON.stringify(await tabState()));
    await html.click('#tab-information');
    console.log('DICTIONARY HTML information:', JSON.stringify(await tabState()), '|',
      JSON.stringify(await html.$$eval('#information h3, #information h4', hs => hs.map(h => h.tagName + ' ' + h.textContent))));
    await html.screenshot({ path: `${out}/08c_dictionary_information.png` });
    // The search and buttons stick above the table's headings
    await html.click('#tab-columns');
    await html.mouse.wheel(0, 1200);
    await html.waitForTimeout(300);
    console.log('DICTIONARY HTML scrolled:', JSON.stringify(await html.evaluate(() => ({
      scrolled: window.scrollY > 0,
      toolsTop: Math.round(document.querySelector('.tools').getBoundingClientRect().top),
      headingsTop: Math.round(document.querySelector('table.columns th').getBoundingClientRect().top),
      toolsBottom: Math.round(document.querySelector('.tools').getBoundingClientRect().bottom) }))));
    await html.close();
  }

  // Copy dictionary for Excel: the clipboard holds an HTML table, every
  // cell but the position marked as text, and the same rows as text
  if (!(await page.isVisible('#dict_excel'))) await page.click('#dict_download');
  await page.click('#dict_excel');
  await page.waitForTimeout(800);
  const excel = await page.evaluate(async () => {
    const item = (await navigator.clipboard.read())[0];
    return { types: item.types, html: await (await item.getType('text/html')).text(),
             text: await (await item.getType('text/plain')).text() };
  });
  const excelLines = excel.text.split('\n');
  console.log('EXCEL clipboard:', JSON.stringify(excel.types), '| text lines', excelLines.length,
    '| headings', JSON.stringify(excelLines[0]), '| Sex 9', JSON.stringify(excelLines.find(l => l.includes('\t9\t'))),
    '| html rows', (excel.html.match(/<tr>/g) || []).length,
    '| cells as text', (excel.html.match(/mso-number-format/g) || []).length,
    '| notes in one cell', excel.html.includes('mso-data-placement:same-cell'));
  await page.waitForTimeout(2500);

  // Dictionary logo: the actepir logo on the band's Nightshade; a white logo on
  // a transparent background, made here as a screenshot, then the actepir
  // logo again
  if (!(await page.isVisible('#dict_logo'))) await page.click('#dict_download');
  await page.click('#dict_logo');
  await page.waitForSelector('#logo_file', { state: 'attached' });
  await settle(400);
  const logoShown = () => page.evaluate(() => {
    const band = document.querySelector('#logo_shown .browse-logo');
    const img = band.querySelector('img');
    return { band: getComputedStyle(band).backgroundColor, src: img ? img.src.slice(0, 22) : null,
             height: img ? Math.round(img.getBoundingClientRect().height) : null,
             notes: [...document.querySelectorAll('#logo_shown .epi-note')].map(p => [p.className.replace('epi-note', '').trim(), p.textContent.trim()]) };
  });
  console.log('LOGO dialog:', await text('.modal-title'), '|', JSON.stringify(await logoShown()));
  const maker = await context.newPage();
  await maker.setContent('<div id=l style="width:640px;height:200px;font:bold 120px sans-serif;color:#fff">LOGO</div>');
  const logoPng = `${out}/white_logo.png`;
  await maker.locator('#l').screenshot({ path: logoPng, omitBackground: true });
  await maker.close();
  await page.setInputFiles('#logo_file', logoPng);
  await settle();
  console.log('LOGO set:', JSON.stringify(await logoShown()));
  await shot('08c_logo_dialog');
  await page.click('#logo_reset');
  await settle();
  console.log('LOGO actepir logo again:', JSON.stringify(await logoShown()));
  await page.click('.modal-footer button[data-bs-dismiss=modal]');
  await settle(400);

  // Dictionaries of all tables: the dialog, the folder list, then the whole
  // database's dictionaries written to a folder in the output directory
  if (!(await page.isVisible('#dict_bulk'))) await page.click('#dict_download');
  await page.click('#dict_bulk');
  await page.waitForSelector('#bulk_write', { state: 'visible' });
  await settle(500);
  // The buttons above the list, the folders in it and the note
  const folders = () => page.evaluate(() => ({
    buttons: [...document.querySelectorAll('#bulk_folders .options-row .browse-folder')]
      .map(b => [b.textContent.trim(), b.disabled]),
    list: [...document.querySelectorAll('#bulk_folders .list-group-item')].map(b => b.textContent.trim()),
    note: ((document.querySelector('#bulk_folders .epi-note') || {}).textContent || '').trim()
  }));
  console.log('BULK DIALOG:', await text('.modal-title'), '|', JSON.stringify(await page.$$eval(
    '#bulk_scope input, #bulk_formats input',
    inputs => inputs.map(i => [i.value, i.checked, i.disabled]))),
    '| folder', JSON.stringify(await page.inputValue('#bulk_dir')),
    '| folders', JSON.stringify(await folders()));
  // The list follows the box; a click on a folder, or Up, fills the box
  for (const d of ['picker/Alpha/Inner', 'picker/beta', 'picker/2024', 'picker/.hidden']) {
    fs.mkdirSync(`${out}/${d}`, { recursive: true });
  }
  const picker = fs.realpathSync(`${out}/picker`);
  await page.fill('#bulk_dir', picker);
  await settle(500);
  console.log('BULK FOLDERS:', JSON.stringify(await folders()), '| list', JSON.stringify(
    await page.$eval('#bulk_folders .browse-folders', e => {
      const s = getComputedStyle(e);
      return [s.maxHeight, s.overflowY, s.overscrollBehaviorY,
              Math.round(e.querySelector('.list-group-item').getBoundingClientRect().height)];
    })));
  await shot('08c_bulk_dialog');
  await page.click('#bulk_folders .list-group-item:has-text("Alpha")');
  await settle(500);
  console.log('BULK INTO:', JSON.stringify(await page.inputValue('#bulk_dir')), JSON.stringify(await folders()));
  await page.click('#bulk_folders .browse-folder:has-text("Up")');
  await settle(500);
  await page.click('#bulk_folders .list-group-item:has-text("2024")');
  await settle(500);
  console.log('BULK UP:', JSON.stringify(await page.inputValue('#bulk_dir')));
  await page.fill('#bulk_dir', `${picker}/new/folder`);
  await settle(500);
  console.log('BULK NEW:', JSON.stringify(await folders()));
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

  // The Tables tab: every table and view of the schema with its rows, dates
  // and labels, each heading's notes in its title; the table chosen is
  // marked, and a click chooses another
  const cells = sel => page.$$eval(`${sel} tbody tr`, trs => trs.map(tr =>
    [...tr.querySelectorAll('td')].map(td => td.textContent.trim())));
  const marked = sel => page.$$eval(`${sel} tbody tr.selected td:first-child`,
    tds => tds.map(td => td.textContent.trim()));
  await page.click('a[data-value=tables]');
  await settle();
  console.log('TABLES note:', await text('#tables_note'), '| info bar shown',
    await page.$eval('#info_bar .info-bar', e => e.checkVisibility()));
  console.log('TABLES headings:', JSON.stringify(await page.$$eval(
    '#tables_table .dataTables_scrollHead th', ths => ths.map(th => [th.textContent.trim(), th.title]))));
  console.log('TABLES rows:', JSON.stringify(await cells('#tables_table')));
  console.log('TABLES marked:', JSON.stringify(await marked('#tables_table')));
  console.log('TABLES fit:', JSON.stringify(await page.evaluate(() => ({
    pageScrolls: document.documentElement.scrollHeight > window.innerHeight,
    tableBottom: Math.round(document.querySelector('#tables_table .dataTables_scrollBody').getBoundingClientRect().bottom),
    window: window.innerHeight }))));
  await shot('09_tables');
  // At 1536 by 730 the columns still fit across, with no sideways scrolling
  await page.setViewportSize({ width: 1536, height: 730 });
  await settle(400);
  console.log('TABLES at 1536 x 730:', JSON.stringify(await page.evaluate(() => {
    const body = document.querySelector('#tables_table .dataTables_scrollBody');
    return { sideways: body.scrollWidth > body.clientWidth,
             pageScrolls: document.documentElement.scrollHeight > window.innerHeight };
  })));
  await shot('09b_tables_1536x730');
  await page.setViewportSize({ width: 1920, height: 950 });
  await settle(400);
  await page.click('#tables_table tbody tr:nth-child(5) td:first-child');
  await settle();
  console.log('TABLES click row 5:', await info(), '| marked', JSON.stringify(await marked('#tables_table')));
  await select('table', 'ACTGHSMYX');
  console.log('TABLES sidebar ACTGHSMYX: marked', JSON.stringify(await marked('#tables_table')));

  // The Var Finder tab: its settings in a panel at its left; Enter, or the
  // button straight after typing, searches the text typed; a click on a
  // column opens its table on the Columns tab
  await page.click('a[data-value=search]');
  await settle();
  console.log('SEARCH panel:', JSON.stringify(await page.evaluate(() => ({
    width: Math.round(document.querySelector('#search_settings').getBoundingClientRect().width),
    look: [...document.querySelectorAll('#search_in input')].map(i => [i.value, i.checked]) }))),
    '| note', await text('#search_note'));
  await page.fill('#search_text', 'female');
  await page.press('#search_text', 'Enter');
  await settle();
  console.log('SEARCH female (Enter):', await text('#search_note'), '|', JSON.stringify(await cells('#search_table')),
    '| filter box', !!(await page.$('#search_table .dataTables_filter')));
  await shot('10_search');
  await page.fill('#search_text', 'staged');
  await page.click('#search_go');
  await settle();
  console.log('SEARCH staged (button at once):', await text('#search_note'), '|', JSON.stringify(await cells('#search_table')));
  await page.click('#search_table tbody tr:first-child td:first-child');
  await settle();
  console.log('SEARCH open:', await text('.navbar-nav a.nav-link.active'), '|', await page.evaluate(() => $('#schema').val()),
    await page.evaluate(() => $('#table').val()), '|', await info());
  await select('schema', 'dbo');

  // The Profiler: its lists draw over the page, so the panel does not
  // scroll; the summary is in the panel; the table fits the window; PatientID,
  // the primary key, is left out; By gives a cross tab
  const summaryRows = () => page.$$eval('#profile_summary tr',
    trs => trs.map(tr => [tr.querySelector('th').textContent, tr.querySelector('td').textContent]));
  // The panel and its sections, but not the box of the list, which scrolls
  // by design
  const panelScrolls = () => page.evaluate(() => {
    const aside = document.querySelector('#profile_settings');
    return [aside, ...aside.querySelectorAll('div')].some(e =>
      /(auto|scroll)/.test(getComputedStyle(e).overflowY) && e.scrollHeight > e.clientHeight + 1);
  });
  // Any area that scrolls, but the tables' own bodies, the sidebar and the
  // lists' options
  const pageScrolls = () => page.evaluate(() => [...document.querySelectorAll('div, main, body')].some(e =>
    !e.closest('.dataTables_scrollBody') && !e.closest('aside#settings') && !e.closest('.selectize-dropdown') &&
    /(auto|scroll)/.test(getComputedStyle(e).overflowY) && e.scrollHeight > e.clientHeight + 1) ||
    document.documentElement.scrollHeight > window.innerHeight);
  await select('table', 'APC');
  await page.click('a[data-value=profile]');
  await settle();
  console.log('PROFILER panel:', JSON.stringify(await page.evaluate(() => ({
    width: Math.round(document.querySelector('#profile_settings').getBoundingClientRect().width),
    column: $('#profile_column').val(),
    options: Object.keys($('#profile_column')[0].selectize.options).slice(0, 3),
    by: Object.keys($('#profile_by')[0].selectize.options).slice(0, 3) }))),
    '| left out', await text('#profile_left_out'), '| note', await text('#profile_note'));
  await page.click('#profile_column + .selectize-control .selectize-input');
  await page.waitForTimeout(500);
  console.log('PROFILER list open: drawn on', await page.evaluate(() =>
    document.querySelector('#profile_column + .selectize-control .selectize-dropdown, body > .selectize-dropdown').parentElement.tagName),
    '| panel scrolls', await panelScrolls(), '| options shown',
    await page.$$eval('body > .selectize-dropdown .option', os => os.filter(o => o.offsetParent).length));
  await shot('11_profiler_list');
  await page.keyboard.press('Escape');
  await select('profile_column', 'Sex');
  await page.click('#profile_go');
  await settle();
  console.log('PROFILER Sex:', await text('#profile_note'), '|', JSON.stringify(await summaryRows()));
  console.log('PROFILER counts:', JSON.stringify(await cells('#profile_table')));
  await shot('12_profiler');
  // At 1536 by 730 the counts table fits below its top, and the page does
  // not scroll
  await page.setViewportSize({ width: 1536, height: 730 });
  await select('profile_column', 'AgeYrs');
  await page.click('#profile_go');
  await settle();
  console.log('PROFILER at 1536 x 730:', JSON.stringify(await page.evaluate(() => {
    const body = document.querySelector('#profile_table .dataTables_scrollBody');
    return { tableTop: Math.round(body.getBoundingClientRect().top),
             tableBottom: Math.round(body.getBoundingClientRect().bottom) };
  })), '| page scrolls', await pageScrolls(), '| panel scrolls', await panelScrolls());
  await shot('12b_profiler_1536x730');
  await page.setViewportSize({ width: 1920, height: 950 });
  await settle(400);
  // By: a cross tab, then per cents of rows, with no new query
  await select('profile_column', 'Sex');
  await select('profile_by', 'Diagnosis1');
  await page.click('#profile_go');
  await settle();
  console.log('PROFILER Sex by Diagnosis1:', await text('#profile_note'), '| headings', JSON.stringify(await page.$$eval(
    '#profile_table .dataTables_scrollHead th', ths => ths.map(th => [th.textContent.trim(), th.title]))));
  console.log('PROFILER cross tab rows:', JSON.stringify(await cells('#profile_table')));
  await page.check('#profile_show input[value=row]');
  await settle();
  console.log('PROFILER per cent of row:', JSON.stringify((await cells('#profile_table')).slice(0, 2)));
  await shot('12c_profiler_crosstab');
  await page.check('#profile_show input[value=rows]');
  await select('profile_by', '');
  // Columns left out: the list and why each is left out
  await page.click('#profile_settings details.browse-exclude > summary');
  await settle(300);
  console.log('PROFILER left out:', JSON.stringify(await page.inputValue('#profile_exclude')), '|',
    await text('#profile_exclude_note'));
  await shot('12d_profiler_left_out');
  await page.click('#profile_settings details.browse-exclude > summary');
  // Text: blank values, and the longest value cut at 200 characters
  await select('table', 'Deaths');
  await select('profile_column', 'Cause');
  await page.click('#profile_go');
  await settle();
  console.log('PROFILER Deaths.Cause:', JSON.stringify(await summaryRows()),
    '| counts', JSON.stringify((await cells('#profile_table')).map(r => [r[0].length, r[1]])));
  await select('table', 'APC');

  // The whole table: a row for each column kept, read a column at a time;
  // the headings' titles; sorted by codes without labels; the table fits
  // the window; a click on a column profiles it alone
  const shown = sel => page.$eval(sel, e => e.checkVisibility());
  await page.check('#profile_mode input[value=table]');
  await settle();
  console.log('PROFILER whole table, before:', await text('#profile_note'), '| Column shown',
    await shown('#profile_column + .selectize-control'), '| summary shown', await shown('#profile_summary'),
    '| Stop shown', await shown('#profile_stop'));
  await page.click('#profile_go');
  await page.waitForFunction(() => document.querySelector('#profile_note').textContent.startsWith('Profile of'),
    null, { timeout: 60000 });
  await settle();
  const whole = await cells('#profile_table');
  console.log('PROFILER whole table:', await text('#profile_note'), '| rows', whole.length,
    '| Stop shown', await shown('#profile_stop'));
  console.log('PROFILER whole table headings:', JSON.stringify(await page.$$eval(
    '#profile_table .dataTables_scrollHead th', ths => ths.map(th => [th.textContent.trim(), !!th.title]))));
  console.log('PROFILER whole table rows:', JSON.stringify(whole.slice(0, 3)));
  console.log('PROFILER whole table fit:', JSON.stringify(await page.evaluate(() => ({
    tableBottom: Math.round(document.querySelector('#profile_table .dataTables_scrollBody').getBoundingClientRect().bottom),
    window: window.innerHeight }))), '| page scrolls', await pageScrolls(), '| panel scrolls', await panelScrolls());
  await shot('12e_profiler_whole_table');
  const sortBy = async name => page.click(`#profile_table .dataTables_scrollHead th:text-is("${name}")`);
  await sortBy('Codes without labels');
  await sortBy('Codes without labels');
  await settle(300);
  console.log('PROFILER whole table by codes without labels:', JSON.stringify((await cells('#profile_table'))[0]));
  await page.click('#profile_table tbody tr:has(td:text-is("AgeYrs")) td:first-child');
  await settle();
  console.log('PROFILER click AgeYrs:', await page.evaluate(() => $('#profile_mode input:checked').val()), '|',
    await text('#profile_note'), '| Column', await page.evaluate(() => $('#profile_column').val()));
  // Back to the whole table: shown again, not read again
  await page.check('#profile_mode input[value=table]');
  await settle();
  console.log('PROFILER whole table again:', await text('#profile_note'), '| rows', (await cells('#profile_table')).length);
  // The same column clicked again profiles it again
  await select('profile_column', 'Sex');
  await page.check('#profile_mode input[value=table]');
  await settle();
  await page.click('#profile_table tbody tr:has(td:text-is("AgeYrs")) td:first-child');
  await settle();
  console.log('PROFILER click AgeYrs again:', await page.evaluate(() => $('#profile_mode input:checked').val()), '|',
    await page.evaluate(() => $('#profile_column').val()));
  await page.check('#profile_mode input[value=column]');
  await settle();

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
