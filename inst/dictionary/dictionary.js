// inst/dictionary/dictionary.js
// The HTML data dictionary's tabs, its search and its buttons for the value
// labels, written into the file. The Columns tab shows first; the search
// bar and buttons stay in view above the table's headings, whose place
// below them (--tools-height) follows the bar's height. The search reads
// the dictionary from its JSON (#dictionary-data), whose columns are the
// table's rows in order: a column is shown when its name, label, type or
// notes hold the text, or a value label does, in which case its value
// labels are opened. Printing opens every list of value labels.
(function () {
  var data = JSON.parse(document.getElementById("dictionary-data").textContent);
  var rows = Array.prototype.slice.call(
    document.querySelectorAll("table.columns > tbody > tr"));
  var index = data.columns.map(function (c) {
    return {
      main: [c.column, c.description, c.type].concat(c.notes).join(" ").toLowerCase(),
      values: c.values.join(" ").toLowerCase()
    };
  });
  var search = document.getElementById("search");
  var count = document.getElementById("count");
  var tools = document.querySelector(".tools");
  var tabs = Array.prototype.slice.call(
    document.querySelectorAll('.tabs [role="tab"]'));

  function fit() {
    document.documentElement.style.setProperty(
      "--tools-height", tools.offsetHeight + "px");
  }
  function show(id) {
    tabs.forEach(function (tab) {
      var panel = tab.getAttribute("aria-controls");
      tab.setAttribute("aria-selected", panel === id ? "true" : "false");
      document.getElementById(panel).hidden = panel !== id;
    });
    fit();
  }
  tabs.forEach(function (tab) {
    tab.addEventListener("click", function () {
      show(tab.getAttribute("aria-controls"));
    });
  });
  document.body.classList.add("tabbed");
  show("columns");
  window.addEventListener("resize", fit);

  function setOpen(open) {
    rows.forEach(function (row) {
      var details = row.querySelector("details");
      if (details && !row.hidden) details.open = open;
    });
  }

  search.addEventListener("input", function () {
    var text = search.value.trim().toLowerCase();
    var shown = 0;
    rows.forEach(function (row, i) {
      var inValues = text !== "" && index[i].values.indexOf(text) >= 0;
      var show = text === "" || inValues || index[i].main.indexOf(text) >= 0;
      var details = row.querySelector("details");
      row.hidden = !show;
      if (details && text !== "") details.open = inValues;
      if (show) shown += 1;
    });
    count.textContent = text === "" ? "" :
      shown + " of " + rows.length + " columns";
  });
  document.getElementById("expand").addEventListener("click", function () {
    setOpen(true);
  });
  document.getElementById("collapse").addEventListener("click", function () {
    setOpen(false);
  });
  window.addEventListener("beforeprint", function () {
    rows.forEach(function (row) {
      var details = row.querySelector("details");
      if (details) details.open = true;
    });
  });
})();
