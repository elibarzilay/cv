"use strict";

const texts = {};
const formats = { pdf: "PDF", txt: "Text", docx: "Word" };

const $  = x => document.querySelector(x);
const $$ = x => Array.from(document.querySelectorAll(x));

const sleep = ms => new Promise(res => setTimeout(res, ms));

const downloadIcon = [
  `<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"`,
  ` viewBox="0 -960 960 960" fill="currentColor">`,
  `<path d="M480-320 280-520l56-58 104 104v-326h80v326l104-104 56 58-200`,
  ` 200ZM240-160q-33 0-56-23t-24-57v-120h80v120h480v-120h80v120q0`,
  ` 33-23 57t-57 23H240Z"/>`,
  `</svg>`,
].join("");
const viewIcon = [
  `<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"`,
  ` viewBox="0 0 1024 1024" fill="currentColor">`,
  `<path d="M953 496c-7-9-152-221-443-221A547 547 0 0 0 68 496a28 28 0 0 0 0`,
  ` 31c6 9 152 222 442 222 291 0 436-213 443-222a28 28 0 0 0 0-31zM510`,
  ` 693a501 501 0 0 1-383-181 499 499 0 0 1 767 0 499 499 0 0 1-384 181z"/>`,
  `<path d="M510 386a126 126 0 1 0 1 252 126 126 0 0 0-1-252zm0 196a70`,
  ` 70 0 1 1 1-140 70 70 0 0 1-1 140z"/>`,
  `</svg>`,
].join("");

let shown = null;

const show = () => {
  const layoutMode =
    getComputedStyle(document.documentElement).getPropertyValue('--layout-mode').trim();
  const savedScroll = layoutMode === "small" && window.scrollY;
  $("#versions").classList.toggle("flip", shown !== Object.keys(texts)[0]);
  $("#text").innerHTML = texts[shown];
  const headers = $$("#text h1, #text h2, #text h3, #text h4");
  document.title = headers[0].textContent;
  $("#toc-links").innerHTML =
    headers.map((h, i) => {
      h.id = `S${i}`;
      const [tag, title] = !i ? ["H2", "Intro"] : [h.tagName, h.innerHTML];
      return `<${tag}><a href="#${h.id}" tabindex="-1">${title}</a></${tag}>`;
    }).join("\n");
  $$("#toc-links a").forEach(a => a.addEventListener("click", e => {
    e.preventDefault(); e.stopImmediatePropagation();
    const elt = $(e.target.getAttribute("href"));
    jumpTo(elt.parentNode, elt, "start");
  }));
  $$("span.what").forEach(s => s.textContent = shown);
  $("#text").focus();
  if (savedScroll) window.scrollTo(0, savedScroll);
  else $("#text-row").scrollTo(0, 0);
};

const jumpTo = async (elt, hilite, where = "center") => {
  if (typeof elt === "string") elt = document.getElementById(elt);
  if (!elt) {
    if (shown !== "long")
      longVerPopup("This event is described in the long version.", "#828");
    return;
  }
  while (elt.textContent === "") elt = elt.parentNode;
  if (!hilite) hilite = elt;
  const style = props => Object.assign(hilite.style, props)
  style({ boxShadow: "inset 0 0 0 9999px #f806",
          transition: "box-shadow 0.3s ease-in-out",
          borderRadius: "5px" });
  elt.scrollIntoView({ behavior: "smooth", block: where });
  await sleep(2000);
  style({ boxShadow: "inset 0 0 0 9999px #f800",
          transition: "box-shadow 1s ease-in-out" });
  await sleep(1000);
  style({ background: "", transition: "", borderRadius: "" });
};

const longVerPopup = async (msg, bgcolor, seconds = 1200) => {
  const popup = $("#verpopup");
  popup.style.setProperty("--bg", bgcolor);
  popup.innerHTML = msg;
  popup.classList.add("on");
  clearTimeout(longVerPopup.timeout);
  longVerPopup.timeout = setTimeout(() => popup.classList.remove("on"), seconds);
};

const mimicClick = e => {
  if (e.key !== " " && e.key !== "Enter") return;
  e.preventDefault();
  e.target.click();
};

const init = ()=>{
  const addText = node => {
    if (!node) return;
    const txt = node.data.trim().replace(/^--+\n|\n--+$/g, "").trim();
    const nl = txt.indexOf("\n");
    if (nl < 0) return;
    const [what, contents] = [txt.substring(0, nl), txt.substring(nl + 1)];
    if (!(what.match(/^[a-z]+$/) && contents.startsWith("<"))) return;
    // wrap each section in a div for the sticky headers
    texts[what] = `<div>\n${contents}\n</div>`
      .replaceAll(/<([hH][1-6])>.*?<\/\1>/g, "</div><div>\n$&")
      .replace(/^\s*<div>\s*<\/div>\s*/, "");
    return true;
  };
  { const n = document.createNodeIterator(
      document.head, NodeFilter.SHOW_COMMENT, ()=> NodeFilter.FILTER_ACCEPT);
    while (addText(n.nextNode())) { }
  }
  { // the CSS is expecting exactly two versions
    const versions = Object.keys(texts);
    const elt = $("#versions");
    elt.tabIndex = 0;
    elt.setAttribute("role", "button");
    elt.setAttribute("aria-label", "Toggle version");
    elt.innerHTML =
      versions.map(what => `<span>${what}</span>`)
        .join(`<div class="switch"></div>`);
    elt.addEventListener("click", () => {
      shown = versions[(versions.indexOf(shown) + 1) % versions.length];
      history.replaceState(null, "", `/${shown}`);
      show();
      if (shown === "long" && localStorage.longWarning !== "yes") {
        // show long version warning once, but allow a few flicks to read it
        setTimeout(() => localStorage.longWarning = "yes", 10_000);
        longVerPopup("Note: this is the long version,<br>which is too detailed for most uses.",
                     "#263", 2000);
      }
    });
    elt.addEventListener("keydown", mimicClick);
  }
  { $("#formats").innerHTML =
      (Object.entries(formats)).map(([ext, name]) => [
        `<div class="get" data-file="download/Eli_Barzilay-VER.${ext}"`,
        ` data-name="${name}">`,
        `<span data-op="download" tabindex="0" role="button">${
          downloadIcon}&#x200A;${name}</span>`,
        `<span data-op="view" tabindex="0" role="button">${viewIcon}</span>`,
        `</div>`,
      ].join("")).join("\n");
    const a = document.createElement("a");
    const popup = $("#getpopup");
    popup.innerHTML = [
      `<span class="op"></span>`,
      ` the <span class="what"></span> version in`,
      ` <span class="format"></span> format`,
      `<span class="fmt-warn"></span>`,
    ].join("");
    const spans = $$("#formats div.get span");
    spans.forEach(span => {
      const b = span.parentElement;
      span.addEventListener("click", () => {
        const file = b.dataset.file.replace(/\bVER\b/, shown);
        a.href = file, a.download = file.replace(/^.*\//, "");
        $("#text").focus();
        if (span.dataset.op === "view") location = a.href; else a.click();
      });
      span.addEventListener("keydown", mimicClick);
      const setText = unfocus => () => {
        if (unfocus) document.activeElement.blur();
        spans.forEach(s => s.style.anchorName = s !== span ? "" : "--getpopup");
        $("span.fmt-warn").innerHTML =
          b.dataset.name !== "Word" ? ""
          : "<br><i>(Note: the PDF is better formatted)</i>";
        popup.querySelector("span.op").textContent =
          span.dataset.op.toLowerCase().replace(/\b\w/g, c => c.toUpperCase());
        popup.querySelector("span.format").textContent = b.dataset.name.toLowerCase();
      };
      span.addEventListener("mouseenter", setText(true));
      span.addEventListener("focus", setText(false));
    });
  }
  { // set path to root if invalid
    const p = location.pathname.slice(1);
    if (p in texts) {
      shown = p;
    } else {
      if (p) history.replaceState(null, "", "/");
      shown = Object.keys(texts)[0];
    }
  }
  show();
};

// hack around chrome/edge bug
const focusHack = ()=> {
  const css = document.head.appendChild(document.createElement("style"));
  css.innerHTML = "#text:focus { outline:none; }";
  $("#text").tabIndex = 0;
  $("#text").focus();
};

window.addEventListener("load", ()=> { init(); renderTimeline(); focusHack(); });
