"use strict";

const texts = {};
const formats = { pdf: "PDF", docx: "Word", txt: "Text" };
let curVersion = "";

const $  = x => document.querySelector(x);
const $$ = x => Array.from(document.querySelectorAll(x));

const downloadIcon = [
  `<svg xmlns="http://www.w3.org/2000/svg" height="24px"`,
  ` viewBox="0 -960 960 960" width="24px" fill="currentColor">`,
  `<path d="M480-320 280-520l56-58 104 104v-326h80v326l104-104 56 58-200`,
  ` 200ZM240-160q-33 0-56.5-23.5T160-240v-120h80v120h480v-120h80v120q0`,
  ` 33-23.5 56.5T720-160H240Z"/></svg>`,
].join("");

const show = what => {
  if (typeof what === "number") what = Object.keys(texts)[what];
  curVersion = what;
  $("#text").innerHTML = texts[what];
  const headers = $$("#text h1, #text h2, #text h3, #text h4");
  document.title = headers[0].textContent;
  $("#toc-links").innerHTML =
    headers.map((h, i) => {
      h.id = `S${i}`;
      const [tag, title] = !i ? ["H2", "Intro"] : [h.tagName, h.innerHTML];
      return `<${tag}><a href="#${h.id}">${title}</a></${tag}>`;
    }).join("\n");
  $$("#toc-links a").forEach(a => a.addEventListener("click", e => {
    e.preventDefault(); e.stopImmediatePropagation();
    const elt = $(e.target.getAttribute("href"));
    jumpTo(elt.parentNode, elt, "start");
  }));
  $$("span.what").forEach(s => s.textContent = what);
  $$("#versions button").forEach(b =>
    b.classList.toggle("selected", b.textContent === what));
  $("#text").focus();
  $("#text-row").scrollTo(0, 0);
};

const jumpTo = (elt, hilite, where = "center") => {
  if (typeof elt === "string") elt = document.getElementById(elt);
  while (elt.textContent === "") elt = elt.parentNode;
  if (!hilite) hilite = elt;
  const style = props => Object.assign(hilite.style, props)
  style({ background: "#f806",
          transition: "background 0.3s ease-in-out",
          borderRadius: "5px" });
  setTimeout(()=> { style({ background: "#f800",
                            transition: "background 1s ease-in-out" });
                    setTimeout(()=> style({ background: "", transition: "",
                                            borderRadius: "" }),
                               1000); },
             2000);
  elt.scrollIntoView({behavior: "smooth", block: where});
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
  { // expecting exactly two versions
    const vers = $("#versions");
    vers.innerHTML =
      Object.keys(texts).map((what, i) => `<span class="s${i}">${what}</span>`)
        .join(`<div class="switch"></div>`);
    vers.addEventListener("click", () =>
      show(vers.querySelectorAll("span")[+vers.classList.toggle("on")]
             .textContent));
  }
  { $("#formats").innerHTML = `Download <span class="what"></span> version: `
      + (Object.entries(formats)).map(([ext, name]) =>
          `<button class="dnld" data-file="download/Eli_Barzilay-VER.${ext}"`
          + ` data-name="${name}">${downloadIcon} ${name}</button>`)
        .join("\n");
    const a = document.createElement("a");
    const popup = $("#popup");
    popup.innerHTML = [
      `Click to download the <span class="what"></span> version in`,
      ` <span class="format"></span> format`,
      `<br>Shift+click to view; switch version on the left`,
      `<span class="fmt-warn"></span>`,
    ].join("");
    popup.addEventListener("transitionend", ()=> {
      if (!popup.classList.contains("active")) popup.style.display = "none"; });
    $$("#formats button").forEach(b => {
      b.addEventListener("click", ({target: {dataset: {file}}, shiftKey}) => {
        a.href = file.replace(/\bVER\b/, curVersion);
        a.download = a.href.replace(/^.*\//, "");
        $("#text").focus();
        if (shiftKey) location = a.href; else a.click();
      });
      b.addEventListener("mouseenter", ({target: {dataset: {name}}}) => {
        $$("span.fmt-warn").forEach(s =>
          s.innerHTML = name !== "Word" ? ""
                        : "<br><i>(Note: the PDF is much better formatted)</i>");
        popup.querySelector("span.format").textContent = name.toLowerCase();
        popup.style.display = "block";
        popup.style.top = b.getBoundingClientRect().bottom + "px";
        popup.style.left = (b.offsetLeft - popup.offsetWidth + b.offsetWidth) + "px";
        popup.classList.add("active");
      });
      b.addEventListener("mouseleave", ()=> popup.classList.remove("active"));
    });
  }
  show(0);
};

// hack around chrome/edge bug
const focusHack = ()=> {
  const css = document.head.appendChild(document.createElement("style"));
  css.innerHTML = "#text:focus { outline:none; }";
  $("#text").tabIndex = 0;
  $("#text").focus();
};

window.addEventListener("load", ()=> { init(); renderTimeline(); focusHack(); });
