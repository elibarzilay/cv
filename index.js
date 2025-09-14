"use strict";

const texts = {};
const formats = { pdf: "PDF", txt: "Text", docx: "Word" };
const defaultVersion = "long";

const $  = x => document.querySelector(x);
const $$ = x => Array.from(document.querySelectorAll(x));

const downloadIcon = [
  `<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"`,
  ` viewBox="0 -960 960 960" fill="currentColor">
  <path d="M480-320 280-520l56-58 104 104v-326h80v326l104-104 56 58-200`,
  ` 200ZM240-160q-33 0-56-23t-24-57v-120h80v120h480v-120h80v120q0`,
  ` 33-23 57t-57 23H240Z"/>
</svg>`,
].join("");
const viewIcon = [
  `<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"`,
  ` viewBox="0 0 1024 1024" fill="currentColor">
  <path d="M953 496c-7-9-152-221-443-221A547 547 0 0 0 68 496a28 28 0 0 0 0`,
  ` 31c6 9 152 222 442 222 291 0 436-213 443-222a28 28 0 0 0 0-31zM510`,
  ` 693a501 501 0 0 1-383-181 499 499 0 0 1 767 0 499 499 0 0 1-384 181z"/>`,
  `<path d="M510 386a126 126 0 1 0 1 252 126 126 0 0 0-1-252zm0 196a70`,
  ` 70 0 1 1 1-140 70 70 0 0 1-1 140z"/>
</svg>
`,
].join("");

const show = () => {
  $("#versions").classList.toggle(
    "flip", sessionStorage.shown !== Object.keys(texts)[0]);
  $("#text").innerHTML = texts[sessionStorage.shown];
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
  $$("span.what").forEach(s => s.textContent = sessionStorage.shown);
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
  { // the CSS is expecting exactly two versions
    const versions = Object.keys(texts);
    $("#versions").innerHTML =
      versions.map(what => `<span>${what}</span>`)
        .join(`<div class="switch"></div>`);
    $("#versions").addEventListener("click", () => {
      sessionStorage.shown =
        versions[(versions.indexOf(sessionStorage.shown) + 1) % versions.length];
      show();
    });
  }
  { $("#formats").innerHTML =
      (Object.entries(formats)).map(([ext, name]) => [
        `<div class="get" data-file="download/Eli_Barzilay-VER.${ext}"`,
        ` data-name="${name}">`,
        `<span data-op="download">${downloadIcon}&#x200A;${name}</span>`,
        `<span data-op="view">${viewIcon}</span>`,
        `</div>`,
      ].join("")).join("\n");
    const a = document.createElement("a");
    const popup = $("#popup");
    popup.innerHTML = [
      `Click to <span class="op"></span>`,
      ` the <span class="what"></span> version in`,
      ` <span class="format"></span> format`,
      `<span class="fmt-warn"></span>`,
    ].join("");
    popup.addEventListener("transitionend", ()=> {
      if (!popup.classList.contains("active")) popup.style.display = "none"; });
    $$("#formats div.get span").forEach(span => {
      const b = span.parentElement;
      span.addEventListener("click", () => {
        const file = b.dataset.file.replace(/\bVER\b/, sessionStorage.shown);
        a.href = file, a.download = file.replace(/^.*\//, "");
        $("#text").focus();
        if (span.dataset.op === "view") location = a.href; else a.click();
      });
      span.addEventListener("mouseenter", () => {
        $("span.fmt-warn").innerHTML =
          b.dataset.name !== "Word" ? ""
          : "<br><i>(Note: the PDF is better formatted)</i>";
        popup.querySelector("span.op").textContent = span.dataset.op;
        popup.querySelector("span.format").textContent = b.dataset.name.toLowerCase();
        popup.style.display = "block";
        popup.style.top = (b.getBoundingClientRect().bottom + 4) + "px";
        popup.style.left = (span.offsetLeft - popup.offsetWidth + span.offsetWidth) + "px";
        popup.classList.add("active");
      });
      b.addEventListener("mouseleave", ()=> popup.classList.remove("active"));
    });
  }
  sessionStorage.shown ??= defaultVersion;
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
