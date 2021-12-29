"use strict";

const texts = {};
const formats = { pdf: "PDF", docx: "Word", txt: "Text" };
let curVersion = "";

const $  = x => document.querySelector(x);
const $$ = x => Array.from(document.querySelectorAll(x));

const show = what => {
  if (typeof what === "number") what = Object.keys(texts)[what];
  curVersion = what;
  const text = texts[what];
  document.title = text.title;
  $("#text").innerHTML = text.HTML;
  $$("span.what").forEach(s => s.textContent = what);
  $$("#versions button").forEach(b =>
    b.classList.toggle("selected", b.textContent === what));
  $("#toc-links").innerHTML = text.headersHTML;
  $$("#toc-links a").forEach(a => a.addEventListener("click", tocJump));
  setTimelineTargets();
  $("#text").focus();
  $("#text-row").scrollTo(0, 0);
};

const jumpTo = elt => {
  Object.assign(elt.style, {
    backgroundColor: "#f806",
    transition: "background-color 0.3s ease-in-out",
    borderRadius: "5px" });
  setTimeout(()=> Object.assign(elt.style, {
                    backgroundColor: "",
                    transition: "background-color 1s ease-in-out" }),
             1000);
  elt.scrollIntoView({behavior: "smooth", block: "center"});
};

const switchTo = e => show(e.target.textContent);
const tocJump = e => {
  const text = $("#text");
  const hdr = e.target.textContent;
  jumpTo(hdr === "Intro" ? $("#text h1")
         : $$(`#text ${e.target.parentElement.tagName}`)
             .find(h => h.textContent === hdr));
};

const setTimelineTargets = ()=> {
  const headers = Object.fromEntries(
    $$("#text > div > *").filter(n => n.tagName.startsWith("H"))
      .map(h => [h.textContent, h]));
  const blockTags = "P UL OL LI PRE H1 H2 H3 H4 H5 H6".split(/ +/);
  Object.entries(dateInfo).forEach(([sec, xs]) => {
    const dt = dateTypes.find(dt => dt.section === sec);
    let node = headers[sec];
    if (!node) throw Error(`dateInfo section not found: ${sec}`);
    const nodes = [];
    const loop = n => {
      if (!n || (n instanceof Element && n.tagName.startsWith("H"))) return;
      const texts = [];
      n.childNodes.forEach(n =>
        n instanceof Element && blockTags.includes(n.tagName)
          ? (texts.push("\n"), loop(n))
          : texts.push(n));
      const text = texts.map(t => typeof t === "string" ? t : t.textContent)
                        .join("").replaceAll(/\s+/g, " ").trim();
      if (text.length) nodes.push([n, text]);
    };
    while ((node = node.nextSibling)
           // no real need for this, since each section is in its own div,
           // but keep it anyway
           && !(node instanceof Element && node.tagName.startsWith("H")))
      loop(node);
    xs.forEach(x => {
      const anchor = !x.anchor ? x[dt.anchor]
                   : x.anchor in x ? x[x.anchor]
                   : x.anchor;
      const ns = nodes.filter(n => n[1].includes(anchor));
      if (ns.length !== 1)
        console.error(`${ns.length} matches for ${JSON.stringify(x)}`);
      if (!x.go) x.go = function go() { jumpTo(go.to); }
      x.go.to = ns[0][0];
    });
  });
};

const init = ()=>{
  const r = new commonmark.Parser({smart: true});
  const w = new commonmark.HtmlRenderer();
  // const tweakPubs = txt =>
  //   txt.replace(/(?<=\n#+ Publications\n+)((?:(?:[^#\n].*)?\n)+)/, pubs =>
  //     pubs.replaceAll(/\n  - /g, " \\\n   "));
  const md = (txt, tweak) => {
    // italics for all quotes
    // txt = txt.replaceAll(/"[^"]+",?/g, "*$&*");
    if (tweak) txt = tweak(txt);
    txt = w.render(r.parse(txt));
    // - put self-urls in <code>
    txt = txt.replaceAll(/<a href="(?:mailto:)?([^"]+)">\1<\/a>/g, "<code>$&</code>");
    return txt;
  };
  const addText = node => {
    if (!node) return;
    const txt = node.data.trim().replace(/^--+\n|\n--+$/g, "").trim();
    const nl = txt.indexOf("\n");
    if (nl < 0) return;
    const name = txt.substring(0, nl);
    const contents = txt.substring(nl + 1);
    if (!contents.startsWith("#")) return;
    const headers = contents.split("\n").filter(s => s.startsWith("#"));
    const title = headers[0].replace(/^#+ */, "");
    let hNum = 0;
    const HTML = md(contents) // md(contents, name === "long" && tweakPubs);
      .replaceAll(/<([hH][1-6])>.*?<\/\1>/g, s =>
        (hNum++ ? "</div>\n" : "") + "<div>\n" + s)
      .replace(/$/, "</div>");
    const headersHTML = md("## Intro\n" + headers.slice(1).join("\n"))
      .replace(/<[hH].>/g, "$&<a>").replace(/<\/[hH].>/g, "</a>$&");
    texts[name] = { name, contents, title, headers, HTML, headersHTML };
    return true;
  };
  { const n = document.createNodeIterator(
      document.head, NodeFilter.SHOW_COMMENT, ()=> NodeFilter.FILTER_ACCEPT);
    while (addText(n.nextNode())) { }
  }
  { $("#versions").innerHTML = "Version: "
      + Object.keys(texts).map(what => `<button>${what}</button>`).join("\n");
    $$("#versions button").forEach(b => b.addEventListener("click", switchTo));
  }
  { $("#formats").innerHTML = `Download <span class="what"></span> version: `
      + (Object.entries(formats)).map(([ext, name]) =>
          `<button data-file="download/Eli_Barzilay-VER.${ext}"`
          + ` data-name="${name}">${name}⭳</button>`)
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
