"use strict";

const texts = {};
const formats = { pdf: "PDF", docx: "Word", txt: "Text" };
let curVersion = "";

const show = what => {
  if (typeof what === "number") what = Object.keys(texts)[what];
  curVersion = what;
  const text = texts[what];
  document.title = text.title;
  document.getElementById("text").innerHTML = text.HTML;
  for (const s of document.querySelectorAll("span.what")) s.textContent = what;
  for (const b of document.getElementById("versions")
                          .getElementsByTagName("button"))
    b.classList.toggle("selected", b.textContent === what);
  document.getElementById("toc-links").innerHTML = text.headersHTML;
  for (const h of document.getElementById("toc-links").children)
    h.addEventListener("click", tocJump);
  setTimelineTargets();
  document.documentElement.scrollTo(0, 0);
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
  const text = document.getElementById("text");
  const hdr = e.target.textContent;
  jumpTo(hdr === "Intro" ? text.getElementsByTagName("H1")[0]
         : [...text.getElementsByTagName(e.target.parentElement.tagName)]
         .find(h => h.textContent === hdr));
};

const setTimelineTargets = ()=> {
  const headers = Object.fromEntries(
    [...document.getElementById("text").children]
      .filter(n => n.tagName.startsWith("H"))
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
    const HTML = md(contents); // md(contents, name === "long" && tweakPubs);
    const headersHTML = md("## Intro\n" + headers.slice(1).join("\n"))
      .replace(/<[hH].>/g, "$&<a>").replace(/<\/[hH].>/g, "</a>$&");
    texts[name] = { name, contents, title, headers, HTML, headersHTML };
    return true;
  };
  { const n = document.createNodeIterator(
      document.head, NodeFilter.SHOW_COMMENT, ()=> NodeFilter.FILTER_ACCEPT);
    while (addText(n.nextNode())) { }
  }
  { const vs = document.getElementById("versions");
    vs.innerHTML = "Version: "
      + Object.keys(texts).map(what => `<button>${what}</button>`).join("\n");
    for (const b of vs.getElementsByTagName("button"))
      b.addEventListener("click", switchTo);
  }
  { const fs = document.getElementById("formats");
    fs.innerHTML = `Download <span class="what"></span> version: `
      + (Object.entries(formats)).map(([ext, name]) =>
          `<button data-file="Eli_Barzilay-VER.${ext}" data-name="${name}">`
          + `${name}⭳</button>`)
        .join("\n");
    const a = document.createElement("a");
    const popup = document.getElementById("popup");
    popup.innerHTML = [
      `Click to download the <span class="what"></span> version in`,
      ` <span class="format"></span> format`,
      `<br>Shift+click to view; switch version on the left`,
    ].join("");
    popup.addEventListener("transitionend", ()=> {
      if (!popup.classList.contains("active")) popup.style.display = "none"; });
    for (const b of fs.getElementsByTagName("button")) {
      b.addEventListener("click", ({target: {dataset: {file}}, shiftKey}) => {
        a.href = a.download = file.replace(/\bVER\b/, curVersion);
        document.getElementById("text").focus();
        if (shiftKey) location = a.href; else a.click();
      });
      b.addEventListener("mouseenter", ({target: {dataset: {name}}}) => {
        popup.querySelector("span.format").textContent = name.toLowerCase();
        popup.style.display = "block";
        popup.style.top = b.getBoundingClientRect().bottom + "px";
        popup.style.left = (b.offsetLeft - popup.offsetWidth + b.offsetWidth) + "px";
        popup.classList.add("active");
      });
      b.addEventListener("mouseleave", ()=> popup.classList.remove("active"));
    }
  }
  show(0);
};

window.addEventListener("load", ()=> { init(); renderTimeline(); });
