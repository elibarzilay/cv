const renderTimeline = ()=>{

  anychart.format.outputDateTimeFormat("yyyy-MM-dd");
  const chart = anychart.timeline();
  chart.title().enabled(false);
  chart.background({fill: "#222 0.2"});
  chart.axis().height(24).fill("#cfc 0.2").stroke("#224 0.5", 5)
    .ticks().stroke("#224 0.5", 2);
  chart.axis().labels().fontSize(10);

  Object.entries(dateInfo).forEach(([sec, { entries, type, direction }]) => {
    const isMoment = type === "moment";
    for (const x of entries) {
      x.section = sec;
      if (isMoment) x.x = x.date, x.y = x.name;
      if (!x.short) x.short = x.name;
    }
    const r = chart[type](entries);
    r.direction(direction);
    r.labels().useHtml(true).format("{%short}")
     .fontFamily("Tahoma").fontWeight(100).fontSize(isMoment ? 7 : 10)
     .fontColor("#fff")
     .padding(2);
    r.height(14);
    r.tooltip().useHtml(true).fontColor("#fff")
     .titleFormat("{%name}").format(`{%datestr} <i>({%section})</i>`);
    const color = direction === "up" ? "#48c"
                : isMoment ? "#ca3" : "#c84";
    const colors = [2,4,6,8].map(o => `${color} 0.${o}`);
    const setColors = (x, i) =>
      x.fill(colors[i+1]).stroke(colors[i], 2, null, "round");
    setColors(r.normal(),   0);
    setColors(r.hovered(),  1);
    setColors(r.selected(), 2);
    if (isMoment) {
      r.labels().width("fit-content");
      r.markers().type("circle");
      setColors(r.normal()  .markers(), 0).size(6);
      setColors(r.hovered() .markers(), 1).size(7);
      setColors(r.selected().markers(), 2).size(8);
      setColors(r.labels().background(), 0);
    }
  });

  chart.interactivity().selectionMode("singleSelect");
  chart.listen("pointsSelect", e => {
    if (!e.currentPoint.selected) return;
    const p = e.point;
    p.get("go")();
    p.selected(false);
  });

  chart.scale().zoomLevels([[
    {unit: "month", count: 1},
    {unit: "quarter", count: 1},
    {unit: "year", count: 1},
    {unit: "year", count: 5},
  ]]);
  { const z = anychart.ui.zoom(); z.target(chart); z.render(); }

  { const sc = chart.scroller();
    sc.enabled(true);
    sc.height(10).fill("#112 0.33").selectedFill("#448 0.5");
    sc.thumbs().autoHide(true);
    sc.thumbs().normal() .fill("#448 0.33");
    sc.thumbs().hovered().fill("#88F 1.0");
  }

  chart.container("timeline").draw();

};
