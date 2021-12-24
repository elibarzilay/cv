const dateTypes = [{
  section:   "Recent Microsoft Experience",
  type:      "range",
  direction: "up",
  anchor:    "datestr",
}, {
  section:   "Education",
  type:      "range",
  direction: "down",
  anchor:    "datestr",
}, {
  section:   "Teaching Experience",
  type:      "range",
  direction: "down",
  anchor:    "datestr",
}, {
  section:   "Research Experience",
  type:      "range",
  direction: "down",
  anchor:    "datestr",
}, {
  section:   "Publications",
  type:      "moment",
  direction: "down",
  anchor:    "name",
}, {
  section:   "Pre-Microsoft Experience",
  type:      "range",
  direction: "up",
  anchor:    "datestr",
}, {
  section:   "Pre-Microsoft Major Projects",
  type:      "range",
  direction: "up",
  anchor:    "datestr",
}];

const NOW = (new Date).toISOString();
const dateInfo = {
  "Recent Microsoft Experience": [{ ///////////////////////////////////////////
    start:   "2019-10-01", end: NOW,
    datestr: "October 2019 — Present",
    name:    "Microsoft: TypeScript",
    short:   "TypeScript",
  }, {
    start:   "2018-03-01", end: "2019-09-30",
    datestr: "March 2018 — October 2019",
    name:    "Microsoft: Python group",
    short:   "Azure Python",
  }, {
    start:   "2016-07-01", end: "2018-02-28",
    datestr: "Mid 2016 — March 2018",
    name:    "Microsoft: MMLSpark",
    short:   "MMLSpark",
  }, {
    start:   "2015-12-01", end: "2016-06-30",
    datestr: "December 2015 — Mid 2016",
    name:    "Microsoft: Azure Machine Learning Studio",
    short:   "AzureML Studio",
  }],
  "Education": [{ /////////////////////////////////////////////////////////////
    start:   "1997-01-01", end: "2003-07-31",
    datestr: "1997–2003",
    name:    "Ph.D. in Computer Science, Cornell",
    short:   "CS PhD @ Cornell",
  }, {
    start:   "1994-09-01", end: "1996-12-31",
    datestr: "1994–1996",
    name:    "M.Sc. in Computer Science, Ben-Gurion",
    short:   "CS MSc @ Cornell",
  }, {
    start:   "1991-09-01", end: "1994-06-30",
    datestr: "1991–1993",
    name:    "B.Sc. in Math & Computer Science, Ben-Gurion",
    short:   "CS+Math BSc @ BGU",
  }],
  "Teaching Experience": [{ ///////////////////////////////////////////////////
    start:   "2004-01-01", end: NOW,
    datestr: "2004–Present",
    name:    "Programming Languages, Northeastern",
    short:   "Programming Languages",
  }, {
    start:   "1997-01-01", end: "2000-05-31",
    datestr: "1997–2000",
    name:    "Teaching Assistant, Cornell",
    short:   "TA @ Cornell",
  }, {
    start:   "1994-09-01", end: "1996-12-31",
    datestr: "1994–1996",
    name:    "Teaching Assistant, BGU",
    short:   "TA @ BGU",
  }],
  "Research Experience": [{ ///////////////////////////////////////////////////
    start:   "2003-09-01", end: "2014-07-01",
    datestr: "2003–2014",
    name:    "Researcher, Programming Research Laboratory, Northeastern",
    short:   "PRL, Northeastern",
  }, {
    start:   "1997-01-01", end: "2003-07-31",
    datestr: "1997–2003",
    name:    "Graduate Research Assistant in the Nuprl group, Cornell",
    short:   "Nuprl, Cornell",
  }, {
    start:   "1994-09-01", end: "1996-12-31",
    datestr: "1994–1996",
    name:    "Research Assistant, Ben-Gurion",
    short:   "Computer Music @ BGU",
  }],
  "Publications": [{ //////////////////////////////////////////////////////////
    date:    "2018-10-20",
    datestr: "2018",
    name:    "MMLSpark: Unifying Machine Learning Ecosystems at Massive Scales",
    short:   "MMLSpark",
  }, {
    date:    "2018-05-01",
    datestr: "2018",
    name:    "A Programmable Programming Language",
    short:   "Programmable PL",
  }, {
    date:    "2015-05-01",
    datestr: "2015",
    name:    "The Racket Manifesto",
    short:   "Racket Manifesto",
  }, {
    date:    "2011-10-23",
    datestr: "2011",
    name:    "Keeping it Clean with Syntax Parameters",
    short:   "Syntax Parameters",
  }, {
    date:    "2011-10-01",
    datestr: "2011",
    name:    "From Stack Traces to Lazy Rewriting Sequences",
    short:   "Lazy Rewriting",
  }, {
    date:    "2009-08-15",
    datestr: "2009",
    name:    "The Scribble Reader",
    short:   "Scribble Reader",
  }, {
    date:    "2009-08-01",
    datestr: "2009",
    name:    "Keyword and Optional Arguments in PLT Scheme",
    short:   "Keyword Args",
  }, {
    date:    "2009-09-01",
    datestr: "2009",
    name:    "Scribble: Closing the Book on Ad Hoc Documentation Tools",
    short:   "Scribble",
  }, {
    date:    "2006-09-17",
    datestr: "2006",
    name:    "A Self-Hosting Evaluator using HOAS",
    short:   "HOAS",
  }, {
    date:    "2006-01-01",
    datestr: "2006",
    name:    "Implementing Direct Reflection in Nuprl",
    short:   "Direct Reflection",
  }, {
    date:    "2005-03-01", // moved from 2005-09-01
    datestr: "2005",
    name:    "Laziness Without All the Hard Work: Combining Lazy and Strict Languages for Teaching",
    short:   "Lazy Racket",
  }, {
    date:    "2004-09-01",
    datestr: "2004",
    name:    "Foreign Interface for PLT Scheme",
    short:   "FFI",
  }, {
    date:    "2003-08-01",
    datestr: "2003",
    name:    "MetaPRL — A Modular Logical Environment",
    short:   "MetaPRL",
  }, {
    date:    "2003-06-01",
    datestr: "2003",
    name:    "Practical Reflection in Nuprl",
    short:   "Practical Reflection",
  }, {
    date:    "2002-07-01", // moved from 2002-08-01
    datestr: "2002",
    name:    "Reflecting Higher-Order Abstract Syntax in Nuprl",
    short:   "Nuprl HOAS",
  }, {
    date:    "2002-05-01", // moved from 2002-11-01
    datestr: "2002",
    name:    "Abstraction as a Means for End-User Computing in Creative Applications",
    short:   "Creative Abstr.",
  }, {
    date:    "2001-01-01",
    datestr: "2001",
    name:    "Quotation and Reflection in Nuprl and Scheme",
    short:   "Quotation+Reflection",
  }, {
    date:    "1996-12-31",
    datestr: "1997",
    name:    "Booms Object Oriented Music System",
    short:   "Booms",
  }],
  "Pre-Microsoft Experience": [{ //////////////////////////////////////////////
    start:   "2014-05-01", end: "2015-10-01",
    datestr: "2014–2015",
    name:    "Gefen Dekel — Senior Software Developer",
    short:   "Gefen Dekel",
  }, {
    start:   "2003-09-01", end: "2014-07-01",
    datestr: "2003–2014",
    name:    "System Administrator for the PLT group",
    short:   "SysAdmin @ PLT",
  }, {
    start:   "1995-01-01", end: "1996-12-31",
    datestr: "1995–1997",
    name:    "Gefen-Dekel — Software Developer",
    short:   "Gefen-Dekel",
  }, {
    start:   "1994-05-01", end: "1994-12-31",
    datestr: "1994",
    name:    "Shibutzit — Software Developer",
    name:    "Shibutzit",
  }, {
    start:   "1993-08-01", end: "1994-06-30",
    datestr: "1993",
    name:    "System Administrator at Ben-Gurion",
    short:   "Sysadmin @ BGU CS",
  }],
  "Pre-Microsoft Major Projects": [{ //////////////////////////////////////////
    start:   "2008-01-01", end: "2010-12-31",
    datestr: "2008–2010",
    name:    "The Scribble Reader",
    short:   "Scribble",
  }, {
    start:   "2005-01-01", end: "2006-12-31",
    datestr: "2005–2006",
    name:    "Lazy Scheme",
  }, {
    start:   "2004-01-01", end: "2007-12-31",
    datestr: "2004–2007",
    name:    "Handin Server and Client",
    short:   "Handin Server+Client",
  }, {
    start:   "2003-01-01", end: "2004-12-31",
    datestr: "2003–2004",
    name:    "Foreign Function Interface for Racket",
    short:   "Racket FFI",
  }, {
    start:   "2003-09-01", end: "2014-07-01",
    datestr: "2003–2014",
    name:    "PLT Racket Infrastructure",
    short:   "Racket Infrastructure",
  }, {
    start:   "1999-01-01", end: "2003-07-31",
    datestr: "1999–2003",
    name:    "Reflection for Nuprl",
  }, {
    start:   "2001-01-01", end: "2001-12-01",
    datestr: "2001",
    name:    "Web-server",
  }, {
    start:   "1999-01-01", end: "1999-12-31",
    datestr: "1999",
    name:    "Picture collection management",
    short:   "Picture collection",
    anchor:  "name",
  }, {
    start:   "1999-01-01", end: "1999-12-31",
    datestr: "1999",
    name:    "HTML generation",
    anchor:  "name",
  }, {
    start:   "1998-01-01", end: "1998-12-31",
    datestr: "1998",
    name:    "Swindle",
  }, {
    start:   "1996-09-01", end: "1997-12-31",
    datestr: "1997",
    name:    "Calculator",
  }, {
    start:   "1995-01-01", end: "1996-12-31",
    datestr: "1995–1996",
    name:    "Booms",
  }, {
    start:   "1995-01-01", end: "1996-12-31",
    datestr: "1994–1996",
    name:    "Multeam",
  }],
};
