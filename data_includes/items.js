var shuffleSequence = seq("consent", "intro", "Practice", "presep", "dummysep",
                        sepWith("sep", rshuffle(startsWith("agr-amb"), startsWith("agr-dist"), startsWith("filler"))), "exit");
//var practiceItemTypes = ["Practice"];
//var progressBarText = ["Progress"];
var ds = DashedSentence;

var showProgressBar = false;

counterOverride = 0;

var defaults = [
    Separator,{
        ignoreFailure: false, 
        errorMessage: "Wrong answer. Please read each sentence carefully!"
    },
    ds,{
        mode: "self-paced reading",
        display: "in place"
    },
];

 // insert breaks
function modifyRunningOrder(ro) {

 for (var i = 0; i < ro.length; ++i) {
 if (i % 24 == 22 && i > 25 && i < 200) {
 ro[i].push(new DynamicElement(
 "Message",
 { html: "<p>Please take a short break. The experiment will continue in 10 seconds.</p>", transfer: 10000 },
 true
 ));
 }
 }
 return ro;
 }


var items = [

["setcounter", "__SetCounter__", { }],
["consent", "Form", {consentRequired: true, html: {include: "consent-2015.html" }} ],    
["intro", "Form", {consentRequired: true, html: {include: "intro.html" }} ],
["intro", "Form", {consentRequired: true, html: {include: "intro1.html" }} ],
["intro", "Form", {consentRequired: true, html: {include: "intro2.html" }} ],
["intro", "Form", {consentRequired: true, html: {include: "intro3.html" }} ],
["exit", "Form", {consentRequired: false, html: {include: "exit.html" }} ],
["debrief", "Form", {consentRequired: false, html: {include: "debrief.html" }} ],

["sep", Separator, { }],

["question", "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],

["Practice", ds, {s: ["This is", "just a practice sentence", "to get you used", "to the method", "of presentation."]}],
//["Practice", "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
["Practice", ds, {s: ["This is", "another practice sentence", "which is longer", "and a little more complicated", "than the one", "you just read."]}],
//["Practice", "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
                           
["Practice", Message, {consentRequired: false, transfer: "keypress",
                     html: ["div",
                           ["p", "That's all there is to it! Let's try some practice sentences more like the ones you'll be seeing in the experiment, which include a judgment task at the end. Press the space bar to get started!"]
                           ]}],
                           
["Practice", ds, {s: ["Elias","told Martha","that the attractive lifeguard","on the pier","who is dating","Tracy's sister","saved","a little boy","from being","swept out to sea","by a rip-tide yesterday."]}],
["Practice", "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],

["Practice", Message, {consentRequired: false, transfer: "keypress",
                    html: ["div",
                          ["p", "Some sentences, like the one you just read, are fairly long and complex. Despite this, most people respond to a sentence like this with a 6 or a 7"],
                          ["p", "Try your hand at this next sentence. Don't overthink your response: go with your gut feeling or intuition!"]
                          ]}],
    

["Practice", ds, {s: ["The pop star", "sang", "myself hoarse","at the concert", "last night."]}],
["Practice", "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],

["Practice", Message, {consentRequired: false, transfer: "keypress",
                    html: ["div",
                          ["p", "That sentence should have been pretty unacceptable. Most people rate it a 1 or a 2. Remember, you may not know exactly why the sentence is bad. What's important is to go with your instinct!"],
                          ["p", "Let's try another one!"]
                          ]}],

["Practice", ds, {s: ["The kids", "were eating", "yesterday", "the string cheese"]}],
["Practice", "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],

["Practice", Message, {consentRequired: false, transfer: "keypress",
                    html: ["div",
                          ["p", "That sentence should have been somewhere in the middle. Most people rate it a 3, 4, or 5. You know what the sentence means, but you might not say it that way yourself."],
                          ["p", "Let's try one more!"]
                          ]}],

["Practice", ds, {s: ["The plumber", "working in the bathroom","cursed himself", "for forgetting", "his wrench."]}],
["Practice", "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
                           
["Practice", Message, {consentRequired: false, transfer: "keypress",
                     html: ["div",
                           ["p", "That's all the practice! When you're ready to begin the experiment, press any button to move ahead. REMEMBER: it will last approximately 20 minutes, and will require your full attention throughout that period. Thank you for your help!"]
                           ]}],
                           
["presep", Separator, { transfer: 3000, normalMessage: "Get your hands in position, and get ready to begin!" }],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],
["dummysep", Separator, {transfer: 10, normalMessage: ""}],

[["A",1], ds, {s: ["The busboy", "that worked with the server", "apparently was tired", "of the patrons", "who didn't tip well."]}, "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
[["B",1], ds, {s: ["The busboy", "that worked with the servers", "apparently was tired", "of the patrons", "who didn't tip well."]}, "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
[["C",1], ds, {s: ["The busboy", "that worked with the server", "apparently were tired", "of the patrons", "who didn't tip well."]}, "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
[["D",1], ds, {s: ["The busboy", "that worked with the servers", "apparently were tired", "of the patrons", "who didn't tip well."]}, "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
[["E",1], ds, {s: ["The busboy", "with the server", "apparently was tired", "of the patrons", "who didn't tip well."]}, "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
[["F",1], ds, {s: ["The busboy", "with the servers", "apparently was tired", "of the patrons", "who didn't tip well."]}, "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
[["G",1], ds, {s: ["The busboy", "with the server", "apparently were tired", "of the patrons", "who didn't tip well."]}, "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],
[["H",1], ds, {s: ["The busboy", "with the servers", "apparently were tired", "of the patrons", "who didn't tip well."]}, "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],


[["filler",49], ds, {s: ["John envied", "the friend", "of the colonel,", "who was on the balcony."]}, "Question", {q: "How acceptable did you find the sentence?", as: ["1","2","3","4","5","6","7"],presentAsScale: true, autoFirstChar: true, leftComment: "(bad)", rightComment: "(good)"}],

];