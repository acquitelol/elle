/**
!! THIS FILE IS TEMPORARY !!

* QBE compiles floating point constant data sections with the incorrect name
* This file simply replaces all instances of "Lfp(\d+):" with "_Lfp(\d+):"
which fixes this issue.
* The developer of QBE is aware of this.
*/

const fs = require("fs"); // Necessary because this isn't a module
const inputFile = process.argv[2];
const file = fs.readFileSync(inputFile);

const out = file.toString();
console.log(out.replace(/Lfp(\d+):/g, "_Lfp$1:"));
