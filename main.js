const fs = require("fs");
const inputFile = process.argv[2];
const file = fs.readFileSync(inputFile);

const out = file.toString();
console.log(out.replace(/Lfp(\d+):/g, "_Lfp$1:"));
