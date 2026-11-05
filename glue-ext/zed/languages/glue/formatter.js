#!/usr/bin/env node

let source = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (chunk) => {
  source += chunk;
});
process.stdin.on("end", () => {
  const lines = source.split(/\r?\n/);
  let indent = 0;
  let inString = false;
  const output = lines.map((line) => {
    const text = line.trim();
    if (text === "") return "";

    const lineIndent = text.startsWith(")") ? Math.max(0, indent - 1) : indent;
    let opens = 0;
    let closes = 0;
    let escaped = false;
    for (const character of text) {
      if (!inString && character === ";") break;
      if (inString) {
        if (escaped) escaped = false;
        else if (character === "\\") escaped = true;
        else if (character === '"') inString = false;
      } else if (character === '"') {
        inString = true;
      } else if (character === "(") {
        opens += 1;
      } else if (character === ")") {
        closes += 1;
      }
    }
    indent = Math.max(0, indent + opens - closes);
    return `${" ".repeat(lineIndent * 4)}${text}`;
  });

  let formatted = output.join("\n");
  if (source.endsWith("\n") && !formatted.endsWith("\n")) formatted += "\n";
  process.stdout.write(formatted);
});
