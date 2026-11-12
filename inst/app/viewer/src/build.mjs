// Build from a directory containing these pinned npm dependencies:
// @deck.gl/{core,layers,extensions}@9.4.1 @hms-dbmi/viv@0.23.0
// @math.gl/core@4.1.0 esbuild@0.28.2. Dependencies are bundled for offline apps.
// node build.mjs /path/to/node_modules
import {createRequire} from 'node:module';
import {fileURLToPath} from 'node:url';
import path from 'node:path';
import fs from 'node:fs';
const root = path.dirname(fileURLToPath(import.meta.url));
const modules = process.argv[2] || path.join(root, 'node_modules');
const require = createRequire(path.join(modules, '_build.cjs'));
const {build} = require('esbuild');
const outfile = path.join(root,'../viewer.js');
const result = await build({entryPoints:[path.join(root,'viewer.js')], bundle:true, format:'iife',
  platform:'browser', target:['es2022'], minify:true, legalComments:'eof',
  nodePaths:[modules], outfile, metafile:true,
  define:{'process.env.NODE_ENV':'"production"'}});

// Preserve full third-party license texts as comments in the distributed code.
const packages = new Map();
for (const input of Object.keys(result.metafile.inputs)) {
  if (!input.includes('node_modules')) continue;
  let directory = path.dirname(path.resolve(input));
  while (directory !== path.dirname(directory)) {
    const metadata = path.join(directory, 'package.json');
    if (fs.existsSync(metadata)) {
      const info = JSON.parse(fs.readFileSync(metadata, 'utf8'));
      if (info.name) { packages.set(directory, info); break; }
    }
    directory = path.dirname(directory);
  }
}
const notices = [];
for (const [directory, info] of packages) {
  const file = ['LICENSE','LICENSE.md','LICENSE.txt','LICENCE','license','license.md','COPYING'].map(n => path.join(directory,n)).find(fs.existsSync);
  if (file) notices.push(`${info.name} ${info.version}\n${fs.readFileSync(file,'utf8')}`);
  else notices.push(`${info.name} ${info.version} — ${typeof info.license === 'string' ? info.license : JSON.stringify(info.license)}`);
}
fs.appendFileSync(outfile, '\n/* Bundled dependency licenses\n' + notices.join('\n\n').replaceAll('*/','* /') + '\n*/\n');
