var ghpages = require('gh-pages');
var fs = require('fs');
var path = require('path');

function copyFolder(source, target) {
  try {
    fs.mkdirSync(target)
  } catch (e) {
    if (e.code != "EEXIST") throw e;
  }

  files = fs.readdirSync(source);
  files.forEach(function (file) {
    var srcFile = path.join(source, file);
    var tgtFile = path.join(target, file);
    console.log("Copying " + srcFile + " to " + tgtFile)
    fs.createReadStream(srcFile)
      .pipe(fs.createWriteStream(tgtFile));
  } );
}
copyFolder("stdlib", "assets/stdlib");
ghpages.publish('./assets', err => {if (err) console.error(err); else console.log('Published successfully');});
