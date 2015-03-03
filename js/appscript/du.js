var folderId = 'XXXXXXXX';

function getDestFolder() {
  return DriveApp.getFolderById(folderId);
}

function makeFilelist() {
  var tmpdir = getDestFolder();
  var content = [];
  var files = DriveApp.getFiles();
  while (files.hasNext()) {
    var file = files.next();
    content.push(file.getName() + "\t" + file.getSize())
  }
  tmpdir.createFile("allthethings.txt", content.join("\n"));
}
