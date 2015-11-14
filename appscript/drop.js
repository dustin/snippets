var dropFolder = '0B-XXXXXXXXXXXXXXXXXXXXXXXXX';

function moveDroppedFiles() {

  function mv(file, src, dest) {
    Logger.log(" -- Adding %s to %s", file.getName(), dest.getName());
    dest.addFile(file);
    Logger.log(" -- Removing %s from %s", file.getName(), src.getName());
    src.removeFile(file);
  }

  function findOrCreate(parent, name) {
    var children = parent.getFoldersByName(name);
    if (children.hasNext()) {
      Logger.log(" # Found subfolder %s in %s", name, parent.getName());
      return children.next();
    }
    Logger.log(" ! Creating %s under %s", name, parent.getName());
    return parent.createFolder(name);
  }

  function subProcess(src, dest) {
    var done = 0;

    for (var files = src.getFiles(); files.hasNext(); ) {
      mv(files.next(), src, dest);
      done++;
    }

    for (var subs = src.getFolders(); subs.hasNext(); ) {
      var sub = subs.next();
      Logger.log(" * Found subfolder: %s", sub.getName());
      done += subProcess(sub, findOrCreate(dest, sub.getName()));
    }

    return done;
  }

  var done = subProcess(DriveApp.getFolderById(dropFolder),
                        DriveApp.getRootFolder());
  Logger.log(' # Total moved: %s', done);

  if (done > 0) {
    MailApp.sendEmail(Session.getActiveUser().getEmail(),
                      'Moved Drop Files', Logger.getLog());
  }
}
