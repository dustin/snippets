var flightFolder = '0B-xxxxxxxxxxxxxxxx';

function renameFlightMedia() {
  function fixupLocationDir(sub) {
    var renameDirs = sub.getFolders();
    while (renameDirs.hasNext()) {
      var rdir = renameDirs.next();
      Logger.log("Found rename dir in %s: %s", sub.getName(), rdir.getName());

      var rfiles = rdir.getFiles();
      while (rfiles.hasNext()) {
        var rfile = rfiles.next();
        var newname = rdir.getName() + "-" + rfile.getName();
        // newname = newname.replace("20141012-2014012-", "");
        Logger.log("Renaming %s to %s", rfile.getName(), newname);
        rfile.setName(newname);
        Logger.log(" -- Adding %s to %s", newname, sub.getName());
        sub.addFile(rfile);
        Logger.log(" -- Removing %s from %s", newname, rdir.getName());
        rdir.removeFile(rfile);
      }
    }
  }

  var folder = DriveApp.getFolderById(flightFolder);
  var subs = folder.getFolders();
  while (subs.hasNext()) {
    fixupLocationDir(subs.next())
  }
}

function removeEmptyDirs() {
  function removeEmptyDirsIn(sub) {
    var renameDirs = sub.getFolders();
    while (renameDirs.hasNext()) {
      var rdir = renameDirs.next();
      Logger.log("Found rename dir in %s: %s", sub.getName(), rdir.getName());
      var rfiles = rdir.getFiles();
      var nFiles = 0;
      while (rfiles.hasNext()) {
        nFiles++;
        var rfile = rfiles.next();
      }
      if (nFiles > 0) {
        Logger.log("Keeping %s", rdir.getName());
      } else {
        Logger.log("Removing %s", rdir.getName());
        sub.removeFolder(rdir);
      }
    }
  }


  var folder = DriveApp.getFolderById(flightFolder);
  var subs = folder.getFolders();
  while (subs.hasNext()) {
    removeEmptyDirsIn(subs.next())
  }
}
