var flightFolder = '0B-xxxxxxxxxxxxxxxx';

function renameFlightMedia() {
  function fixupLocationDir(sub) {
    for (var renameDirs = sub.getFolders(); renameDirs.hasNext(); ) {
      var rdir = renameDirs.next();
      Logger.log("Found rename dir in %s: %s", sub.getName(), rdir.getName());

      for (var rfiles = rdir.getFiles(); rfiles.hasNext(); ) {
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

  for (var subs = DriveApp.getFolderById(flightFolder).getFolders(); subs.hasNext(); ) {
    fixupLocationDir(subs.next())
  }
}

function removeEmptyDirs() {
  function removeEmptyDirsIn(sub) {
    for (var renameDirs = sub.getFolders(); renameDirs.hasNext(); ) {
      var rdir = renameDirs.next();
      Logger.log("Found rename dir in %s: %s", sub.getName(), rdir.getName());
      var nFiles = 0;
      for (var rfiles = rdir.getFiles(); rfiles.hasNext(); rfiles.next()) {
        nFiles++;
      }
      if (nFiles > 0) {
        Logger.log("Keeping %s", rdir.getName());
      } else {
        Logger.log("Removing %s", rdir.getName());
        sub.removeFolder(rdir);
      }
    }
  }


  for (var subs = DriveApp.getFolderById(flightFolder).getFolders(); subs.hasNext(); ) {
    removeEmptyDirsIn(subs.next())
  }
}
