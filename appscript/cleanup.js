/*
 * Clean up files in google drive.
 *
 * I run cleanTmp daily to remove anything that's been in my tmp dir
 * for more than a week.
 *
 * I occasionally run cleanBadNames when I upload a bunch of stuff
 * that might have OS X or emacs garbage left behind.
 */
function cleanIter(threshold, it) {
  var cleaned = [];
  while (it.hasNext()) {
    var f = it.next();
    if (f.getLastUpdated() < threshold) {
      Logger.log('--- Deleting %s, last updated %s', f.getName(), f.getLastUpdated());
      f.setTrashed(true);
      cleaned.push(f.getName());
    } else {
      Logger.log('Keeping %s, last updated %s', f.getName(), f.getLastUpdated());
    }
  }
  return cleaned;
}

function cleanTmp() {
  var tmpdir = DriveApp.getFolderById('TMP_DIR_ID');
  var threshold = new Date(new Date() - 86400 * 7 * 1000);
  Logger.log('Cleaning anything older than %s', threshold);

  var cleaned = cleanIter(threshold, tmpdir.getFiles());
  cleaned = cleaned.concat(cleanIter(threshold, tmpdir.getFolders()));

  if (cleaned.length > 0) {
    var msg = "Cleaned the following items:\n\n * " + cleaned.join("\n * ") + "\n\nLog:\n" + Logger.getLog();
    MailApp.sendEmail(Session.getActiveUser().getEmail(), 'Cleaned your tmp', msg);
  }
}

// Clean up stuff that's more than 30d old from the trash.
// I was kind of surprised to find this doesn't happen by default.
function dumpsterFire() {
  var threshold = new Date(new Date() - 86400 * 30 * 1000);
  var start = new Date();
  var duration = function() {
    return new Date() - start;
  };
  var it = DriveApp.getTrashedFiles();
  var i = 0;
  while (it.hasNext()) {
    if (duration() > 300*1000) {
      break
    }
    var f = it.next();
    if (f.isTrashed() && f.getLastUpdated() < threshold) {
      Logger.log(' F %s – %s', f.getName(), f.getLastUpdated());
      Drive.Files.remove(f.getId());
      i++;
    }
  }

  if (duration() < 330*1000) {
    Logger.log("There's a bit of time left, let's clean directories, too");
    var it = DriveApp.getTrashedFolders();
    while (it.hasNext()) {
      if (duration() > 340*1000) {
        break
      }
      var d = it.next();
      var hasChildren = d.getFolders().hasNext() && d.getFiles().hasNext();
      if (d.isTrashed() && !hasChildren) {
        Logger.log(' D %s – %s', d.getName(), d.getLastUpdated());
        Drive.Files.remove(d.getId());
        i++;
      }
    }
  }

  if (i > 0) {
    var msg = "Removed the following " + i + " files/dirs from trash:\n\n" + Logger.getLog();
    MailApp.sendEmail(Session.getActiveUser().getEmail(), 'Cleaned your trash', msg);
  }
}

// Clean up files that have bad names.
//
// Give me your .DS_Store, your backup~ files, etc...
function cleanBadNames() {

  var badNameFunc = function(n) {
    return n === '.DS_Store' || /~$/.test(n);
  }

  var cleaned = 0;
  for (var iter = DriveApp.getFiles(); iter.hasNext(); ) {
    var f = iter.next();
    var n = f.getName();
    if (badNameFunc(n)) {
      Logger.log('%s has a bad name', n);
      f.setTrashed(true);
      cleaned++;
    }
  }
  if (cleaned > 0) {
    MailApp.sendEmail(Session.getActiveUser().getEmail(),
                      'Cleaned files with bad names', Logger.getLog());
  }
}
