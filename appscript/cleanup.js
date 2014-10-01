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
  var cleaned = 0;
  while (it.hasNext()) {
    var f = it.next();
    if (f.getLastUpdated() < threshold) {
      Logger.log('Deleting %s', f.getName());
      f.setTrashed(true);
      cleaned++;
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
  cleaned += cleanIter(threshold, tmpdir.getFolders());

  if (cleaned > 0) {
    MailApp.sendEmail(Session.getActiveUser().getEmail(), 'Cleaned your tmp', Logger.getLog());
  }
}

function yougivelove(n) {
  return n === '.DS_Store' || /~$/.test(n);
}

// Clean up files that have bad names.
//
// Give me your .DS_Store, your backup~ files, etc...
function cleanBadNames() {
  var cleaned = 0;
  var iter = DriveApp.getFiles();
  while (iter.hasNext()) {
    var f = iter.next();
    var n = f.getName();
    if (yougivelove(n)) {
      Logger.log('%s has a bad name', n);
      f.setTrashed(true);
      cleaned++;
    }
  }
  if (cleaned > 0) {
    MailApp.sendEmail(Session.getActiveUser().getEmail(), 'Cleaned files with bad names', Logger.getLog());
  }
}
