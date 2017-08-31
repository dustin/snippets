/*
 * copyAppend is similar to sheet.append, but copies the row from
 * above, including relative references in formulas, etc...
 *
 * Supplied values are copied only when not undefined.
 *
 * e.g. if you append with   [1, undefined, "hello"],
 *
 * The last row in the spreadsheet will be duplicated, and then column
 * one will be replaced with the value "1" and column three will be
 * replaced with the value "hello".
 */
function copyAppend(sheet, values) {
  var lastr = sheet.getLastRow();
  var lastc = Math.min(values.length, sheet.getLastColumn());
  var r = sheet.getRange(lastr, 1, 1, lastc);
  var destr = sheet.getRange(lastr+1, 1, 1, lastc);
  r.copyTo(destr);
  for (var i = 0; i < values.length; i++) {
    if (values[i] !== undefined) {
      destr.offset(0, i, 1, 1).setValue(values[i]);
    }
  }
  return r.getValues()[0];
}
