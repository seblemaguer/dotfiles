var child_process = require('child_process');

function getStdout(cmd) {
    var stdout = child_process.execSync(cmd);
    return stdout.toString().trim();
}

exports.host = "outlook.office365.com";
exports.port = 993;
exports.tls = true;
exports.tlsOptions = { "rejectUnauthorized": false };
exports.username = "lemagues@tcd.ie";
exports.password = getStdout("python3 ~/.get_passwd.py 'outlook.office365.com' 'lemagues@tcd.ie'");
exports.onNewMail = "syncmail TCD";
exports.onNewMailPost = "";
exports.boxes = [ "INBOX" ];
