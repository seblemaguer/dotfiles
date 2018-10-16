var child_process = require('child_process');

function getStdout(cmd) {
    var stdout = child_process.execSync(cmd);
    return stdout.toString().trim();
}

exports.host = "imap.gmail.com";
exports.port = 993;
exports.tls = true;
exports.tlsOptions = { "rejectUnauthorized": false };
exports.username = "sebastien.lemaguer@adaptcentre.ie";
exports.password = getStdout("python3 ~/.get_passwd.py 'imap.gmail.com' 'sebastien.lemaguer@adaptcentre.ie'");
exports.onNewMail = "syncmail AdaptGmail";
exports.boxes = [ "INBOX" ];
