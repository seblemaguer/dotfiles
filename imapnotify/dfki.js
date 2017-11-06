var child_process = require('child_process');

function getStdout(cmd) {
    var stdout = child_process.execSync(cmd);
    return stdout.toString().trim();
}

exports.host = "imap.dfki.de";
exports.port = 993;
exports.tls = true;
exports.tlsOptions = { "rejectUnauthorized": false };
exports.username = "sele01";
exports.password = getStdout("python3 ~/.get_passwd.py 'imap.dfki.de' 'sele01'");
exports.onNewMail = "syncmail Dfki";
exports.boxes = [ "INBOX" ];
