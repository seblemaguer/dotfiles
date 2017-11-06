
var child_process = require('child_process');

function getStdout(cmd) {
    var stdout = child_process.execSync(cmd);
    return stdout.toString().trim();
}

exports.host = "mail.mmci.uni-saarland.de";
exports.port = 993;
exports.tls = true;
exports.tlsOptions = { "rejectUnauthorized": false };
exports.username = "lemaguer";
exports.password = getStdout("python3 ~/.get_passwd.py 'mail.mmci.uni-saarland.de' 'lemaguer'");
exports.onNewMail = "syncmail MMCI";
exports.boxes = [ "INBOX" ];
