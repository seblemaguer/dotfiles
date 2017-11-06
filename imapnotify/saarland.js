var child_process = require('child_process');

function getStdout(cmd) {
    var stdout = child_process.execSync(cmd);
    return stdout.toString().trim();
}

exports.host = "mail.coli.uni-saarland.de";
exports.port = 993;
exports.tls = true;
exports.tlsOptions = { "rejectUnauthorized": false };
exports.username = "slemaguer";
exports.password = getStdout("python3 ~/.get_passwd.py 'mail.coli.uni-saarland.de' 'slemaguer'");
exports.onNewMail = "syncmail Saarland";
exports.onNewMailPost = "";
exports.boxes = [ "INBOX" ];
