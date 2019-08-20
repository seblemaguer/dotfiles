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
exports.onNewMail = "mbsync -q Saarland";
exports.onNewMailPost = "emacsclient  -e '(mu4e-update-index)'";
exports.boxes = [ "INBOX" ];
