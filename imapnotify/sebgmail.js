var child_process = require('child_process');

function getStdout(cmd) {
    var stdout = child_process.execSync(cmd);
    return stdout.toString().trim();
}

exports.host = "imap.gmail.com";
exports.port = 993;
exports.tls = true;
exports.tlsOptions = { "rejectUnauthorized": false };
exports.username = "seb.lemaguer@gmail.com";
exports.password = getStdout("python3 ~/.get_passwd.py 'imap.gmail.com' 'seb.lemaguer@gmail.com'");
exports.onNewMail = "mbsync -q SebGmail";
exports.onNewMailPost = "~/environment/local/emacs/bin/emacsclient  -e '(mu4e-update-index)'";
exports.boxes = [ "INBOX" ];
