#!/usr/bin/python3
# -*- coding: utf-8 -*-

import os
import sys
# import gnupg, base64
import io
import logging
from sys import platform as _platform
from subprocess import check_output

GPG_PROG = "/usr/bin/gpg"

def decrypt(filename):
    # # Load encrypted data
    # f = open(os.path.expanduser(authinfo), "rb")
    # encrypted_data = f.read()
    # f.close()

    # binary = GPG_PROG
    # homedir = os.getenv("HOME") + "/.gnupg/"
    # with open(os.path.expanduser(authinfo), "rb") as f:
    #     decrypted_data = gnupg.GPG(binary=binary, homedir=homedir).decrypt(f.read())

    return check_output("%s -dq %s" % (GPG_PROG, filename), shell=True).decode("utf-8")

def contains(d, m):
    '''Return True if d contains all items of m.'''
    for k in m:
        if not k in d or d[k] != m[k]:
            return False
    return True

def getnetrc(authinfo=None, **match):
    '''A dumb filter for ~/.netrc.gpg. It understands oneline entries,
    comments and default entry. No macros or multiline entries.

    Return first matching dict or None.'''
    default = None
    if not authinfo:
        authinfo = os.getenv('NETRC') or '~/.authinfo.gpg'

    # Decrypt
    decrypted_data = decrypt(os.path.expanduser(authinfo))
    # logging.debug(decrypted_data)
    # Extracting informations
    for li in str(decrypted_data).split("\n"):
        li = li.split() # current line in list
        if not li:
            continue
        if li[0] == 'default':
            default = dict(zip(li[1:-1:2], li[2::2]))
            continue
        elif li[0] == 'macdef':
            continue

        li = dict(zip(li[:-1:2], li[1::2])) # current line in dict
        if match and contains(li, match):
            return li

    if default and contains(default, match):
        return default

# debug
if __name__ == "__main__":
    print(getnetrc(machine=sys.argv[1], login=sys.argv[2])['password'])
