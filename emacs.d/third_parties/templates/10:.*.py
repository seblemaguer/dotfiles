#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
AUTHOR

    `user-full-name` <`user-mail-address`>

DESCRIPTION

LICENSE
    This script is in the public domain, free from copyrights or restrictions.
    Created: `(format-time-string "%e %B %Y")`
"""

# System/default
import sys
import os

# Arguments
import argparse

# Messaging/logging
import traceback
import time
import logging
from logging.config import dictConfig

###############################################################################
# global constants
###############################################################################
LEVEL = [logging.WARNING, logging.INFO, logging.DEBUG]

###############################################################################
# Functions
###############################################################################


###############################################################################
# Main function
###############################################################################
def main():
    """Main entry function
    """
    global args


###############################################################################
#  Envelopping
###############################################################################
if __name__ == "__main__":
    try:
        parser = argparse.ArgumentParser(description="")

        # Add options
        parser.add_argument("-l", "--log_file", default=None, help="Logger file")
        parser.add_argument(
            "-v",
            "--verbosity",
            action="count",
            default=0,
            help="increase output verbosity",
        )

        # Add arguments
        # Example : parser.add_argument("echo", help="description")
        # TODO

        # Parsing arguments
        args = parser.parse_args()

        # create logger and formatter
        logger = logging.getLogger()
        formatter = logging.Formatter(
            "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
        )

        # Verbose level => logging level
        log_level = args.verbosity
        if args.verbosity >= len(LEVEL):
            log_level = len(LEVEL) - 1
            # logging.warning("verbosity level is too high, I'm gonna assume you're taking the highest (%d)" % log_level)

        logging_config = dict(
            version=1,
            disable_existing_logger=True,
            formatters={
                "f": {
                    "format": "[%(asctime)s] [%(levelname)s] — [%(name)s — %(funcName)s:%(lineno)d] %(message)s",
                    "datefmt": "%d/%b/%Y: %H:%M:%S ",
                }
            },
            handlers={
                "h": {
                    "class": "logging.StreamHandler",
                    "formatter": "f",
                    "level": LEVEL[log_level],
                }
            },
            root={"handlers": ["h"], "level": LEVEL[log_level]},
        )

        if args.log_file is not None:
            logging_config["handlers"]["f"] = {
                "class": "logging.FileHandler",
                "formatter": "f",
                "level": LEVEL[log_level],
                "filename": args.log_file
            }
            logging_config["root"]["handlers"] = ["h", "f"]

        dictConfig(logging_config)
        logger = logging.getLogger(__name__)

        # # create file handler
        # if args.log_file is not None:
        #     fh = logging.FileHandler(args.log_file)
        #     logger.addHandler(fh)

        # Debug time
        start_time = time.time()
        logger.info("start time = " + time.asctime())

        # Running main function <=> run application
        main()

        # Debug time
        logger.info("end time = " + time.asctime())
        logger.info(
            "TOTAL TIME IN MINUTES: %02.2f" % ((time.time() - start_time) / 60.0)
        )

        # Exit program
        sys.exit(0)
    except KeyboardInterrupt as e:  # Ctrl-C
        raise e
    except SystemExit:  # sys.exit()
        pass
    except Exception as e:
        logging.error("ERROR, UNEXPECTED EXCEPTION")
        logging.error(str(e))
        traceback.print_exc(file=sys.stderr)
        sys.exit(-1)
